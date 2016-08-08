require 'bundler'
require 'sinatra'
require 'mongo'
require 'tempfile'
require 'json'
require 'uri'

set :port, 4568
Mongo::Logger.logger.level = ::Logger::INFO

Executable = Struct.new(:name, :timestamps)

def submit_solution(executable_name, problem_id, timestamp, solution_id, output)
  Tempfile.create('gyakurasetsu') do |f|
    f.puts(output)
    f.flush
    Bundler.clean_system("/home/icfpc/shared/osak/submit_solution.rb -e '#{executable_name}' -p #{problem_id} -f '#{f.path}' -t #{timestamp} -s '#{solution_id}'")
  end
end

def db_connection
  @client ||= Mongo::Client.new(['localhost:27017'], database: 'origami')
  @db ||= @client.database
end

def solutions
  db_connection[:solutions]
end

def problems
  db_connection[:problems]
end

get '/executables/list' do
  @title = "実行一覧"
  @executables = []
  solutions.distinct('executable_name').each do |name|
    @executables << Executable.new(name, solutions.distinct('timestamp', {executable_name: name}))
  end
  slim :executable_list
end

get '/executables/show/:name/:timestamp' do
  @results = solutions.find({executable_name: params[:name], timestamp: params[:timestamp].to_i})
  @problems = @results.reduce({}){|h, r| h[r['problem_id']] = File.read(File.join(__dir__, '..', 'problems', "#{r['problem_id']}.in")); h}
  @name = params[:name]
  @timestamp = params[:timestamp]
  slim :executable_result
end

get '/executables/submit' do
  slim :submit
end

def escape_command(str)
  URI.escape(str).gsub('+', '%2B').gsub('&', '%26')
end

post '/executables/submit' do
  @name = params[:name]
  @workdir = params[:workdir]
  @compile_command = params[:compile_command]
  @run_command = params[:run_command]
  @condition = params[:condition]

  url = "http://tsubasa.osak.jp/jenkins/job/KaDingel/buildWithParameters?token=gyakurasetsu&NAME=#{URI.escape(@name)}&WORK_DIRECTORY=#{URI.escape(@workdir)}&COMPILE_COMMAND=#{escape_command(@compile_command)}&RUN_COMMAND=#{escape_command(@run_command)}&CONDITION=#{URI.escape(@condition)}"
  f = Tempfile.create('gyakurasetu')
  f.close
  output = `curl -D #{f.path} -X POST '#{url}'`
  m = File.read(f.path).match(/^Location:\s*.*\/item\/(\d+)\/.*$/)
  if m
    @jenkins_url = "http://tsubasa.osak.jp/jenkins/job/KaDingel/#{m[1]}"
  end
  slim :submit_complete
end

post '/submit' do
  problem_id = params[:problem_id]
  timestamp = params[:timestamp]
  executable_name = params[:executable_name]
  if problem_id.nil? || timestamp.nil? || executable_name.nil?
    raise "Missing param"
  end

  solution = solutions.find({problem_id: problem_id.to_i, timestamp: timestamp.to_i, executable_name: executable_name}).first
  result = nil
  begin
    submit_solution(executable_name, problem_id, timestamp, solution['_id'], solution['output'])
    result = {status: 'ok'}
  rescue => e
    STDERR.puts(e)
    STDERR.puts(e.backtrace)
    result = {error: e.message}
  end
  result.to_json
end

post '/executables/bulk_submit/:name/:timestamp' do
  limit = params[:threshold].nil? ? 0 : params[:threshold].to_i
  @results = solutions.find({executable_name: params[:name], timestamp: params[:timestamp].to_i, problem_id: {'$gt' => limit}})
  solutions = []
  @results.each do |result|
    solutions << submit_solution(params[:name], result['problem_id'], params[:timestamp].to_i, result['_id'], result['output'])
  end
  solutions.to_json
end

def calculate_best_results
  bests = solutions.aggregate([{'$group' => {_id: '$problem_id', resemblance: {'$max' => '$resemblance'}}}])
  best_results = {}
  bests.each do |best|
    best_results[best['_id']] = solutions.find({problem_id: best['_id'], resemblance: best['resemblance']}).first
  end
  best_results
end

def calculate_score_spec(problem_spec, best_results)
  top_count = problem_spec['resemblances'].count{|r| r == 1}
  base_score = problem_spec['solution_size'] / (top_count + 1.0)
  den = problem_spec['resemblances'].select{|r| r != 1}.inject(1.0, &:+)
  best_result = best_results[problem_spec['problem_id']]
  resemblance = (best_result && best_result['resemblance']) || 0
  my_score = (resemblance == 1) ? base_score : (base_score * resemblance / den)
  score_if_i_was_top = base_score * (top_count + 1) / (top_count + 2)
  [my_score, score_if_i_was_top]
end

get '/' do
  @title = "現在の状況"
  @best_results = calculate_best_results
  problem_specs = problems.find({})
  @global_bests = {}
  @my_score = {}
  @score_if_i_were_top = {}
  problem_specs.each do |spec|
    problem_id = spec['problem_id']
    @global_bests[problem_id] = spec['resemblances'].max
    top_count = spec['resemblances'].count{|r| r == 1}
    base_score = spec['solution_size'] / (top_count.to_f + 1)
    den = spec['resemblances'].select{|r| r != 1}.inject(&:+) || 1.to_f
    best_result = @best_results[problem_id]
    resemblance = (best_result && best_result['resemblance']) || 0
    @my_score[problem_id] = (resemblance == 1) ? base_score : (base_score * resemblance / den)
    @score_if_i_were_top[problem_id] = base_score * (top_count + 1) / (top_count + 2)
  end
  slim :index
end

Cluster = Struct.new(:repr, :size, :my_score, :expected_score)
get '/cluster' do
  @best_results = calculate_best_results
  @clusters = []
  json = JSON.parse(File.read(File.join(__dir__, '..', 'grace', 'clusters.json')))
  json.each_value do |cluster|
    if solutions.find({problem_id: {'$in' => cluster}, resemblance: 1}).limit(1).count == 0
      problem_specs = problems.find({problem_id: {'$in' => cluster}})
      my_score, expected_score = 0, 0
      problem_specs.each do |problem_spec|
        my, expected = calculate_score_spec(problem_spec, @best_results)
        my_score += my
        expected_score += expected
      end
      @clusters << Cluster.new(cluster.first, cluster.size, my_score, expected_score)
    end
  end
  slim :cluster
end

