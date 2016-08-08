require 'bundler'
require 'sinatra'
require 'json'
require 'rational'
require 'open3'
require 'mongo'
require 'digest/sha1'

set :port, 4569
Mongo::Logger.logger.level = ::Logger::INFO

def db_connection
  @client ||= Mongo::Client.new(['localhost:27017'], database: 'origami')
  @db ||= @client.database
end

def histories
  db_connection[:gekirin_histories]
end

def submission_queue
  db_connection[:submit_queue]
end

def solutions
  db_connection[:solutions]
end

def communicate(path, &block)
  Open3.popen3(path, &block)
end

def parse_point(str)
  coord = str.split(',').map(&:to_r).map(&:to_f)
  {x: coord[0], y: coord[1], str: str}.freeze
end

def parse_facets!(lines)
  facets = []
  facet_count = lines.shift.to_i
  facet_count.times do |i|
    facet = []
    np = lines.shift.to_i
    np.times do
      facet << parse_point(lines.shift)
    end
    facets << facet
  end
  facets
end

def parse_skeletons!(lines)
  skeletons = []
  skeleton_count = lines.shift.to_i
  skeleton_count.times do |i|
    point_strs = lines.shift.split
    skeletons << [parse_point(point_strs[0]), parse_point(point_strs[1])]
  end
  skeletons
end

def read_problem(problem_id)
  lines = File.read(File.join(__dir__, '..', 'problems', "#{problem_id}.in")).lines
  facets = parse_facets!(lines)
  skeletons = parse_skeletons!(lines)
  [facets, skeletons]
end

get '/' do
  slim :index
end

post '/start' do
  problem_id = params[:problemId]
  facets, skeletons = read_problem(problem_id)
  {
      state: {
          history: '',
          facets: facets,
          skeletons: skeletons,
          phase: 'init'
      }
  }.to_json
end

post '/send' do
  begin
    phase = params[:phase].to_sym
    facet_ids = params[:facetIds]
    skeleton = params[:skeleton]
    fold_direction = params[:foldDirection].to_i
    history = params[:history]
    problem_id = params[:problemId]
    if fold_direction == -1
      skeleton.reverse! if skeleton
    end
    result = communicate('/home/icfpc/shared/kawatea/ui_helper') do |stdin, stdout, stderr, t|
      case phase
        when :init
          stdin.puts('init')
          stdin.puts("#{skeleton[0]} #{skeleton[1]}")
        when :fold
          stdin.puts('fold')
          stdin.puts(history)
          stdin.puts("#{skeleton[0]} #{skeleton[1]}")
          stdin.puts("#{facet_ids.length} #{facet_ids.join(' ')}")
        when :end
          stdin.puts('end')
          stdin.puts(history)
      end
      stdin.close
      stdout.read
    end
    if phase == :init || phase == :fold
      lines = result.lines
      facets = parse_facets!(lines)
      _, skeletons = read_problem(problem_id)
      {
          state: {
            phase: 'fold',
            facets: facets,
            skeletons: skeletons,
            history: result.strip,
            undoHistory: history
          }
      }.to_json
    elsif phase == :end
      {
          state: {
              solution: result,
              phase: 'end'
          }
      }.to_json
    end
  rescue => e
    STDERR.puts(e.message)
    STDERR.puts(e.backtrace)
    status 502
    {
        errorMessage: e.message
    }.to_json
  end
end

post '/submit' do
  problem_id = params[:problemId]
  solution = params[:solution]
  Tempfile.create('gekirin') do |f|
    f.puts(solution)
    f.flush
    Bundler.clean_system("/home/icfpc/shared/osak/submit_solution.rb -e 'ame-no-gekirin' -p #{problem_id} -f '#{f.path}'")
  end
  {status: 'ok'}.to_json
end

post '/restore' do
  problem_id = params[:problemId]
  history = params[:history]
  lines = history.lines
  facets = parse_facets!(lines)
  _, skeletons = read_problem(problem_id)
  {
      state: {
          phase: 'fold',
          facets: facets,
          skeletons: skeletons,
          history: history.strip,
      },
      success: true
  }.to_json
end

get '/submission_status' do
  submission_id = params[:submissionId]
  problem_id = params[:problemId]&.to_i
  executable_name = params[:executableName]

  if submission_id.nil? && (problem_id.nil? || executable_name.nil?)
    {
        status: 'error',
        errorMessage: 'Either submissionId or pair of (problemId, executableName) is required'
    }.to_json
  else
    condition = {}
    if submission_id
      condition[:submission_id] = submission_id
    end
    if problem_id && executable_name
      condition[:problem_id] = problem_id
      condition[:executable_name] = executable_name
    end
    submission = submission_queue.find(condition).sort({timestamp: -1}).limit(1).first
    if submission.nil? || submission['timestamp'] < Time.now.to_i - 600
      {
          status: 'missing'
      }.to_json
    elsif submission['status'] == 'pending'
      {
          status: 'pending'
      }.to_json
    elsif submission['status'].match(/rejected/)
      {
          status: submission['status']
      }.to_json
    elsif submission['status'] == 'complete'
      solution = solutions.find({problem_id: submission['problem_id'], executable_name: submission['executable_name'], timestamp: submission['timestamp']}).limit(1).first
      {
          status: 'complete',
          resemblance: solution['resemblance']
      }.to_json
    end
  end
end

def parse_burning_skeletons(lines)
  n = lines.shift.to_i
  n.times.map {
    arr = lines.shift.split
    p1 = parse_point(arr[0])
    p2 = parse_point(arr[1])
    visible = arr[2].to_i == 1
    {
        a: p1,
        b: p2,
        visible: visible
    }
  }
end

get '/burning' do
  slim :burning_index
end

post '/burning/start' do
  problem_id = params[:problemId]
  result = communicate('/home/icfpc/shared/yuf/max_burning/a.out') do |stdin, stdout, stderr, t|
    stdin.puts('init')
    stdin.puts(File.read(File.join(__dir__, '..', 'problems', "#{problem_id}.in")))
    stdin.close
    stdout.read
  end
  STDERR.puts(result)
  lines = result.lines
  skeletons = parse_burning_skeletons(lines)
  {
      state: {
          history: lines.join,
          skeletons: skeletons
      }
  }.to_json
end


post '/burning/send' do
  unfold_skeletons = params[:unfoldIds]
  axis_skeleton = params[:axis]
  skeletons = params[:skeletons]
  history = params[:history]
  phase = params[:phase]
  result = communicate('/home/icfpc/shared/yuf/max_burning/a.out') do |stdin, stdout, stderr, t|
    stdin.puts(phase)
    STDERR.puts(phase)
    stdin.puts(skeletons.size)
    STDERR.puts(skeletons.size)
    skeletons.values.each do |skeleton|
      stdin.puts("#{skeleton['a']['str']} #{skeleton['b']['str']} #{skeleton['visible'] == 'true' ? 1 : 0}")
      STDERR.puts("#{skeleton['a']['str']} #{skeleton['b']['str']} #{skeleton['visible'] == 'true' ? 1 : 0}")
    end
    if phase == 'unfold'
      stdin.puts(unfold_skeletons.size + 1)
      STDERR.puts(unfold_skeletons.size + 1)
      stdin.puts("#{skeletons[axis_skeleton]['a']['str']} #{skeletons[axis_skeleton]['b']['str']}")
      STDERR.puts("#{skeletons[axis_skeleton]['a']['str']} #{skeletons[axis_skeleton]['b']['str']}")
      unfold_skeletons.each do |i|
        stdin.puts("#{skeletons[i]['a']['str']} #{skeletons[i]['b']['str']}")
        STDERR.puts("#{skeletons[i]['a']['str']} #{skeletons[i]['b']['str']}")
      end
    end
    stdin.puts(history)
    STDERR.puts(history)
    stdin.close
    stdout.read
  end
  if phase == 'unfold'
    lines = result.lines
    skeletons = parse_burning_skeletons(lines)
    {
        state: {
            history: lines.join,
            skeletons: skeletons
        }
    }.to_json
  elsif phase == 'end'
    {
        state: {
            solution: result,
            phase: 'end'
        }
    }.to_json
  end
end
