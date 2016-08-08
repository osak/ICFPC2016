require 'open3'
require 'mongo'

Mongo::Logger.logger.level = ::Logger::INFO

class Evaluator
  attr_reader :problem_id, :max_score, :owner_name
  def initialize(problem_id, max_score, owner_name)
    @problem_id = problem_id
    @max_score = max_score
    @owner_name = owner_name
  end
end

client = Mongo::Client.new(['localhost:27017'], database: 'origami')
db = client.database
solutions = db[:solutions]
problems = db[:problems]

executable_name = ARGV[0]
work_dir = ARGV[1]
execute_command = ARGV[2]
condition = ARGV[3]
timestamp = Time.now.to_i

Dir.chdir(work_dir)
problem_specs = problems.find.reduce({}){|h, spec| h[spec['problem_id']] = spec; h}
Dir.glob('/home/icfpc/shared/problems/*.in') do |input_path|
  probid = input_path.match(/\d+/)[0].to_i
  best_solution = solutions.find({problem_id: probid}).sort({resemblance: -1}).limit(1).first
  owner_name = problem_specs[probid]['owner_name']
  if best_solution && best_solution['resemblance'] == 1
    puts "Skip #{probid} as already achieved 1.0 for that problem."
    next
  end
  if condition == 'new' && best_solution
    puts "Skip #{probid} as running in new problem only mode."
    next
  end
  if condition != 'new' && condition != '' && best_solution
    e = Evaluator.new(probid, best_solution['resemblance'], owner_name)
    val = e.instance_eval(condition)
    unless val
      puts "Skip #{probid} as it does not satisfy the condition #{condition}."
      next
    end
  end

  puts "Run #{probid}"
  escaped_execute_command = execute_command.gsub("'", "\\'")
  Open3.popen3("bash -c '#{escaped_execute_command}'") do |stdin, stdout, stderr, wait_thr|
    stdin.puts(File.read(input_path))
    stdin.close
    out = stdout.read
    err = stderr.read
    doc = {
        executable_name: executable_name,
        timestamp: timestamp,
        problem_id: probid,
        problem_alias: probid <= 101 ? "Kojiki #{probid}" : probid,
        output: out,
        error: err,
    }
    solutions.insert_one(doc)
  end
end

