#!/usr/bin/env ruby

require 'bundler'
Bundler.require

require 'mongo'
require 'json'
require 'logger'

END {
  `/home/icfpc/shared/rasis/rasis_say -channel general 'サブミットキュー処理するやつが死にマシタ!はわ～ッww'`
}

def logger
  @logger ||= Logger.new(File.join(__dir__, '..', 'logs', 'aonoissen.log'))
end

Mongo::Logger.logger = logger
Mongo::Logger.logger.level = ::Logger::INFO

def db_connection
  @client ||= Mongo::Client.new(['localhost:27017'], database: 'origami')
  @db ||= @client.database
end

def submit_queue
  db_connection[:submit_queue]
end

def solutions
  db_connection[:solutions]
end

def submit_solution(problem_id, output)
  sleep 1.5
  post_response = nil
  Tempfile.create('aonoissen') do |f|
    f.puts output
    f.flush
    post_response = `curl --compressed -L -H Expect: -H 'X-API-Key: 58-9827ac551dda5c98cc7b3bd7e9de92ec' -F 'problem_id=#{problem_id}' -F 'solution_spec=@#{f.path}' 'http://2016sv.icfpcontest.org/api/solution/submit'`
    logger.info("API Response: #{post_response}")
  end
  JSON.parse(post_response)
end

def should_stop(arg = nil)
  if arg.nil?
    @should_stop ||= false
  else
    @should_stop = arg
  end
end

def process_pending_object(spec)
  logger.info("Processing: #{spec['_id']} (problem_id: #{spec['problem_id']}, executable_name: #{spec['executable_name']}, timestamp: #{spec['timestamp']}")
  solution = solutions.find({problem_id: spec['problem_id'], resemblance: 1}).limit(1)
  if solution.count > 0
    logger.info("Skip #{spec['_id']} as already solved with resemblance 1.0")
    submit_queue.update_one({_id: spec['_id']}, {'$set' => {status: 'rejected_redundant'}})
  else
    json = submit_solution(spec['problem_id'], spec['output'])

    doc = {
        executable_name: spec['executable_name'],
        problem_id: spec['problem_id'],
        timestamp: spec['timestamp'],
        output: spec['output'],
        resemblance: json['resemblance'].to_f
    }.freeze
    if spec['solution_id']
      solutions.update_one({_id: BSON::ObjectId(spec['solution_id'])}, {'$set' => doc}, {upsert: true})
      logger.info("Updated #{spec['solution_id']}")
    else
      solutions.insert_one(doc)
      logger.info("Inserted")
    end

    submit_queue.update_one({_id: spec['_id']}, {'$set' => {status: 'complete'}}, {upsert: true})
    logger.info("Complete #{spec['_id']}")
  end
end

def main
  logger.info("PID: #{Process.pid}")
  until should_stop
    pending_list = submit_queue.find({status: 'pending'}).sort({submission_time: 1}).limit(1)
    if pending_list.count > 0
      pending_list.each do |spec|
        process_pending_object(spec)
      end
    else
      sleep 1
    end
  end
end

Signal.trap('INT') { |sig|
  should_stop(true)
}

Process.daemon
main
