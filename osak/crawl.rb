#!/usr/bin/env ruby

Dir.chdir(__dir__)
require 'bundler'
Bundler.require

require 'json'
require 'mongo'
require 'set'
require 'logger'

API_URL = 'http://2016sv.icfpcontest.org/api'

Mongo::Logger.logger.level = ::Logger::INFO

def problems_collection
  @client ||= Mongo::Client.new(['localhost:27017'], database: 'origami')
  @db ||= @client.database
  @db[:problems]
end

def logger
  @logger ||= Logger.new(File.join(__dir__, '..', 'logs', 'crawler.log'))
end

def query(path, format: :json)
  sleep 1
  url = "#{API_URL}/#{path}"
  puts "Downloading #{url}"
  raw = `curl -s --compressed -L -H Expect: -H 'X-API-Key: 58-9827ac551dda5c98cc7b3bd7e9de92ec' #{API_URL}/#{path}`
  case format
  when :json
    JSON.parse(raw)
  when :raw
    raw
  end
end

def latest_snapshot
  snapshot = query('snapshot/list')
  snapshot['snapshots'].last
end

def blob_lookup(hash, format: :json)
  query("blob/#{hash}", format: format)
end

logger.info('Crawler Run')
problem_ids = Set[*problems_collection.distinct('problem_id')]
latest_hash = latest_snapshot['snapshot_hash']
snapshot = blob_lookup(latest_hash)
users = snapshot['users']
user_hash = users.reduce({}) {|h, u| h[u['username'].to_i] = u['display_name']; h}
problems = snapshot['problems']
problems.each do |problem_spec|
  begin
    hash = problem_spec['problem_spec_hash']
    id = problem_spec['problem_id'].to_i
    logger.info("Processing: #{id}")
    if File.exists?("../problems/#{id}.in")
      logger.info("Skip downloading as already downloaded")
    else
      logger.info("Download problem #{id}")
      File.open("../problems/#{id}.in", 'w') do |f|
        f.puts(blob_lookup(hash, format: :raw))
      end
    end
    resemblances = problem_spec['ranking'].map{|r| r['resemblance']}
    max_resenblance = resemblances.max
    doc = {
        problem_id: id,
        best_resemblance: max_resenblance,
        resemblances: resemblances,
        solution_size: problem_spec['solution_size'].to_i,
        owner_id: problem_spec['owner'].to_i,
        owner_name: user_hash[problem_spec['owner'].to_i]
    }.freeze
    problems_collection.update_one({problem_id: id}, {'$set' => doc}, {upsert: true})
  rescue => e
    logger.error(e.message)
    logger.error(e.backtrace)
  end
end
