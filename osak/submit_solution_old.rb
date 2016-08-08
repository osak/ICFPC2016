#!/usr/bin/env ruby

pwd = Dir.pwd
Dir.chdir(__dir__)
require 'bundler'
Bundler.require
Dir.chdir(pwd)

require 'mongo'
require 'optparse'

Mongo::Logger.logger       = ::Logger.new(File.join(__dir__, 'log', 'mongo.log'))
Mongo::Logger.logger.level = ::Logger::INFO

def submit_queue
  @client ||= Mongo::Client.new(['localhost:27017'], database: 'origami')
  @db ||= @client.database
  @db[:submit_queue]
end

def valid_solution?(str)
  lines = str.lines.map(&:strip)
  return false unless lines[0].match(/^\d+$/)
  num_points = lines[0].to_i
  num_points.times do |i|
    return false unless lines[1+i].match(%r{^-?\d+(?:/\d+)?,-?\d+(?:/\d+)?$})
  end
  num_facets = lines[1+num_points].to_i
  num_facets.times do |i|
    arr = lines[1+num_points+1+i].split
    len = arr[0].to_i
    return false if arr.size != len + 1
    return false unless arr.all?{|a| a.match(/^\d+$/)}
  end
  num_points.times do |i|
    return false unless lines[1+num_points+1+num_facets+i].match(%r{^-?\d+(?:/\d+)?,-?\d+(?:/\d+)?$})
  end
  true
end

Configuration = Struct.new(:executable_name, :problem_id, :timestamp, :solution_file, :solution_id)
def main
  conf = Configuration.new
  opt = OptionParser.new
  opt.on('-e NAME', '--executable-name', 'サブミッションの名前') {|v| conf.executable_name = v}
  opt.on('-p PROB_ID', '--problem-id', '問題ID') {|v| conf.problem_id = v.to_i}
  opt.on('-f FILE_NAME', '--solution-file', '解答ファイル') {|v| conf.solution_file = v.strip}
  opt.on('-t TIMESTAMP', '--timestamp', 'タイムスタンプ（省略可）') {|v| conf.timestamp = Time.at(v.to_i).to_i}
  opt.on('-s SOLUTION_ID', '--solution-id', '解答ID (省略可)') {|v| conf.solution_id = v}
  opt.parse!(ARGV)

  if conf.executable_name.nil? || conf.problem_id.nil? || conf.solution_file.nil?
    STDERR.puts('ERROR: -e and -p, and -f are required parameters.')
    STDERR.puts(opt.help)
    exit 1
  end
  if conf.timestamp.nil?
    conf.timestamp = Time.now.to_i
  end

  output = File.read(conf.solution_file)
  unless valid_solution?(output)
    STDERR.puts("#{conf.solution_file} は解答ファイルじゃなさそうに見えます。")
    exit 2
  end
  doc = {
      executable_name: conf.executable_name,
      problem_id: conf.problem_id,
      timestamp: conf.timestamp,
      output: output,
      submission_time: Time.now.to_i,
      solution_id: conf.solution_id,
      status: 'pending'
  }.freeze
  res = submit_queue.insert_one(doc)
  puts "Submitted: #{res.inserted_ids.first}"
end

main
