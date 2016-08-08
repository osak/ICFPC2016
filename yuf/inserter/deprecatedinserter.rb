#! /usr/bin/env ruby
require 'open3'

files = Dir.glob("./out/*")

t = Time.now.to_i
files.each { |file|
    puts "submitting " + file

    num = file.match(/\d+/)[0].to_i

    puts "~/shared/osak/submit_solution.rb -e tenkuunoyoake -p #{num} -f #{file} -t #{t}"
    stdin, stdout, stderr = Open3.popen3("~/shared/osak/submit_solution.rb -e tenkuunoyoake -p #{num} -f #{file} -t #{t}")
    puts stdout.read
    puts stderr.read
}
