#! /usr/bin/env ruby
require 'open3'

file = ARGV[0]
solver = ARGV[2] || "/home/icfpc/shared/kawatea/a.out"
gideon = ARGV[3] || "/home/icfpc/shared/mkut/bin/gideon"

f = open(file)

input = f.readlines

stdin, stdout, stderr = Open3.popen3 solver

input[2..-3].each { |line|
    stdin.puts line
}

sol = stdout.read

# save
#// puts "writing to tmp file " + file + ".tmp"
#// tmp = open(file + ".tmp", "w")
#// tmp.write(gideon + " " + input[-1])
#// tmp.write(sol)
#// tmp.close

stdin, stdout, stderr = Open3.popen3(gideon + " " + input[-1])

stdin.puts sol
sol = stdout.read

puts sol
puts stderr.read
