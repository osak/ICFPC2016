#! /usr/bin/env ruby
require 'open3'

files = Dir.glob("./tenku/*")
Dir.chdir(__dir__)

t = Time.now.to_i
files.each { |file|
    filename = file.split('/')[-1]
    outfile = "./out/" + filename
    outfilerev = "./out/" + filename + "r"
    next if File.zero?(file) || File.exist?(outfilerev)
    File.open(outfilerev, "w").close()
    puts "inserting " + filename

    stdin, stdout, stderr = Open3.popen3("./fastsmartlonghandedcloseinserter.rb " + file + " r")

    ore = stdout.read
    furnace = File.open(outfile, "w")

    furnace.write(ore)
    furnace.close

    puts "submitting " + filename

    num = filename.match(/\d+/)[0].to_i 

    puts "~/shared/osak/submit_solution.rb -e tenkuunoyoake -p #{num} -f #{outfile} -t #{t}"
    stdin, stdout, stderr = Open3.popen3("~/shared/osak/submit_solution.rb -e tenkuunoyoake -p #{num} -f #{outfile} -t #{t}")
    puts stdout.read
    puts stderr.read
}
