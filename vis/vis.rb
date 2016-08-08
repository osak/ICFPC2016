require 'sinatra'
require 'tempfile'
require_relative 'compile'

def compile(input, force: false)
  Dir.chdir(__dir__)
  f = Tempfile.create('vis')
  f.puts(input)
  f.flush
  f.close
  sha1 = `sha1sum #{f.path}`.split[0]
  outfile = "js/#{sha1}.js"
  if force || !File.exists?(outfile)
    File.open(outfile, 'w') do |of|
      of.puts(Compiler.compile(File.open(f.path)))
    end
  end
  sha1
end

def compile_solution(input, force: false)
  Dir.chdir(__dir__)
  sha1 = nil
  Tempfile.create('vis') do |f|
    f.puts(input)
    f.flush
    sha1 = `sha1sum #{f.path}`.split[0]
    outfile = "js/#{sha1}.js"
    animated_outfile = "js/#{sha1}-animated.js"
    if force || !File.exists?(outfile)
      `../yuf/bloomin_feeling/a.out < #{f.path} > #{outfile}`
      `../yuf/bloomin_feeling/b.out < #{f.path} > #{animated_outfile}`
    end
  end
  sha1
end

get '/' do
  File.read(File.expand_path(File.join(__dir__, 'index.html')))
end

get '/show/:hash' do
  @hash = params[:hash]
  @animate = params[:animate].to_i == 1
  @title = @hash
  slim :input_view
end

get '/show/problem/:id' do
  @hash = compile(File.read(File.join(__dir__, '..', 'problems', params['id'] + '.in')), force: params['force'].to_i != 0)
  @probid = params[:id].to_i
  @animate = params[:animate].to_i == 1
  @title = "Problem #{@probid}"
  slim :input_view
end

post '/post' do
  sha1 = compile(params['input'].gsub("\r\n", "\n"))
  redirect to("/vis/show/#{sha1}")
end

post '/post_solution' do
  sha1 = compile_solution(params['input'].gsub("\r\n", "\n"), force: params['force'].to_i == 1)
  redirect to("/vis/show/#{sha1}")
end