require 'cairo'

require 'rational'

module Compiler
  Point = Struct.new(:x, :y)

  class << self
    def compile(input_file)
      surface = Cairo::ImageSurface.new(Cairo::FORMAT_ARGB32, 400, 400)
      context = Cairo::Context.new(surface)
      context.set_source_rgb(1, 1, 1)
      context.rectangle(0, 0, 400, 400)
      context.fill

      polygons = []
      n = input_file.gets.to_i
      n.times do
        nv = input_file.gets.to_i
        poly = nv.times.map{Point.new(*input_file.gets.split(',').map(&:to_r))}
        polygons << poly
      end

      lines = []
      minx, miny = nil, nil
      nsk = input_file.gets.to_i
      nsk.times do
        coords = input_file.gets.chomp.split(' ')
        lines << coords.map!{|spec| Point.new(*spec.split(',').map(&:to_r))}
        minx = lines.last[0].x if minx.nil? || minx > lines.last[0].x
        minx = lines.last[1].x if minx.nil? || minx > lines.last[1].x
        miny = lines.last[0].y if miny.nil? || miny > lines.last[0].y
        miny = lines.last[1].y if miny.nil? || miny > lines.last[1].y
      end

      # Generate commands
      commands = []
      #commands << "color('gray');"
      context.set_source_rgb(0.5, 0.5, 0.5)
      lines.each {|l|
        #commands << "linedash(#{linepoints(l[0], l[1], minx, miny)});"
        p1 = transform(l[0], minx, miny)
        context.move_to(p1.x, p1.y)
        p2 = transform(l[1], minx, miny)
        context.line_to(p2.x, p2.y)
        context.set_line_width(1)
        context.stroke
      }
      polygons.each do |poly|
        if ccw?(poly[0], poly[1], poly[2])
          #commands << 'color(\'black\');'
          context.set_source_rgb(0, 0, 0)
        else
          #commands << 'color(\'red\');'
          context.set_source_rgb(1, 0, 0)
        end
        poly.each_with_index do |p, i|
          np = i+1 < poly.size ? poly[i+1] : poly[0]
          #commands << "line(#{linepoints(p, np, minx, miny)});"
          tp = transform(p, minx, miny)
          tnp = transform(np, minx, miny)
          context.move_to(tp.x, tp.y)
          context.line_to(tnp.x, tnp.y)
          context.set_line_width(2)
          context.stroke
          #commands << "color('black');"
          #commands << "text('(#{p.x}, #{p.y})', #{tp.x.to_f}, #{tp.y.to_f + rand(20) - 10});"
        end
        ymin = -Float::INFINITY
      end
      #commands.join("\n")
      basename = File.basename(input_file)
      id = basename.match(/\d+/)[0]
      out = File.join(__dir__, 'images', "#{id}.png")
      surface.write_to_png(out)
    end

    private
    def ccw?(p1, p2, p3)
      v1 = Point.new(p2.x - p1.x, p2.y - p1.y)
      v2 = Point.new(p3.x - p1.x, p3.y - p1.y)
      v1.x * v2.y - v1.y * v2.x > 0
    end

    def linepoints(p1, p2, minx, miny)
      tp1 = transform(p1, minx, miny)
      tp2 = transform(p2, minx, miny)
      "#{tp1.x.to_f}, #{tp1.y.to_f}, #{tp2.x.to_f}, #{tp2.y.to_f}"
    end

    def transform(p, minx, miny)
      Point.new((p.x - minx) * 300 + 10, 400 - ((p.y - miny) * 300 + 10))
    end
  end
end

if __FILE__ == $0
  Dir.glob(File.join(__dir__, '..', 'problems', '*.in')) do |file|
    puts file
    File.open(file) do |f|
      Compiler.compile(f)
    end
  end
end
