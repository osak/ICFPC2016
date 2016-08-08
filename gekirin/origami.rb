require 'rational'

class Origami
  Point = Struct.new(:x, :y)
  Segment = Struct.new(:a, :b)
  RefSegment = Struct.new(:ai, :bi)

  def initialize(filename)
    silhouette_tmp = []
    skeleton_tmp = []
    File.open(filename) do |f|
      n = f.gets.to_i
      n.times do
        silhouette_tmp << Point.new(*gets.split(',').map(&:to_r))
      end

      m = f.gets.to_i
      m.times do
        skeleton_tmp << Segment.new(*gets.split.map{|s| Point.new(*s.split(',').map(&:to_r))})
      end
    end

    @points = silhouette_tmp + skeleton_tmp.map{|s| [s.a, s.b]}.flatten
    @points.uniq!
    @silhouette = silhouette_tmp.map{|p| @points.index(p)}
    @skeletons = skeleton_tmp.map{|s| RefSegment.new(@points.index(s.a), @points.index(s.b))}
    @facets = []
  end

  def unfold(facet_idx, p1_idx, p2_idx)

  end
end