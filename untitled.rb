require 'redis'

def method(b,g = 2, *k, bb:, e: 2, c:,  **d)
end


def method2(a = 2, g, bb:, e: 2, c:, **d)
end

def method3(b,g = 2, *k, f, bb:, e: 2, c:,  **d)
end

def method5(g=2, *k, bb:, **d) ; end


def method3(b, *k, f, bb:, e: 2, c:,  **d)
end

def method4(b = 1, g = 2, *k, f, bb:, e: 2, c:, **d)
end

method(2,3, bb: 5, c: 1, d: 4)

def self.each(path)
    return unless File.file?(path)

    open(path: path) do |zip|
      zip.each do |entry|
        yield entry
      end
    end
  end
