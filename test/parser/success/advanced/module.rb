class A
  def module ; end
  def class  ; end
  def if ; end
  def def ; end

  self.something = a

  CONSTANT = 1

  'string'.freeze

  included do
    class_attribute
    something :some_name, something: false, true: 1
  end

  class << self
    1
  end
end

a = A.new

a.class
a.module
a.if


# class << 2

# end
