class A
  class B ; end
end

class Test < A
end

class Test::B < A::B
end

class Something < Class.new
end

class C < (1 + 1).class
end

