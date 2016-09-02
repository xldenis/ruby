require 'some_path'

module Something
  class Lol ; end
  class New < Lol
    def initialize(arg)
    end

    alias test initialize

    def something_check(action: :log) ; end

    def method_name(test: def a ; end)
      raise 'bullshit'
    end

    def returning_method
      return 3, 5
    end

    def five_string
      5.to
    end

    def hello(and2: 1 + 1)
      1 + 1
      1 ~ 1
      1 * 2 / 3 % 6 + 4 << 20
    end

    def something
      1 if 5
      5 unless 0
    end
  end
end
