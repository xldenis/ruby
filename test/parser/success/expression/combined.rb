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
  end
end
