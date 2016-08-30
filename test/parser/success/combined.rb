module Something
  class Lol ; end
  class New < Lol
    def initialize(arg)
    end

    alias test initialize
  end
end
