class AdminAreaController < ApplicationController
  self.responder = Responder::Api

  class ClassOne < PublicMessageError; end
  class ClassTwo < PublicMessageError; end

  extend ModuleOne

  include ModuleTwo
  include ModuleThree
  include Four

  helper Helper1, Helper2,
    Helper3,
    Helper4,
    Helper5
end
