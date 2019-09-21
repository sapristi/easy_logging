open Make_logging

module Handlers = Handlers

module Formatters = Formatters

module Logging = MakeLogging(Handlers)



module Internal =
  struct
    module Logging_types = Logging_types
    module MakeLogging = MakeLogging
    module Logging_infra = Logging_infra
    module Colorize = Colorize
  end
