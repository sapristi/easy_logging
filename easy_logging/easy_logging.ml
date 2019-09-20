open Make_logging


module Level = struct
  type level = Logging_types.level =
    | Debug
    | Trace
    | Info
    | Warning
    | Error
    | Flash
    | NoLevel

  end

module Handlers = Handlers

module Formatters = Formatters

module Logging = MakeLogging(Handlers)



module Internal =
  struct
    module Logging_types = Logging_types
    module MakeLogging = MakeLogging
    module Logging_infra = Logging_infra
  end
