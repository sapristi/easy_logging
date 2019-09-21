module E = Easy_logging
open Easy_logging_yojson_aux

module Handlers = Handlers
module Formatters = E.Formatters


module Logging = MakeLogging(Handlers)
module Logging_types = Logging_types

module Internal =
  struct
    module Logging_types = Logging_types
    module MakeLogging = MakeLogging
    module Logging_infra = E.Internal.Logging_infra
    module Colorize = E.Internal.Colorize
  end
