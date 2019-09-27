module Level :
sig
  type t = Logging_types.level =
    | Debug
    | Trace
    | Info
    | Warning
    | Error
    | Flash
    | NoLevel
  end

open Logging_types


(** Default implementation of a Handlers module. *)
module Handlers = Handlers

(** Log formatters: functions of type [log_item -> string]. *)
module Formatters = Formatters

(** Default implementation of a Logging module. *)
module Logging :
sig
  val debug : bool ref
  class logger :
    ?parent:logger option ->
    string ->
    object

      (** {3 Attributes} *)

      (** Name of the logger:
          - can be displayed in log messages.
          - defines the logger place in the logging tree. *)
      val name : string

      (** Value used to filter log messages.*)
      val mutable level : level option

      (** Registered handlers for this logger. *)
      val mutable handlers : Handlers.t list

      (** The optional parent of this logger. *)
      val parent : logger option

      (** Whether messages passed to this logger are propagated to its ancestors' handlers.*)
      val propagate : bool

      (** The list of functions used for dynamic tagging of messages *)
      val mutable tag_generators : (unit -> string) list


      (** {3 Classic logging Methods}
          Each of these methods takes an optional [string list] of tags, then a set of parameters the way a printf function does. If the log level of the instance is low enough, a log item will be created theb passed to the handlers.

          Example :
          {[logger#warning "Unexpected value: %s" (to_string my_value)]}
      *)

      method flash : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
      method error : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
      method warning : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
      method info : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
      method trace : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a
      method debug : 'a. ?tags:string list -> ('a, unit, string, unit) format4 -> 'a


      (** {3 Lazy logging methods}
          Each of these methods takes a [string lazy_t] as an input (as well as the optional tags. If the log level of the instance is low enough, the lazy value will forced into a [string], a log item will be created then passed to the handlers.

          Example:
          {[logger#ldebug (lazy (heavy_calculation () ))]}
      *)

      method ldebug : ?tags:string list -> string lazy_t -> unit
      method ltrace : ?tags:string list -> string lazy_t -> unit
      method linfo : ?tags:string list -> string lazy_t -> unit
      method lwarning : ?tags:string list -> string lazy_t -> unit
      method lerror : ?tags:string list -> string lazy_t -> unit
      method lflash : ?tags:string list -> string lazy_t -> unit

      (** {3 String logging methods}
          Each of these methods takes a [string] as an input (as well as the optional tags).

          Example:
          {[logger#sdebug string_variable]}
      *)


      method sdebug : ?tags:string list -> string -> unit
      method strace : ?tags:string list -> string -> unit
      method sinfo : ?tags:string list -> string -> unit
      method swarning : ?tags:string list -> string -> unit
      method serror : ?tags:string list -> string -> unit
      method sflash : ?tags:string list -> string -> unit


      (** {3 Other methods} *)


      (** Sets the log level of the logger instance. *)
      method set_level : level  -> unit

      (** Adds a handler to the logger instance. *)
      method add_handler : Handlers.t -> unit

      method get_handlers : Handlers.t list
      method set_handlers : Handlers.t list -> unit

      (** Will add a tag to each log message, resulting from the call of the supplied fonction (called each time a message is logged)*)
      method add_tag_generator: (unit -> string) -> unit

      (** Sets the propagate attribute, which decides whether messages passed to this logger are propagated to its ancestors' handlers. *)
      method set_propagate : bool -> unit

      (** {4 Internal methods} *)

      (** Returns the list of handlers of the logger, recursing with parents handlers
          if propagate is true*)
      method get_handlers_propagate : Handlers.t list

      (** Returns this logger level if it is not [None], else searches amongst ancestors for the first defined level; returns [NoLevel] if no level can be found. *)
      method effective_level : level
    end

  (** [make_logger name level handlers_descs]
      creates a new logger instance from the given arguments,
      then register it internally, and returns it.  *)
  val make_logger :
    ?propagate:bool -> string -> level  -> Handlers.desc list -> logger

  (** Returns a registered logger by name. *)
  val get_logger : string -> logger

end


module MakeLogging = Make_logging.MakeLogging

module Internal:
  sig
    module Logging_types = Logging_types
    (** Types used in easy_logging. *)

    module MakeLogging = MakeLogging
    (** Functor to generate a Logging module over a Handlers module.*)

    module Logging_infra = Logging_infra
  end
