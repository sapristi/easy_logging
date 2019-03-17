


type log_formatter = Easy_logging_types.log_formatter
type log_level = Easy_logging_types.level
val pp_log_level :
  Batteries.Format.formatter -> log_level -> Ppx_deriving_runtime.unit
val show_log_level : log_level -> Ppx_deriving_runtime.string
module Make :
functor (H : Easy_logging__Easy_logging_types.HandlersT) ->
sig
  
  class logger :
          string ->
          log_level option ->
          H.desc list ->
          object
            val mutable handlers : H.t list
            val mutable levelo : log_level option
            val name : string
            method add_handler : H.t -> unit
            method debug : string -> unit
            method error : string -> unit
            method flash : string -> unit
            method info : string -> unit
            method ldebug : string lazy_t -> unit
            method lerror : string lazy_t -> unit
            method lflash : string lazy_t -> unit
            method linfo : string lazy_t -> unit
(*            method log_msg : log_level -> string -> unit
            method log_msg_lazy : log_level -> string lazy_t -> unit *)
            method lwarning : string lazy_t -> unit
            method set_level : log_level option -> unit
            method warning : string -> unit
          end
  val _loggers : (string, logger) Batteries.Hashtbl.t
  val set_level : string -> log_level option -> unit
  val get_logger : string -> logger
  val make_logger : string -> log_level option -> H.desc list -> logger
  val dummy : logger
  module Handlers :
  sig
    type t =
           H.t = {
        mutable fmt : log_formatter;
        mutable level : log_level;
        output : unit Batteries.IO.output;
      }
    val handle :
      t -> Easy_logging__Easy_logging_types.log_item -> unit
            type desc = H.desc
            val make : desc -> t
  end
end

module Handlers = Default_handlers
module Logging :
sig
  (** {3 Attributes} *)
  class logger :
          string ->
          log_level option ->
          Handlers.desc list ->
          object

            (** Name displayed in log messages *)
            val name : string

            (** Value used to filter log messages. If levelo is [None], no message is outputed.*)
            val mutable levelo : log_level option
            (** {[type log_level = | Debug | Info | Warning | Error | Flash ]} *)

            
            val mutable handlers : Handlers.t list
            (** Registered handlers for this logger 
              
              
{3 Classic logging Methods}
Each of these methods takes a [string] as an input. If the log level of the instance is low enough, a log item will be created theb passed to the handlers.

Example : 
{[logger#warning "Something wrong happened"]}
 *)

            method debug : string -> unit
            method info : string -> unit
            method warning : string -> unit
            method error : string -> unit
            method flash : string -> unit
            (** {3 Lazy logging methods} 
Each of these methods takes a [string lazy_t] as an input. If the log level of the instance is low enough, the lazy value will forced into a [string], a log item will be created then passed to the handlers.

Example:
{[logger#ldebug (lazy (heavy_calculation () ))]}
*)
                 
            method ldebug : string lazy_t -> unit
            method linfo : string lazy_t -> unit
            method lwarning : string lazy_t -> unit
            method lerror : string lazy_t -> unit
            method lflash : string lazy_t -> unit
            (** {3 Other methods} *)

            (** Adds a handler to the logger instance*)
            method add_handler : Handlers.t -> unit
            
            (** Sets the log level of the logger instance *)                 
            method set_level : log_level option -> unit

                 
          (*            method log_msg : log_level -> string -> unit
            method log_msg_lazy : log_level -> string lazy_t -> unit*)
          end
  val _loggers : (string, logger) Batteries.Hashtbl.t
  val set_level : string -> log_level option -> unit
  val get_logger : string -> logger
  val make_logger :
    string -> log_level option -> Handlers.desc list -> logger
  val dummy : logger
    (*
  module Handlers :
  sig
    type t =
           Handlers.t = {
        mutable fmt : log_formatter;
        mutable level : log_level;
        output : unit Batteries.IO.output;
      }
     
    val handle : t -> Easy_logging__Easy_logging_types.log_item -> unit
    type desc = Handlers.desc
    val make : desc -> t
  end
     *)
end
