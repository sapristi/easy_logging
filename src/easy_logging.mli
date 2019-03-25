
(** Type for log levels. *)
type log_level = Easy_logging__.Easy_logging_types.level
[@@deriving show { with_path = false }]



               
(** Signature of Handlers modules. *)
module type HandlersT = Easy_logging__.Easy_logging_types.HandlersT

(** Makes a Logging module from a Handlers module. *)
module MakeLogging :
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
            method debug : ?tags:H.tag list -> string -> unit
            method error : ?tags:H.tag list -> string -> unit
            method flash : ?tags:H.tag list -> string -> unit
            method info : ?tags:H.tag list -> string -> unit
            method ldebug : ?tags:H.tag list -> string lazy_t -> unit
            method lerror : ?tags:H.tag list -> string lazy_t -> unit
            method lflash : ?tags:H.tag list -> string lazy_t -> unit
            method linfo : ?tags:H.tag list -> string lazy_t -> unit
                 (*
            method log_msg : log_level -> string -> unit
            method log_msg_lazy : log_level -> string lazy_t -> unit *)
            method lwarning : ?tags:H.tag list -> string lazy_t -> unit
            method set_level : log_level option -> unit
            method warning : ?tags:H.tag list -> string -> unit
                 
            method flog : log_level -> (('a, unit, string, unit) format4) -> 'a
          end
  val _loggers : (string, logger) Hashtbl.t
  val set_level : string -> log_level option -> unit
  val get_logger : string -> logger
  val make_logger : string -> log_level option -> H.desc list -> logger
  val dummy : logger

end


(** Default implementation of a Handlers module. *)
module Default_handlers = Default_handlers

(** Default implementation of a Logging module. *)
module Logging :
sig
  (** {3 Attributes} *)
  class logger :
          string ->
          log_level option ->
          Default_handlers.desc list ->
          object

            (** Name of the logger, displayed in log messages *)
            val name : string

            (** Value used to filter log messages. If levelo is [None], no message is outputed.*)
            val mutable levelo : log_level option
            (** {[type log_level = | Debug | Info | Warning | Error | Flash ]} *)

            
            val mutable handlers : Default_handlers.t list
            (** Registered handlers for this logger 
              
              
{3 Classic logging Methods}
Each of these methods takes an optional [tag list] and a [string] as an input. If the log level of the instance is low enough, a log item will be created theb passed to the handlers.

Example : 
{[logger#warning "Something wrong happened"]}
 *)

            method debug : ?tags:Default_handlers.tag list -> string -> unit
            method info : ?tags:Default_handlers.tag list -> string -> unit
            method warning : ?tags:Default_handlers.tag list -> string -> unit
            method error : ?tags:Default_handlers.tag list -> string -> unit
            method flash : ?tags:Default_handlers.tag list -> string -> unit
            (** {3 Lazy logging methods} 
Each of these methods takes a [string lazy_t] as an input (as well as the optional [tag list]. If the log level of the instance is low enough, the lazy value will forced into a [string], a log item will be created then passed to the handlers.

Example:
{[logger#ldebug (lazy (heavy_calculation () ))]}
*)
                 
            method ldebug : ?tags:Default_handlers.tag list -> string lazy_t -> unit
            method linfo : ?tags:Default_handlers.tag list -> string lazy_t -> unit
            method lwarning : ?tags:Default_handlers.tag list -> string lazy_t -> unit
            method lerror : ?tags:Default_handlers.tag list -> string lazy_t -> unit
            method lflash : ?tags:Default_handlers.tag list -> string lazy_t -> unit
            (** {3 Other methods} *)

            method flog : log_level -> (('a, unit, string, unit) format4) -> 'a
              
            (** Adds a handler to the logger instance. *)
            method add_handler : Default_handlers.t -> unit
            
            (** Sets the log level of the logger instance. *)                 
            method set_level : log_level option -> unit

                                                     (*
            method log_msg : log_level -> string -> unit
            method log_msg_lazy : log_level -> string lazy_t -> unit *)
          end

  (** [make_logger name level handlers_descs] 
      creates a new logger instance from the given arguments,
      then register it internally, and returns it.  *)
  val make_logger :
    string -> log_level option -> Default_handlers.desc list -> logger

  (** Internally registered loggers. *)
  val _loggers : (string, logger) Hashtbl.t


  (** [set_level prefix level] sets the level of all 
      registered loggers whose name begins by [prefix]
      to [level]. *)
  val set_level : string -> log_level option -> unit


    
  (** Returns a registered logger. *)
  val get_logger : string -> logger


  (** dummy logger : does nothing. *)
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
