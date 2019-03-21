(** 
In the [DefaultHandlers] module, handlers have level of their own. Their are two kinds of logger : 

 - Cli handler: outputs colored messages to stdout 
   {[ let h = Default_handlers.make (Cli Debug) ]}
 - File handler : outputs messages to a given file
   {[ let h = Default_handlers.make (File ("filename", Debug)) ]}


 *)


(** Type of a handler. *)
type tag = unit
         
type log_item = {
    level : Easy_logging_types.level;
    logger_name : string;
    msg : string;
    tags : tag list
  }
type log_formatter = log_item -> string
type t =
  {
    mutable fmt : log_formatter;
    mutable level : Easy_logging_types.level;
    output : out_channel;
  }

  
val outputs : (string, out_channel) Hashtbl.t

val apply : t -> log_item -> unit

val make_cli_handler : Easy_logging_types.level -> t

val make_file_handler :
  Easy_logging_types.level -> string -> t

val set_level : t -> Easy_logging_types.level -> unit

val set_formatter :
  t -> log_formatter -> unit

val handlers : (string, t) Hashtbl.t

val register_handler : string -> t -> unit
type desc  =
  Cli of Easy_logging_types.level
| File of string * Easy_logging_types.level
| Reg of string

val make : desc -> t

val handle_test : t -> log_item -> unit
