

(** Possible level of a log item. *)
type level =
  | Debug
  | Info
  | Warning
  | Error
  | Flash
[@@deriving show { with_path = false }]

type log_item = {
    level : level;
    logger_name : string;
    msg : string;
  }


type log_formatter = log_item -> string


module type HandlersT =
  sig
    
    (** Type of a handler *) 
    type t 

    (** Overrides the formatter of the given handler. *)
    val set_formatter : t -> log_formatter -> unit

    (** Overrides the level of the given handler *)
    val set_level : t -> level -> unit

    (** Applies the handler to a [log_item] *)
    val apply : t -> log_item -> unit

    (** Type used to instantiate a handler*)
    type desc

    (** Instantiates a handler *)
    val make : desc -> t
  end                   
