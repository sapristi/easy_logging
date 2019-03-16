open Batteries
open File

   
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
    
    type t =
      {mutable fmt : log_formatter;
       mutable level : level;
       output : unit IO.output}

    val handle : t -> log_item -> unit
    type desc
    val make : desc -> t
  end                   
