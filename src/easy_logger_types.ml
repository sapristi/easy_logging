open Batteries
open File

   
type level =
  | Flash
  | Error
  | Warning
  | Info
  | Debug
[@@deriving ord, show { with_path = false }]
(** Log levels *)

  
let level_gt l1 l2 =
  compare_level l1 l2 <= 0


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
