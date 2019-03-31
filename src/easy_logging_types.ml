

(** Possible level of a log item. *)
type level =
  | Debug
  | Info
  | Warning
  | Error
  | Flash
  | NoLevel
[@@deriving show { with_path = false }]

let level_to_yojson lvl : Yojson.Safe.json =
  `String (show_level lvl)
let level_of_yojson lvl_json =
  match lvl_json with
  | `String lvl_str ->
     (
       match String.lowercase_ascii lvl_str with
       | "debug" -> Ok Debug
       | "info" -> Ok Info
       | "warning" -> Ok Warning
       | "error" -> Ok Error
       | "flash" -> Ok Flash
       | "nolevel" -> Ok NoLevel
       | _ -> Error (lvl_str ^ " does not represent a valid log level")
     )
  | _ -> Error ("Cannot decode "^ (Yojson.Safe.to_string lvl_json) ^" to log level")

       
module type HandlersT =
  sig
    
    (** Type of a handler *) 
    type t
       
    type tag
       
    type log_item = {
        level : level;
        logger_name : string;
        msg : string;
        tags : tag list
      }
    type log_formatter = log_item -> string

    (** Applies the handler to a [log_item] *)
    val apply : t -> log_item -> unit

    (** Type used to instantiate a handler*)
    type desc
       [@@deriving yojson]
    (** Instantiates a handler *)
    val make : desc -> t

  end                   
