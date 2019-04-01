type log_level = Easy_logging_type.level

                   
let log_level_to_yojson lvl : Yojson.Safe.json =
  `String (show_level lvl)
let log_level_of_yojson lvl_json =
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
    include Easy_logging_type.HandlersT

      
    type config
       [@@deriving of_yojson]
    val default_config : config
    val set_config : config -> unit
  end
