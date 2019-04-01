module E = Easy_logging
   
type log_level = E.log_level

let log_level_to_yojson lvl : Yojson.Safe.json =
  `String (E.log_level_to_string lvl)
let log_level_of_yojson lvl_json =
  match lvl_json with
  | `String lvl_str ->
     (
       match String.lowercase_ascii lvl_str with
       | "debug" -> Ok (Debug :  log_level)
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
    include E.HandlersT

    val desc_of_yojson :  Yojson.Safe.json -> (desc,string) result
    val desc_to_yojson : desc -> Yojson.Safe.json
    type config
       [@@deriving of_yojson]
    val default_config : config
    val set_config : config -> unit
  end


module Default_handlers =
  struct
    

    type file_handlers_config = {
        logs_folder: string;
        truncate: bool;
        file_perms: int}
                                  [@@deriving yojson]

    type config =  {mutable file_handlers: file_handlers_config}
                     [@@deriving yojson]

    include (E.Default_handlers :
             module type of E.Default_handlers
                            with type file_handlers_config := E.Default_handlers.file_handlers_config and
                                 type config := E.Default_handlers.config)
          
    
    let file_handlers_defaults = {
        logs_folder = "logs/";
        truncate = true;
        file_perms = 0o660;
      }
                               
    let default_config = {file_handlers = {
        logs_folder = "logs/";
        truncate = true;
        file_perms = 0o660;
      }}
    let config = {file_handlers = {
        logs_folder = "logs/";
        truncate = true;
        file_perms = 0o660;
      }}
                              
      
    let set_config c = config.file_handlers <- c.file_handlers
    type cli_json_desc =
      {cli : log_level}
        [@@deriving yojson]
    type file_json_desc_aux =
      {filename : string;level: log_level}
        [@@deriving yojson]
    type file_json_desc =
      {file : file_json_desc_aux}
        [@@deriving yojson] 
      
    let desc_of_yojson json =
      match cli_json_desc_of_yojson json with
      | Ok {cli=level} -> Ok (Cli level)
      | Error _ ->
         match file_json_desc_of_yojson json with
         | Ok {file={filename=fname;level=level}} ->
            Ok (File (fname, level))
         | Error r -> Error ("desc_of yojson: "^r)
                    
    let desc_to_yojson d =
      match d with
      | Cli lvl -> cli_json_desc_to_yojson {cli=lvl}
      | File (fname, lvl) ->
         file_json_desc_to_yojson
           {file= {filename=fname;level=lvl}}
        
  end           

module MakeLogging (H : HandlersT) =
  struct
    module L =  E.MakeLogging(H)
    include L

    type config_logger = {
        name: string;
        level : log_level;
        handlers : H.desc list;
        propagate : bool; [@default true]
      } [@@deriving of_yojson]

                       
    type config = {
        handlers : H.config; [@default H.default_config]
        loggers : config_logger list
      } [@@deriving of_yojson]
                
    let load_config config_str =
      match config_of_yojson (Yojson.Safe.from_string config_str) with
      | Ok {handlers;loggers} ->
         H.set_config handlers;
         List.iter (fun {name=name;
                         level=level;
                         handlers=handlers;
                         propagate=propagate} ->
             let l = make_logger name level handlers in
             l#set_propagate propagate) loggers
      | Error r ->
         failwith @@ "Error loading log config : "^r
  end

module Logging = MakeLogging(Default_handlers)
     
let config = {| 
{loggers: 
    [{"name" : "test_7", "level" : "debug", 
      "handlers": [{"cli" : "info"}]}]
}
|};;
Logging.load_config config;
let logger = Logging.get_logger "test_7" in
logger#info "this message";
logger#debug "but not this one";
