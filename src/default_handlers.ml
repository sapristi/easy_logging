(** 
In the [DefaultHandlers] module, handlers have level of their own. Their are two kinds of logger : 

 - Cli handler: outputs colored messages to stdout 
   {[ let h = Default_handlers.make (Cli Debug) ]}
 - File handler : outputs messages to a given file
   {[ let h = Default_handlers.make (File ("filename", Debug)) ]}

 *)


open Easy_logging_types
open Default_format
(** {1 Type definitions } *)

type tag = string
type log_item = {
    level : Easy_logging_types.level;
    logger_name : string;
    msg : string;
    tags : string list
  }
              
module T = 
  struct
    type tag = string
    type log_item  = {
        level : level;
        logger_name : string;
        msg : string;
        tags : string list
      }
    let show_tag = fun x -> x end
module Formatter = Default_format (T)

                 
type log_formatter = level -> Formatter.item_format


(** type of a handler *)
type t =
  {
    mutable fmt : log_formatter;
    mutable level : Easy_logging_types.level;
    output : out_channel;
  }

  
   
let format_default _ : Formatter.item_format =
  L (" ", [Timestamp; F("%-10s", Level); F("%-20s", Logger_name); Tags ("[", "|","]"); Msg])

let format_color level : Formatter.item_format =
  let level_style, msg_style = 
    match level with
    | Flash -> [Colorize.Fg Colorize.LMagenta], [Colorize.Fg Black; Bg LMagenta]
    | Error -> [Fg Colorize.LRed], []
    | Warning -> [Fg Colorize.LYellow], []
    | Info -> [Fg Colorize.LBlue], []
    | Trace -> [Fg Colorize.Cyan], []
    | Debug -> [Fg Colorize.Green], []
    | NoLevel -> [Fg Colorize.Default], []
  in L (" ", [Timestamp; F("%-10s", C (level_style, Level)); F("%-20s", Logger_name); Tags ("[", "|","]"); C (msg_style, Msg)])

   
(** {1 Handlers creation and setup utility functions } *)
  
  
let make_cli_handler level =
  {fmt = format_color;
   level = level;
   output = stdout}


  
type file_handlers_config = {
    logs_folder: string;
    truncate: bool;
    file_perms: int}

let file_handlers_defaults = {
    logs_folder = "logs/";
    truncate = true;
    file_perms = 0o660;
  }

type config =
  {mutable file_handlers: file_handlers_config}
let config = {file_handlers = file_handlers_defaults}

let set_config c = config.file_handlers <- c.file_handlers
let make_file_handler level filename  =
  
  if not (Sys.file_exists config.file_handlers.logs_folder)
  then  
    Unix.mkdir config.file_handlers.logs_folder 0o775;

  let open_flags =
    if config.file_handlers.truncate
    then [Open_wronly; Open_creat;Open_trunc]
    else [Open_wronly; Open_creat]
  in
  let oc = 
    open_out_gen open_flags
      config.file_handlers.file_perms
      (config.file_handlers.logs_folder^filename)
      
  in
  {fmt = format_default;
   level = level;
   output = oc;
  }
  
  
type desc = | Cli of level | File of string * level
   
let make d = match d with
  | Cli lvl -> make_cli_handler lvl
  | File (f, lvl) -> make_file_handler lvl f
                  
(** {1 Handlers usage } *)
                   
let set_level (h:t) lvl =
  h.level <- lvl
let set_formatter h fmt =
  h.fmt <- fmt


let apply (h : t) (item: log_item) =
  if item.level >= h.level
  then
    (
      let full_log_msg = Formatter.format_item (h.fmt item.level) item.level item.logger_name item.tags item.msg in 
      output_string h.output (full_log_msg ^ "\n");
      flush h.output;
    )
