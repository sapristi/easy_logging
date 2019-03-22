

open Easy_logging_types


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


let format_default (item : log_item) =
  Printf.sprintf "%-6.3f %-10s %-20s %s" (Sys.time ())
    (show_level item.level)
    item.logger_name
    item.msg
  
      
let format_color (item : log_item) =
  
  let level_to_color lvl =
    match lvl with
    | Flash -> Colorize.LMagenta
    | Error -> Colorize.LRed
    | Warning -> Colorize.LYellow
    | Info -> Colorize.LBlue
    | Debug -> Colorize.Green
  in
  
  let item_level_fmt = Colorize.format [ Fg (level_to_color item.level)]  (show_level item.level)
  and logger_name_fmt = Colorize.format [ Underline] item.logger_name
  and item_msg_fmt =
    match item.level with
    | Flash -> Colorize.format [ Fg Black; Bg LMagenta] item.msg
    | _ -> item.msg in
  
  (Printf.sprintf "%-6.3f %-20s %-30s %s" (Sys.time ())
     item_level_fmt
     logger_name_fmt
     item_msg_fmt)

  
  
let apply (h : t) (item: log_item) =
  if item.level >= h.level
  then
    (
      output_string h.output (Printf.sprintf "%s\n" (h.fmt item));
      flush h.output;
    )
  
let make_cli_handler level =
  {fmt = format_color;
   level = level;
   output = stdout}
  
let make_file_handler level filename  =
  
  if not (Sys.file_exists "logs")
  then  
    Unix.mkdir "logs" 0o777;
  
  let oc = 
      open_out @@ "logs/"^filename
  in
  {fmt = format_default;
   level = level;
   output = oc;
  }
  
let set_level h lvl =
  h.level <- lvl
let set_formatter h fmt =
  h.fmt <- fmt
  
let handlers : (string, t) Hashtbl.t = Hashtbl.create 10
let register_handler name handler =
  Hashtbl.replace handlers name handler
  
  
type desc = | Cli of level | File of string * level | Reg of string

let make d = match d with
  | Cli lvl -> make_cli_handler lvl
  | File (f, lvl) -> make_file_handler lvl f
  | Reg n ->
     Hashtbl.find handlers n
    
let handle_test h fmt =
  List.iter  (fun x -> apply h fmt )
    [{level=Flash; logger_name="Flash"; msg="Flash"; tags=[]};
     {level=Error; logger_name="Error"; msg="Error"; tags=[]}; 
     {level=Warning; logger_name="Warning"; msg="Warning"; tags=[]};
     {level=Info; logger_name="Info"; msg="Info"; tags=[]};
     {level=Debug; logger_name="Debug"; msg="Debug"; tags=[]}] 
