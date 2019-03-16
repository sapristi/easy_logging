

open Easy_logging_types
open Batteries
open File
open Default_formatters

type t =
  {mutable fmt : log_formatter;
   mutable level : level;
   output : unit IO.output}
  
  
let outputs : (string, unit IO.output) Hashtbl.t =  Hashtbl.create 10
                                                  
let handle (h : t) (item: log_item) =
  if item.level >= h.level
  then
    (
      IO.write_line h.output (Printf.sprintf "%s" (h.fmt item));
      IO.flush h.output;
    )
  
let make_cli_handler level =
  Hashtbl.replace outputs "stdout" IO.stdout;
  {fmt = format_color;
   level = level;
   output = IO.stdout}
  
let make_file_handler level filename  =
  
  if not (Sys.file_exists "logs")
  then  
    Unix.mkdir "logs" 0o777;
  
  let oc = 
    if Hashtbl.mem outputs filename
    then
      Hashtbl.find outputs filename
    else
      let p = File.perm [user_read; user_write; group_read; group_write] in
      open_out ~mode:[`create (*; `append *)] ~perm:p ("logs/"^filename)
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
  List.iter  (fun x -> handle h fmt )
    [{level=Flash; logger_name="Flash"; msg="Flash"};
     {level=Error; logger_name="Error"; msg="Error"}; 
     {level=Warning; logger_name="Warning"; msg="Warning"};
     {level=Info; logger_name="Info"; msg="Info"};
     {level=Debug; logger_name="Debug"; msg="Debug"}] 
