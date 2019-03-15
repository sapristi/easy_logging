
open Easy_logging_types
              


let format_default item =
  Printf.sprintf "%-6.3f %-20s %-10s %s" (Sys.time ()) item.logger_name
    (show_level item.level) item.msg
  
      
let format_color item =
  
  let level_to_color lvl =
    match lvl with
    | Flash -> Colorize.LMagenta
    | Error -> Colorize.LRed
    | Warning -> Colorize.LYellow
    | Info -> Colorize.LBlue
    | Debug -> Colorize.Green
  in
  
  let item_level_str = Colorize.colorize  ~fgc:(level_to_color item.level)  (show_level item.level) in
  let item_msg_str =
    match item.level with
    | Flash -> Colorize.colorize ~fgc:Colorize.Black ~bgc:Colorize.LMagenta item.msg
    | _ -> item.msg in
  
  (Printf.sprintf "%-6.3f %-20s %-30s %s" (Sys.time ()) item.logger_name
     item_level_str item_msg_str)
