
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
  
  let item_level_fmt = Colorize.format [ Fg (level_to_color item.level)]  (show_level item.level)
  and logger_name_fmt = Colorize.format [ Underline] item.logger_name
  and item_msg_fmt =
    match item.level with
    | Flash -> Colorize.format [ Fg Black; Bg LMagenta] item.msg
    | _ -> item.msg in
  
  (Printf.sprintf "%-6.3f %-30s %-20s %s" (Sys.time ()) logger_name_fmt
     item_level_fmt item_msg_fmt)
