
type color =
  | Default | Black | Red | Green | Yellow
  | Blue | Magenta | Cyan | Gray | White 
  | LRed | LGreen | LYellow | LBlue
  | LMagenta | LCyan | LGray

type style =
  | Bold | Underline | Invert | NoStyle
                              
let style_to_string s =
  match s with
  | Bold -> "\027[1m"   | Underline -> "\027[4m" | Invert -> "\027[7m"  
  | NoStyle -> "\027[0m"
             
let to_fg c =
  match c with 
  | Default -> "\027[39m" | Black -> "\027[30m"  | Red -> "\027[31m"
  | Green -> "\027[32m"   | Yellow -> "\027[33m" | Blue -> "\027[34m"
  | Magenta -> "\027[35m" | Cyan -> "\027[36m"   | Gray -> "\027[90m"
  | White -> "\027[97m"   | LRed -> "\027[91m"   | LGreen -> "\027[92m"
  | LYellow -> "\027[93m" | LBlue -> "\027[94m"  | LMagenta -> "\027[95m"
  | LCyan -> "\027[96m"   | LGray -> "\027[37m"
                                   
let to_bg c = 
  match c with 
  | Default -> "\027[49m" | Black -> "\027[40m"  | Red -> "\027[41m"
  | Green -> "\027[42m"   | Yellow -> "\027[43m" | Blue -> "\027[44m"
  | Magenta -> "\027[45m" | Cyan -> "\027[46m"   | Gray -> "\027[100m"
  | White -> "\027[107m"  | LRed -> "\027[101m"  | LGreen -> "\027[102m"
  | LYellow -> "\027[103m"| LBlue -> "\027[104m"  | LMagenta -> "\027[105m"
  | LCyan -> "\027[106m"   | LGray -> "\027[47m"
                                    
let colorize ?fgc:(fg=Default) ?bgc:(bg=Default) ?s:(style=NoStyle) t =
  Printf.sprintf "%s%s%s%s%s%s%s" (style_to_string style) (to_fg fg) (to_bg bg) t (to_fg Default) (to_bg Default)  (style_to_string NoStyle)
  
