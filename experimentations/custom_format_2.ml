type msg_format =
  | C of (unit -> string)
  | Timestamp
  | Msg
  | Tags of (string*string*string)
  | Logger_name
  | F of string * msg_format
  | S of string
  | W of (string*string)*msg_format
  | L of string*(msg_format list)


type test_itme = {
    logger_name : string;
    msg : string;
    tags : string list
  }


let reduce (f: 'a -> 'a -> 'a) (l: 'a list) (d: 'a) =
  let rec aux l res =
    match l with
    | [] -> res
    | h::t ->
       let res' = f h res in
       aux t res'
  in
  match l with
  | [] -> d
  | h::t -> aux t h


let format_tags format (tags : string list) =
  let (start, stop, sep) = format in
  match tags with
  | [] -> ""
  | _ -> 
     let elems_str = reduce (fun s e -> e ^ sep ^ s) tags "" 
     in start ^ elems_str ^ stop
          
let rec format_item item_format item =
  match item_format with
  | C f -> f ()
  | Timestamp -> string_of_float (Sys.time ())
  | Msg -> item.msg
  | Tags tag_format -> format_tags tag_format item.tags
  | Logger_name -> item.logger_name
  | F (f_string, format') ->
     let format_str = Scanf.format_from_string f_string "%s" in
     Printf.sprintf format_str (format_item format' item)
  | S s -> s
  | W ((start,stop), format') ->
     start ^ (format_item format' item) ^ stop
  | L (sep, format_l) ->
     let formatted = List.map (fun f -> format_item f item) format_l
     in
     reduce (fun s e -> e ^ sep ^ s)
       formatted ""



let item_1 = {
    logger_name = "mylogger";
    msg = "this is a message #1";
    tags = ["t1";"t2"]}

let item_2 = {
    logger_name = "mylogger";
    msg = "this is a message #2";
    tags = []}

let item_3 = {
    logger_name = "mylogger";
    msg = "this is a message #2";
    tags = ["t1"]}

let i_formatter =
  L (" ", [S "["; Timestamp; Logger_name; S "]"; Tags ("[ ", " ]", " | "); Msg]);;

let i_formatter_2 =
  L (" ", [W (("[","]"),
              L (" :: ",
                 [Timestamp; F ("%-10s",Logger_name)]));
           Tags ("|", "|", " | ");
           S "\n";
           Msg]);;

format_item i_formatter item_1;;
format_item i_formatter item_2;;
format_item i_formatter_2 item_3;;
