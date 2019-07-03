open Easy_logging_types
   
module Default_format (T: sig
                           type tag
                           val show_tag: tag -> string end)
  = struct
   
  let reduce (f: 'a -> 'a -> 'a) (l: 'a list) (d: 'a) =
    let rec aux l res =
      match l with
      | [] -> res
      | h::t ->
         let res' = f res h in
         aux t res'
    in
    match l with
    | [] -> d
    | h::t -> aux t h 
            

  type item_format =
    | FC of (unit -> string)
    | Level
    | Timestamp
    | Msg
    | Tags of (string*string*string)
    | Logger_name
    | F of string * item_format
    | S of string
    | W of (string*string)*item_format
    | L of string*(item_format list)
      
  let format_tags format (tags : T.tag list) =
    let (start, stop, sep) = format in
    match tags with
    | [] -> ""
    | _ ->
       let tags_str = List.map T.show_tag tags in 
       let elems_str = reduce (fun s e -> e ^ sep ^ s) tags_str "" 
       in start ^ elems_str ^ stop
        
  let rec format_item_prefix item_format level logger_name tags msg =
    match item_format with
    | FC f -> f ()
    | Level -> show_level level
    | Timestamp -> string_of_float (Sys.time ())
    | Msg -> msg
    | Tags tag_format -> format_tags tag_format tags
    | Logger_name -> logger_name
    | F (f_string, format') ->
       let format_str = Scanf.format_from_string f_string "%s" in
       Printf.sprintf format_str (format_item_prefix format' level logger_name tags msg)
    | S s -> s
    | W ((start,stop), format') ->
       start ^ (format_item_prefix format' level logger_name tags msg) ^ stop
    | L (sep, format_l) ->
       let formatted = List.map (fun f -> format_item_prefix f level logger_name tags msg) format_l
       in
       reduce (fun s e -> e ^ sep ^ s)
         formatted ""
       
end
