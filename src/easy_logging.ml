

(* Type for log levels *)
type log_level = Easy_logging_types.level
               [@@deriving show { with_path = false }, yojson]
module type HandlersT = Easy_logging_types.HandlersT
                     
                      
module MakeLogging (H : HandlersT) =
  struct

  
    class logger
            ?parent:(parent=None)
            (name: string)
      =
    object(self)
             
      val name = name
      val mutable level : log_level option = None              
      val mutable handlers : H.t list = []
      val parent : logger option = parent
      val mutable propagate = true

      method set_level new_level =
        level <- Some new_level
      method add_handler h = handlers <- h::handlers
      method set_propagate p = propagate <- p
           
      method effective_level : log_level =
        match level, parent  with
        | None, None  -> NoLevel
        | None, Some p -> p#effective_level
        | Some l,_ -> l
                          
      method get_handlers =
        match propagate, parent with
        | true, Some p -> handlers @ p#get_handlers
        | _ -> handlers

      method private treat_msg : 'a. ('a -> string) -> H.tag list -> log_level -> 'a -> unit
        = fun unwrap_fun tags msg_level msg ->
        
        let item : H.log_item= {
            level = msg_level;
            logger_name = name;
            msg = unwrap_fun msg;
            tags=tags} in 
        List.iter (fun handler ->
            H.apply handler item)
          self#get_handlers
        
      method private _log_msg : 'a. ('a -> string) -> H.tag list -> log_level -> 'a -> unit
        = fun unwrap_fun tags msg_level msg ->
           if msg_level >= self#effective_level
           then
             self#treat_msg unwrap_fun tags msg_level msg
           else
             ()                           
          
      method private _flog_msg : 'a. H.tag list -> log_level -> ('a, unit, string, unit) format4 -> 'a
        =  fun tags msg_level -> 
        if msg_level >= self#effective_level
        then
          Printf.ksprintf (
              self#treat_msg (fun x -> x) tags msg_level)
        else Printf.ifprintf () 
        

      method flash : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Flash
      method error : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Error
      method warning : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Warning
      method info : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Info        
      method debug : 'a. ?tags:H.tag list -> ('a, unit, string, unit) format4 -> 'a
        = fun ?tags:(tags=[]) -> self#_flog_msg tags Debug

                               
      method sflash ?tags:(tags=[]) = self#_log_msg (fun x->x) tags Flash
      method serror ?tags:(tags=[]) = self#_log_msg (fun x->x) tags Error
      method swarning ?tags:(tags=[]) = self#_log_msg (fun x->x) tags Warning
      method sinfo ?tags:(tags=[]) =  self#_log_msg (fun x->x) tags Info
      method sdebug ?tags:(tags=[]) = self#_log_msg (fun x->x) tags Debug

                                    
      method lflash ?tags:(tags=[]) = self#_log_msg Lazy.force tags Flash
      method lerror ?tags:(tags=[]) = self#_log_msg Lazy.force tags Error
      method lwarning ?tags:(tags=[]) = self#_log_msg Lazy.force tags Warning
      method linfo ?tags:(tags=[]) =  self#_log_msg Lazy.force tags Info
      method ldebug ?tags:(tags=[]) = self#_log_msg Lazy.force tags Debug
    end

    let root_logger = new logger "root"
      
    module Infra =
      Logging_infra.MakeTree(
          struct
            type t = logger
            let make (n:string) parent = new logger ~parent n
            let root = root_logger
          end)
                        
      
    let get_logger name =
      Infra.get name
        
    let make_logger ?propagate:(propagate=true) name lvl hdescs  =
      let l = Infra.get name in
      l#set_level lvl;
      l#set_propagate propagate;
      List.iter (fun hdesc -> l#add_handler (H.make hdesc)) hdescs;
      l

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

module Default_handlers = Default_handlers
                
module Logging = MakeLogging(Default_handlers)


