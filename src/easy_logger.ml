open Easy_logger_types
open Batteries
open File

   
module type HandlersT =
  sig
    
    type t =
      {mutable fmt : log_formatter;
       mutable level : level;
       output : unit IO.output}

    val handle : t -> log_item -> unit
    type desc
    val make : desc -> t
  end
  
  
module Make (H : HandlersT) =
  struct
    
    module Level =
      struct
        type t = Easy_logger_types.level
               [@@deriving show {with_path = false}]
      end

    type log_formatter = Easy_logger_types.log_formatter
    class logger
            (name: string)
            (levelo: level option)
            (handlers_desc : H.desc list)  =
    object(self)
        
      val mutable handlers = List.map H.make handlers_desc
      val mutable levelo = levelo
      val name = name
               
      method log_msg msg_level msg =
        match levelo with
        | None ->()
        | Some level ->
           if level_gt msg_level level
           then
             begin
               let item = {
                   level = msg_level;
                   logger_name = name;
               msg = msg} in 
               List.iter (fun handler ->
                   H.handle handler item)
                 handlers
             end
           else
             ()                           
          
      method log_msg_lazy (msg_level : level) msg =
        match levelo with
        | None ->()
        | Some level ->
           if level_gt msg_level level
           then
             begin
               let item = {
                   level = msg_level;
                   logger_name = name;
                   msg = Lazy.force msg} in 
               List.iter (fun handler ->
                   H.handle handler item)
                 handlers
             end
           else
             ()
      method add_handler h = handlers <- h::handlers
      method set_level new_levelo =
        levelo <- new_levelo
        
      method flash = self#log_msg Flash
      method error = self#log_msg Error
      method warning = self#log_msg Warning
      method info =  self#log_msg Info
      method debug = self#log_msg Debug
                   
      method lflash = self#log_msg_lazy Flash
      method lerror = self#log_msg_lazy Error
      method lwarning = self#log_msg_lazy Warning
      method linfo =  self#log_msg_lazy Info
      method ldebug = self#log_msg_lazy Debug
    end


  
    let _loggers : (string, logger) Hashtbl.t =  Hashtbl.create 10
                                               
    let set_level p lvlo =
      Hashtbl.iter
        (fun n l  ->
          
          if String.starts_with n p
          then
            l#set_level lvlo;)
        _loggers
      
    let get_logger name =
      if Hashtbl.mem _loggers name
      then
        Hashtbl.find _loggers name
      else
        let l = new logger name None [] in
        Hashtbl.add _loggers name l;
        l
        
    let make_logger ?lvl:(lvl=None) ?hdescs:(hdescs=[]) name =
      let l = new logger name lvl hdescs in
      Hashtbl.add _loggers name l;
      l
      
      
    let dummy = make_logger "dummy" ~lvl:None ~hdescs:[]
    module Handlers = H
                    
  end

module Logger = Make(Default_handlers)

(* module logger for maybe far later *)
          
  
