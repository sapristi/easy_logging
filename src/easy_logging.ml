


open Easy_logging_types
open Batteries
open File


  
(** Makes a logging module from a Handlers module *)
module Make (H : HandlersT) =
  struct

    (** logger class *)
    class logger
            (name: string)
            (levelo: level option)
            (handlers_desc : H.desc list)  =
    object(self)

      (** Handlers associated to the logger *)
      val mutable handlers = List.map H.make handlers_desc

      (** optional level of the logger *)
      val mutable levelo = levelo

      (** Name of the logger *)
      val name = name

 
      method log_msg msg_level msg =
        match levelo with
        | None ->()
        | Some level ->
           if msg_level >= level
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
           if msg_level >= level
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
        
    let make_logger name lvl hdescs  =
      let l = new logger name lvl hdescs in
      Hashtbl.add _loggers name l;
      l
      
      
    let dummy = make_logger "dummy" None []
    module Handlers = H
                    
  end

(** Instantiation of [Make] over [Default_handlers] *)
module Logging = Make(Default_handlers)


(*module Default_handlers = Default_handlers
               
(** Default formatters provided by easy_logging *)
module Default_formatters = Default_formatters

(** Default handlers provided by easy_logging *)


 (** Types used in easy_logging *) *)
module Types = Easy_logging_types
                
                
