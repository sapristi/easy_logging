(**       
     Logging infrastructure inspired by the Python logging module.
     The aim of this module is to provide a quick and easy to use logging
     infrastructure, with most of the features that can be expected.

{4     Basic example } 
{[   
open Easy_logging
logger = Logging.make_logger "my_logger" (Some Debug) [Cli Debug];;
logger#info "log_message";; ]}   
will output to the stdout a message of the form
{v 1.306  test                 Info                 ok v}


{2 Overall description }

{4 Infrastructure }
Like in the python logging module, this logging infrastructure is based on four concepts:
 
{i loggers, handlers,  formatter and log items }

A call to {e logger} will create a {e log item}, which it will pass to its {e handlers}. Each {e handler} transforms the {e log item} to a string using its assigned formatter, and then treats the item (e.g. outputs to stdout or to a file).
{v
                                   ___________________________
                                  |         handler 1         |
                                  | (formatter) | (treatment) |
             _______________      |---------------------------|
            |     logger    | ==> | -> string  ==>    ( * )   |
            |---------------|     |___________________________|
message ==> | -> log item   |      ___________________________
            [_______________| ==> |         handler 2         |
                                  |            ...            |
v}


{4 Levels}

To each logger, handler and log message are associated a level, which will
be used to filter the messages going through the logging infrastructure.

The predefined levels are  
 + Debug   : used for debugging
 + Info    : used to trace program execution
 + Warning : used for warnings
 + Error   : used for errors
 + Flash   : used for one-shot debugging: displays an easy to spot message.

 *)

open Easy_logging_types
open Batteries
open File


(** Types used in easy_logging *)
module Types = Easy_logging_types

(** Handlers module type signature *)
module type HandlersT =
  sig
    type t =
      {mutable fmt : log_formatter;
       mutable level : level;
       output : unit IO.output}

    val handle : t -> log_item -> unit
    type desc
    val make : desc -> t
    val set_formatter : t -> log_formatter -> unit
  end

  
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

module Default_formatters = Default_formatters
(** Default formatters provided by easy_logging *)
module Default_handlers = Default_handlers
(** Default handlers provided by easy_logging *)
