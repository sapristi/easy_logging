(** Is this doc *)

open Easy_logging

let _ = Logging.debug := true;;
   
(* ************* *)
(* Basic example *)
let logger_1 = Logging.make_logger
               "_1_ Demo" Debug
               [Cli Debug];;
               
logger_1#debug "This is a debug message";
logger_1#info "This is an info message";
logger_1#warning "This is a warning message";
logger_1#error "This is an error message";
logger_1#flash "This is a FLASH message";

(* another logger *)
let logger_1_sublogger = Logging.get_logger "_1_ Demo.sublogger"
in
logger_1_sublogger#set_level Info;

(* ***************** *)
(* log lazy messages *)
let heavy_calculation () = "heavy result" in
logger_1_sublogger#ldebug (lazy (heavy_calculation ()));

(* *** *)
logger_1_sublogger#warning "This is %s" "format !!";



(* ********************************** *)
(* Globally modifying logger level :
 * sets the level of all loggers whose
 * name begins with "test"            *)


(* ***************************** *)
(* Custom handlers example:    
 *   logs are stored in a given list ref *)
module MyHandlers =
  struct
    type t = string -> unit
    type tag = unit
    type log_item = {
        level : Easy_logging__.Easy_logging_types.level;
        logger_name : string;
        msg : string;
        tags : tag list
      }
                  
    type log_formatter = log_item -> string

    type desc = string list ref
                  [@@deriving yojson]

    let apply h (item : log_item) = h item.msg
    let make (_internal : desc) =
      fun s -> _internal := s::!_internal

    type config = unit
                [@@deriving of_yojson]
    let default_config = ()
    let set_config = fun _ -> ()
  end

module MyLogging = MakeLogging(MyHandlers)

let l = ref [];;
let logger_2 = MyLogging.make_logger "_2_ Custom Handlers module" Debug [l];;
logger_2#info "this is a message";
assert (!l = ["this is a message"]);

logger_2#set_level Warning;
logger_2#debug "this message will not be passed to the handler";
assert (!l = ["this is a message"]);



(* ************************** *)
(* Tags handlers example:     *)
module CustomTaggedHandlers =
  struct
    type tag =
      | Time
      | Value of int

    type log_item = {
        level : Easy_logging__.Easy_logging_types.level;
        logger_name : string;
        msg : string;
        tags : tag list
      }
    type t = log_item -> unit
                  
    type log_formatter = log_item -> string

    type desc = unit
                  [@@deriving yojson]

    let apply h (item : log_item) = h item

    let rec tags_to_string tags =
      let open Unix in
      match tags with
        
      | Time :: tags' -> 
         let {tm_sec; tm_min; tm_hour;
   	      tm_mday; tm_mon;  _; } :tm
           = time () |> gmtime
         in
         let s = 
           Printf.sprintf "%d/%d %d:%d:%d"
             (tm_mday+1) (tm_mon+1) tm_hour tm_min tm_sec
         in
         s :: tags_to_string tags'
          
      | Value n :: tags' ->
         ( "[Val: "^(string_of_int n)^"]" ) :: tags_to_string tags'

      | [] -> []
         
    let make () =
      fun item ->
      let tags_s = List.fold_left (fun a b -> a ^ " " ^ b) "" (tags_to_string item.tags) in
      tags_s ^ " " ^ item.msg
      |> print_endline

      
    type config = unit
                [@@deriving of_yojson]
    let default_config = ()
    let set_config = fun _ -> ()
  end



module TagsLogging = MakeLogging(CustomTaggedHandlers);;

let logger_3 = TagsLogging.make_logger "_3_ CustomTags" Debug [()];;
logger_3#info ~tags:[Time; Value 4] "log message with tags";



(* ******************************** *)
(* modifying the level of a handler *)
let logger_4_root = Logging.get_logger "" in
logger_4_root#error "WTF1";

let h = Default_handlers.make (Cli Debug) in
let logger_4_sub = Logging.make_logger "_4_ handlerLevelTest" Debug [] in
logger_4_sub#error "WTF2";
logger_4_sub#add_handler h;
logger_4_sub#debug "this message is displayed";
Default_handlers.set_level h Info;
logger_4_sub#debug "this message is not displayed";


(* *********************************** *)
(* modifying the file handler defaults *)

module H = Default_handlers
let config : H.config =
  {file_handlers = {
     logs_folder= "test/";
     truncate= false;
     file_perms=0o664;}};;
H.set_config config;;
module TestLogging = MakeLogging(H)
let logger_5 = TestLogging.make_logger
               "_4_ File logger demo" Debug [File ("test", Debug)];;
logger_5#info "this is a message";
assert (Sys.file_exists "test/test");




let logger_6_A = Logging.get_logger "_6_ SubLoggers.A"
and logger_6_AB = Logging.get_logger "_6_ SubLoggers.A.B"
and logger_6_AC = Logging.get_logger "_6_ SubLoggers.A.C" in
let h = Default_handlers.make (Cli Debug) in
logger_6_A#add_handler h; logger_6_AC#add_handler h;
logger_6_A#set_level Debug; 
logger_6_A#info "one line";
logger_6_AB#info "another line";
logger_6_AC#warning "two lines";




let logger_7 = Logging.make_logger "_7_ AutoWrap" Debug [Cli Debug] in
let message = "this is not a short message; is it going to get longer ?" in
let long_message = message ^ message ^ message ^ message ^ message in 
logger_7#info "%s" long_message;

let logger_8 = Logging.get_logger "_8_ Json Formatting" in
logger_8#set_level Debug;

let h = Default_handlers.make (Cli Debug) in
Default_handlers.set_formatter h Default_handlers.format_json;
logger_8#add_handler h;
logger_8#info "it is ok";
logger_8#warning "is it json\"\nis it";


let logger_9 = Logging.get_logger "_9_ Handler filter" in
logger_8#set_level Debug;
let h = Default_handlers.make (Cli Debug) in
Default_handlers.add_filter h (fun _ -> false);
logger_9#warning "this is not printed"

