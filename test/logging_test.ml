open Easy_logging

(* ************* *)
(* Basic example *)
let logger = Logging.make_logger
               "test" (Some Debug)
               [Cli Debug];;
               
logger#debug "This is a debug message";
logger#info "This is an info message";
logger#warning "This is a warning message";
logger#error "This is an error message";
logger#flash "This is a FLASH message";

(* another logger *)
let sublogger = Logging.make_logger
                  "test.sub" (Some Info)
                  [Cli Debug]
in

(* ***************** *)
(* log lazy messages *)
let heavy_calculation () = "heavy result" in
sublogger#ldebug (lazy (heavy_calculation ()));





(* ********************************** *)
(* Globally modifying logger level :
 * sets the level of all loggers whose
 * name begins with "test"            *)
Logging.set_level "test" (Some Warning);


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

    let apply h (item : log_item) = h item.msg
    let make (_internal : desc) =
      fun s -> _internal := s::!_internal
  end

module MyLogging = MakeLogging(MyHandlers)

let l = ref [];;
let mylogger = MyLogging.make_logger "mylogger" (Some Debug) [l];;
mylogger#info "this is a message";
assert (!l = ["this is a message"]);

mylogger#set_level (Some Warning);
mylogger#debug "this message will not be passed to the handler";
assert (!l = ["this is a message"]);






(* ************************** *)
(* Tags handlers example:     *)
module TaggedHandlers =
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

    let apply h (item : log_item) = h item

    let rec tags_to_string tags =
      let open Unix in
      match tags with
      | Time :: tags' -> 
         let {
   	     tm_sec;
   	     tm_min;
   	     tm_hour;
   	     tm_mday;
   	     tm_mon;
   	     _;
           } :tm = time () |> gmtime in
         let s = 
           Printf.sprintf "%d/%d %d:%d:%d" (tm_mday+1) (tm_mon+1) tm_hour tm_min tm_sec
         in s :: tags_to_string tags'
      | Value n :: tags' -> (string_of_int n) :: tags_to_string tags'
      | [] -> []
         
    let make () =
      fun item ->
      let tags_s = List.fold_left (fun a b -> a ^ " " ^ b) "" (tags_to_string item.tags) in
      tags_s ^ " " ^ item.msg
    |> print_endline
  end



module TagsLogging = MakeLogging(TaggedHandlers);;

let logger = TagsLogging.make_logger "tagged" (Some Debug) [()];;
logger#info ~tags:[Time; Value 4] "ok";
