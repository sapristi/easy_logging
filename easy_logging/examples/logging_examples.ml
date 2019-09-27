open Easy_logging

(* debug mode: outputs information about which handlers
   each messages is passed to  *)
(* let _ = Logging.debug := true;; *)

(* ]}
   {2 Presentation}

   Basic usage of the loggers.
   {[ *)
let logger_1 = Logging.make_logger
    "_1_ Demo" Debug
    [Cli Debug];;

logger_1#debug   "This is a debug message";
logger_1#trace   "This is a trace message";
logger_1#info    "This is an info message";
logger_1#warning "This is a warning message";
logger_1#error   "This is an error message";
logger_1#flash   "This is a FLASH message";

(* logger_1 sublogger: will use logger_1 handlers *)
let logger_1_sublogger = Logging.get_logger "_1_ Demo.sublogger"
in
logger_1_sublogger#set_level Info;

(* prefix log method with « l » to log lazy messages *)
let heavy_calculation () = "heavy result" in
logger_1_sublogger#ldebug (lazy (heavy_calculation ()));

(* Use printf like formatting *)
logger_1_sublogger#warning "This is %s" "format !!";


(* prefix log method with « s » to log static strings *)
let mystr = "this is a string" in
logger_1_sublogger#sinfo mystr;


(* use automatic tags *)
let counter = ref 0 in
let tag () =
  counter := !counter + 1;
  "M "^ (string_of_int (!counter))
in
logger_1#add_tag_generator tag;
logger_1#info "first message";
logger_1#info "second message";
logger_1#info "third message";

(* you can also add tags by hand *)

logger_1#info ~tags:["OOO"] "another message";
(* ]}
   {2 Modifying the level of a handler}
   {[ *)
let logger_4_root = Logging.get_logger "" in
logger_4_root#error "WTF1";

let h = Handlers.make (Cli Debug) in
let logger_4_sub = Logging.make_logger "_4_ handlerLevelTest" Debug [] in
logger_4_sub#error "WTF2";
logger_4_sub#add_handler h;
logger_4_sub#debug "this message is displayed";
Handlers.set_level h Info;
logger_4_sub#debug "this message is not displayed";
(* ]}
   {2 Modifying the file handler defaults}
   {[ *)
module H = Handlers
let config : H.FileHandler.config ={
  logs_folder= "test/";
  truncate= false;
  file_perms=0o664;
  date_prefix=None;
  versioning=Some 2;
  suffix=".log";
};;
H.set_file_handlers_config config;;
module TestLogging = Internal.MakeLogging(H)
let logger_5 = TestLogging.make_logger
    "_4_ File logger demo" Debug [File ("test", Debug)];;
logger_5#info "this is a message";
assert (Sys.file_exists "test/test_00.log");
(* ]}
   {2 Subloggers}
   {[ *)
let logger_6_A = Logging.get_logger "_6_ SubLoggers.A"
and logger_6_AB = Logging.get_logger "_6_ SubLoggers.A.B"
and logger_6_AC = Logging.get_logger "_6_ SubLoggers.A.C" in
let h = Handlers.make (Cli Debug) in
logger_6_A#add_handler h; logger_6_AC#add_handler h;
logger_6_A#set_level Debug;
logger_6_A#info "one line";
logger_6_AB#info "another line";
logger_6_AC#warning "two lines";
(* ]}
   {2 Json formatter}
   {[ *)
let logger_8 = Logging.get_logger "_8_ Json Formatter"
and h = Handlers.make (Cli Debug) in
Handlers.set_formatter h Formatters.format_json;
logger_8#add_handler h;
logger_8#info "it is ok";
logger_8#warning "is it json\"\nis it";
(* ]}
   {2 Filters}
   {[ *)
let logger_9 = Logging.get_logger "_9_ Handler filter" in
logger_8#set_level Debug;
let h = Handlers.make (Cli Debug) in
Handlers.add_filter h (fun _ -> false);
logger_9#warning "this is not printed"
(* ]}
   {2 Custom handlers module example }

   Replacing the DefaultHandlers module with your own.
   {[ *)

(* Custom handlers example:
 *   logs are stored in a given list ref *)
module MyHandlers =
struct
  type t = string -> unit
  type tag = unit

  type log_formatter = Internal.Logging_types.log_item -> string

  type desc = string list ref
  [@@deriving yojson]

  let apply h (item : Internal.Logging_types.log_item) = h item.msg
  let make (_internal : desc) =
    fun s -> _internal := s::!_internal

  type config = unit
  [@@deriving of_yojson]
  let default_config = ()
  let set_config = fun _ -> ()
end

module MyLogging = Internal.MakeLogging(MyHandlers)

let l = ref [];;
let logger_2 = MyLogging.make_logger "_2_ Custom Handlers module" Debug [l];;
logger_2#info "this is a message";
assert (!l = ["this is a message"]);

logger_2#set_level Warning;
logger_2#debug "this message will not be passed to the handler";
assert (!l = ["this is a message"]);
