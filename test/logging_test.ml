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
(* Custom handlers *)
