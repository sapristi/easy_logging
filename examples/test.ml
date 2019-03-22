open Logging_examples
open Easy_logging
       

let logger = Logging.make_logger
               "test" (Some Debug)
               [Cli Debug];;
               
logger#debug "This is a debug message";
logger#info "This is an info message";
logger#warning "This is a warning message";
logger#error "This is an error message";
logger#flash "This is a FLASH message";
