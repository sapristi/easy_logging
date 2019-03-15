open Easy_logging
       
let logger = Logging.make_logger
               "test" (Some Info)
               [Cli Debug];;
               

logger#info "ok";
logger#warning "warn";
logger#flash "FLASH";
logger#debug "not displayed"
