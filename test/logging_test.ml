open Easy_logging
       
let logger = Logging.make_logger
               ~lvl:(Some Info)
               ~hdescs:[Cli Debug]
               "test";;

logger#info "ok";
logger#warning "warn";
logger#flash "FLASH";
logger#debug "not displayed"
