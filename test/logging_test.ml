open Easy_logging
       
let logger = Logging.make_logger
               "test" (Some Info)
               [Cli Debug];;
               

logger#info "ok";
logger#warning "warn";
logger#flash "FLASH";
logger#debug "not displayed"


let logger2 = new Logging.logger "test2" (Some Debug)
                [Cli Info];;

logger2#warning "WARNI" 
