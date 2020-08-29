initialize_logger <- function(fileName){

  my_logfile = paste0(fileName, '.txt')

  my_console_appender = log4r::console_appender()
  my_file_appender = log4r::file_appender(my_logfile, append = TRUE)

  my_logger <- log4r::logger(threshold = "INFO",
                             appenders= list(my_console_appender,my_file_appender))

  return(my_logger)

}
