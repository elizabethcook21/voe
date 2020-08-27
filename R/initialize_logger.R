initialize_logger <- function(){
  # in case this isn't the first time using the logger, the log file will still have information in it. This cleans it out. Eventually we should probably change the filename to match the name of the function that called it
  if (file.exists("my_logfile.txt"))
    #Delete file if it exists
    file.remove("my_logfile.txt")
  my_logfile = "my_logfile.txt"

  my_console_appender = log4r::console_appender()
  my_file_appender = log4r::file_appender(my_logfile, append = TRUE)

  my_logger <- log4r::logger(threshold = "INFO",
                             appenders= list(my_console_appender,my_file_appender))

  return(my_logger)

}
