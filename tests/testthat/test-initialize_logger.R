# test_that("Simple default logging works", {
#   my_logger <- initialize_logger('testing_Log')
#   log4r::info(my_logger, 'You created a logger that is set to defaults: to save a log in the current working directory')
#   expect_true(file.exists('logfile_testing_Log.txt'))
#   if(file.exists('logfile_testing_Log.txt')){
#     file.remove('logfile_testing_Log.txt')
#   }
# })
# 
# test_that("Logging without saving a logfile works", {
#   my_logger <- initialize_logger('testing_Log', FALSE)
#   log4r::info(my_logger, 'You created a logger that should not save a logfile')
#   expect_false(file.exists('logfile_testing_Log.txt'))
# })
# 
# test_that("Logging to a specific path works", {
#   my_logger <- initialize_logger('testing_Log', logFilePath = '~/Desktop')
#   log4r::info(my_logger, 'You created a logger that should save a file to the Desktop')
#   expect_true(file.exists('~/Desktop/logfile_testing_Log.txt'))
#   if(file.exists('~/Desktop/logfile_testing_Log.txt')){
#     file.remove('~/Desktop/logfile_testing_Log.txt')
#   }
# })