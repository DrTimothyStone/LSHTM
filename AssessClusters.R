## CONSTANTS ##

IS_WRITING_TO_LOGFILE = TRUE
OUTPUT_DIRECTORY = "/Users/timothystone/Desktop/LSHTM/"
IS_USING_COMMAND_LINE_INPUT = TRUE

# AGE BOUNDARY implicitly includes 0 as the first boundary
# and that the final number is x+, eg. 65+ (i.e. boundary to the max age)
# The boundary will be inclusive
AGE_BOUNDARY <- c(20, 40, 60)

# True False Encoding
TF_ENCODING <- c("Y", "N")

# If no command line used or none provided use 65%
DEFAULT_UK_THRESHOLD = 0.65     

# Input file directory can be a single directory name, or a search string
INPUT_FILE_DIRECTORY = "/Users/timothystone/Desktop/LSHTM/cp/"
INPUT_FILE_SEARCH_PATTERN = "bs*" # change to "*" to include all files in the directory
METADATA_FILE_DIRECTORY = "/Users/timothystone/Desktop/LSHTM/"

# The first numerical argument is normally at position 3 on commandArgs()
THRESHOLD_COMMAND_LINE_POSITION <- 3

## PACKAGES ##

library(magrittr)
library(dplyr)

timestamp <- gsub(":", ".", Sys.time())
generic_filename <- paste("CPfilter", timestamp,sep="")

uk_threshold <- DEFAULT_UK_THRESHOLD

# Check command line for numerical-only input, non-NA input and non-zero input
if (IS_USING_COMMAND_LINE_INPUT) {
  commandline_threshold <- commandArgs()[THRESHOLD_COMMAND_LINE_POSITION]
  if (!grepl("[^\\d.]", commandline_threshold) & !is.na(commandline_threshold)
      & length(commandline_threshold) != 0) {
    uk_threshold <- commandline_threshold
  }
}

all_directory <- dir(path = INPUT_FILE_DIRECTORY, pattern = INPUT_FILE_SEARCH_PATTERN)

for (this_directory in all_directory) {
  full_path_name <- paste(INPUT_FILE_DIRECTORY, this_file, sep="")
  input_log_file <- dir(full_path_name, pattern = ".log.txt")
  
  logfile <- paste(full_path_name,".log.txt")
  this_file <- paste(full_path_name, )
  if (nchar(logfile) == 0) {
    if (IS_WRITING_TO_LOGFILE) {
      cat(paste(OUTPUT_DIRECTORY, "\nNo Input logfile\n"), file = logfile, append = T)
    }
    next()
  }
}




