## PARAMETER SETTINGS

AGE_BOUNDARY <- c(20, 40, 60) # Do not include 0 or a maximum value, these will be added (max=Inf)
DEFAULT_UK_THRESHOLD <- 0.65     

## FLAG CONSTANTS ##

IS_WRITING_TO_LOGFILE <- TRUE
IS_USING_COMMAND_LINE_INPUT <- TRUE

# The location on the command line of the threshold
THRESHOLD_COMMAND_LINE_POSITION <- 3

# DIRECTORY LOCATIONS (in case the files are located in different places)

INPUT_FILE_DIRECTORY <- "/Users/timothystone/Desktop/LSHTM/"
OUTPUT_DIRECTORY <- INPUT_FILE_DIRECTORY
METADATA_FILE_DIRECTORY <- INPUT_FILE_DIRECTORY
LOGFILE_DIRECTORY <- INPUT_FILE_DIRECTORY
INPUT_LOG_FILE_SEARCH_PATTERN <- "log.txt"

# FILENAMES
METADATA_FILENAME = "DummyMetadata.csv"

## PACKAGES ##

library(magrittr)
library(stringr)
library(dplyr)

# File prefix for log, includes a timestamp (date + hour_minutes)
timestamp <- gsub(":", "_", Sys.time()) %>% gsub("^(.+)_\\d{2}$", "\\1", .)

# Use default UK threshold, but change it if command line value specified, and it exists
uk_threshold <- DEFAULT_UK_THRESHOLD
if (IS_USING_COMMAND_LINE_INPUT) {
  commandline_threshold <- commandArgs()[THRESHOLD_COMMAND_LINE_POSITION]
  if (!grepl("[^\\d.]", commandline_threshold) & !is.na(commandline_threshold)
      & length(commandline_threshold) != 0) {
    uk_threshold <- commandline_threshold
  }
}

metadata <- read.csv(paste(METADATA_FILE_DIRECTORY, METADATA_FILENAME, sep=""))
input_log_file <- dir(INPUT_FILE_DIRECTORY, pattern = INPUT_LOG_FILE_SEARCH_PATTERN)
output_log_file <- paste(LOGFILE_DIRECTORY, timestamped_filename,".log.txt", sep="")

if (nchar(input_log_file) == 0) {
  if (IS_WRITING_TO_LOGFILE) {
    cat(paste(OUTPUT_DIRECTORY, "\nNo Input logfile for file", ), file = output_log_file, append = T)
  }
  stop("There is no logfile\n")
}

input_log_filehandle <- file(paste(INPUT_FILE_DIRECTORY, "/",input_log_file, sep=""), "r")
this_line <- readLines(input_log_filehandle, n = 1)

sample_data_frame <- data.frame()

is_logfile_readable <- (1)
while (is_logfile_readable) {
  
  if (grepl("^\\d+\\s+\\d", this_line)) {
    cluster_number <- str_extract(this_line, "^\\d+") %>% as.numeric()
    cat("Cluster Number is ", cluster_number, '\n')
    cluster_data <- strsplit(this_line, "\t")
    samples <- gsub("\\[(.*)\\]","\\1", cluster_data[[1]][4]) %>% strsplit(., ", ")
    bootstrap <- cluster_data[[1]][5]
    genetic_distance <- cluster_data[[1]][6]
    samples <- samples[[1]]
    uk_sample_reference <- grep("England|Scotland|Wales|Northern Ireland", samples)
    if (length (samples) / length(uk_sample_reference)    < uk_threshold) {
      next()
    }
     
    remaining_uk_samples <- strsplit(samples[uk_sample_reference], "/")  
    uk_nation <- lapply(remaining_uk_samples, `[[`, 2) %>% unlist()
    id <- lapply(remaining_uk_samples, `[[`, 4) %>% gsub(".*\\|(.+)\\|.+$", "\\1", .) %>% unlist() 
    date <- lapply(remaining_uk_samples, `[[`, 4) %>% gsub(".*\\|.+\\|(.+)$", "\\1", .) %>% unlist() 
    city <- lapply(remaining_uk_samples, `[[`, 3) %>% gsub("([^-]+).*$", "\\1", ., perl = T) %>% unlist() 
    full_city <- lapply(remaining_uk_samples, `[[`, 3) %>% unlist()
    
    cluster_data_frame <- data.frame(Sample.id = id, Cluster.number = cluster_number, UK.location = uk_nation, Fasta.date = date,
                                     FastaFile.ctiy = city, FastaFile.FullCity = full_city)
    cluster_data_frame <- left_join(cluster_data_frame, metadata, by = "Sample.id")
    
    
    # Add the age groups using the cut() function and turn it into a numeric vector
    cluster_data_frame <- mutate(cluster_data_frame, Age.Group = as.numeric(cut(Age, c(0, AGE_BOUNDARY, Inf))))
    
    
    
    sample_data_frame <- rbind(sample_data_frame, cluster_data_frame)
    
  }
  
  this_line <- readLines(input_log_filehandle, n = 1)    
  if (length(this_line) == 0) {
    is_logfile_readable <- 0
  } 
}

close(input_log_filehandle)

output_filename <- paste(OUTPUT_DIRECTORY, "CP_ClusterAssignment.csv", sep="")
write.csv(sample_data_frame, file = output_filename, row.names=F)


