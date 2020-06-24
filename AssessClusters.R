COMMON_FILE_DIRECTORY <- "/Users/timothystone/Desktop/LSHTM/"

## PACKAGES ##

library(magrittr)
library(stringr)
library(dplyr)
library(data.table)

## PARAMETER SETTINGS

AGE_BOUNDARY <- c(20, 40, 60) # Do not include 0 or a maximum value, these will be added (max=Inf)
DEFAULT_UK_THRESHOLD <- 0.65     

## FLAG CONSTANTS ##

IS_WRITING_TO_LOGFILE <- FALSE
IS_USING_COMMAND_LINE_INPUT <- FALSE

# The location on the command line of the threshold
THRESHOLD_COMMAND_LINE_POSITION <- 3

# DIRECTORY LOCATIONS (in case the files are located in different places)

INPUT_FILE_DIRECTORY <- COMMON_FILE_DIRECTORY
OUTPUT_DIRECTORY <- COMMON_FILE_DIRECTORY
OUTPUT_FILENAME <- "CP_ClusterAssignment.tsv"
METADATA_FILE_DIRECTORY <- COMMON_FILE_DIRECTORY
METADATA_FILENAME = "metadata_2020-06-08.tsv"
LOGFILE_DIRECTORY <- COMMON_FILE_DIRECTORY
INPUT_LOGFILE_SEARCH_PATTERN <- "clusterPicks_log.txt"

# FILENAMES
if (grepl("csv$", METADATA_FILENAME, perl = T)) {
  metadata <- read.csv(paste(METADATA_FILE_DIRECTORY, METADATA_FILENAME, sep=""))
} else { 
  metadata <- fread(paste(METADATA_FILE_DIRECTORY, METADATA_FILENAME, sep=""), header=T, na.strings = "?")
}

input_logfile <- dir(INPUT_FILE_DIRECTORY, pattern = INPUT_LOGFILE_SEARCH_PATTERN)
output_filename <- paste(OUTPUT_DIRECTORY, OUTPUT_FILENAME, sep="")


# File prefix for log, includes a timestamp (date + hour_minutes)
timestamp <- gsub(":", "_", Sys.time()) %>% gsub("^(.+)_\\d{2}$", "\\1", .)
if (IS_WRITING_TO_LOGFILE) {
  output_logfile <- paste(LOGFILE_DIRECTORY, "CP_assignment_", timestamp,".log.txt", sep="")
}

# Use default UK threshold, but change it if command line value specified, and it exists
uk_threshold <- DEFAULT_UK_THRESHOLD
if (IS_USING_COMMAND_LINE_INPUT) {
  commandline_threshold <- commandArgs()[THRESHOLD_COMMAND_LINE_POSITION]
  if (!grepl("[^\\d.]", commandline_threshold) & !is.na(commandline_threshold)
      & length(commandline_threshold) != 0) {
    uk_threshold <- commandline_threshold
  }
}

if (nchar(input_logfile) == 0) {
  if (IS_WRITING_TO_LOGFILE) {
    cat(paste("No Input logfile found at",input_logfile,"\n" ), file = output_logfile, append = T)
  }
  stop("There is no logfile\n")
}

input_logfilehandle <- file(paste(INPUT_FILE_DIRECTORY, "/",input_logfile, sep=""), "r")
sample_data_frame <- data.frame()

line <- readLines(input_logfilehandle, n = 1)
is_logfile_readable <- (1)
while (is_logfile_readable) {
  
  #Check to see if there is a number + space in order to grab cluster info from logfile.  
  if (grepl("^\\d+\\s+\\d", line)) {
    cluster_number <- str_extract(line, "^\\d+") %>% as.numeric()
    
    if (IS_WRITING_TO_LOGFILE) {
      cat("Cluster: ", cluster_number,":" , file = output_logfile, append = T)
    }
    
    #cat("Cluster Number is ", cluster_number, '\n')
    
    tip_data <- strsplit(line, "\t")
    tip_members <- gsub("\\[(.*)\\]","\\1", cluster_data[[1]][4]) %>% strsplit(., ", ")
    tip_members <- tip_members[[1]]
    
    uk_sample_reference <- grep("England|Scotland|Wales|Northern Ireland|United Kingdom", tip_members, perl=T)
    uk_proportion <- length(uk_sample_reference) / length(tip_members)
    
    bootstrap <- tip_data[[1]][5]
    genetic_distance <- tip_data[[1]][6]
    
    # Reject tip if not containing sufficient UK proportion, process file otherwise
    if (uk_proportion < uk_threshold) {
      if (IS_WRITING_TO_LOGFILE) {
        cat("\tUK Content=" ,uk_proportion, "\tRejected\n", file = output_logfile, append = T)
      }
    } else {
      
      # Extract the relevant information from the tip data using string-split and regular expressions, unlist them into vectors
      remaining_uk_samples <- strsplit(tip_members[uk_sample_reference], "/")  
      uk_nation <- lapply(remaining_uk_samples, `[[`, 2) %>% unlist()
      id <- lapply(remaining_uk_samples, `[[`, 4) %>% gsub(".*\\|(.+)\\|.+$", "\\1", .) %>% unlist() 
      date <- lapply(remaining_uk_samples, `[[`, 4) %>% gsub(".*\\|.+\\|(.+)$", "\\1", .) %>% unlist() 
      city <- lapply(remaining_uk_samples, `[[`, 3) %>% gsub("([^-]+).*$", "\\1", ., perl = T) %>% unlist() 
      full_city <- lapply(remaining_uk_samples, `[[`, 3) %>% unlist()
      
      cluster_data_frame <- data.frame(gisaid_epi_isl = id, ClusterNumber = cluster_number, UKLocation = uk_nation, TipIDDate = date,
                                       TipIDCity = city, TipIDFullCity = full_city, Bootstrap = bootstrap, GD = genetic_distance)
      cluster_data_frame <- left_join(cluster_data_frame, metadata, by = "gisaid_epi_isl")
      
      # Add the age groups using the cut() function and turn it into a numeric vector
      # Lower bound = 0, Upper bound = Infinity
      cluster_data_frame <- mutate(cluster_data_frame, age.Group = as.numeric(cut(age, c(0, AGE_BOUNDARY, Inf))))
      
      sample_data_frame <- rbind(sample_data_frame, cluster_data_frame)
      
      if (IS_WRITING_TO_LOGFILE) {
        cat("\tUK Content=" ,uk_proportion, "\tAccepted\n", file = output_logfile, append = T)
      }
    }
  }  
  
  # Read the next line and continue if readable
  line <- readLines(input_logfilehandle, n = 1)    
  if (length(line) == 0) {
    is_logfile_readable <- 0
  } 
}

close(input_logfilehandle)

if (grepl("csv$", output_filename)) {
  write.csv(sample_data_frame, file = output_filename, row.names=F)
} else {
  write.table(sample_data_frame, file = output_filename, row.names=F, sep="\t")
}
