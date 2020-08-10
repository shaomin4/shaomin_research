# Usage of this R script (idle_process_auto_kill_script.R)
#   $ Rscript ./idle_process_auto_kill_script.R [CPU|GPU] usage_limit[MB|GB] time_limit[HH:MM:SS] cpu_threshold[%]
# Following script executions are typed in the terminal.
# 1. kill PID which using CPU more than 3 hours, allocating 10GB memory and having lower CPU usage than 0.1%
#   $ Rscript ./idle_process_auto_kill_script.R CPU 10GB 03:00:00 0.1%
# 2. kill PID which using GPU more than 3 hours, allocating 1GB GPU memory and having lower CPU usage than 0.1%
#   $ Rscript ./idle_process_auto_kill_script.R GPU 1GB 03:00:00 0.1%       

source("process_monitor.R")
source("gpu_process_monitor.R")

# White list of killing process
WHITE_LIST <- c("root", "rstudio-server")

# Get arguments
args <- commandArgs(trailingOnly = TRUE)
# Check length of arguments
if(length(args) != 4) {
  stop("Error: wrong number of arguments")
} else {
  check_type  <- args[[1]]
  usage_limit <- args[[2]]
  time_limit  <- args[[3]]
  cpu_threshold <- args[[4]]
  
  # Check format of argument check_type
  if(!(check_type %in% c("CPU", "GPU"))) {
    stop(paste0("Error: 1st argument '", check_type, "' is in the wrong check type, should be CPU or GPU."))
  } 
  
  # Check format of argument usage_limit
  if(!grepl(pattern = "[0-9.]+[GgMm][Bb]", x = usage_limit)) {
    stop(paste0("Error: 2nd argument '", usage_limit, "' is in wrong format, should be like 1024MB or 10GB (capital insensitive)."))
  } else {
    # Get number of usage limit
    usage_limit_number <- gsub(pattern = "([0-9.]+).*", replacement = "\\1", x = usage_limit)
    usage_limit_number <- tryCatch({
      as.double(usage_limit_number)
    }, .warning = function(w) {
      stop(w)
    }, .error = function(e) {
      stop(e)
    })
    if(grepl(pattern = "[Gg][Bb]", x = usage_limit)) {
      # In Gigabyte
      usage_limit_number <- usage_limit_number * (1024)^2
    } else if(grepl(pattern = "[Mm][Bb]", x = usage_limit)) {
      # In Megabyte
      usage_limit_number <- usage_limit_number * (1024)^1
    }
  }
  
  # Check format of argument time_limit
  if(!grepl(pattern = "[0-9]+:[0-9]{2}:[0-9]{2}", x = time_limit)) {
    stop(paste0("Error: 3rd argument '", time_limit, "' is in wrong format, should be HH:MM:SS, for example, 13:00:00 means 13 hours."))
  } else {
    time_frame <- strsplit(time_limit, split = ":")[[1]]
    time_frame <- sapply(X = time_frame, FUN = as.integer)
    hour   <- time_frame[1]
    minute <- time_frame[2]
    second <- time_frame[3]
    # Now, the format of time_limit is transformed into second only
    time_limit <- as.integer(lubridate::duration(hour = hour, minute = minute, second = second))
    if(time_limit <= 0) {
      stop("Error: 3rd argument is the negative time duration.")
    }
  }
  
  # Check format of argument cpu_threshold
  if(!grepl(pattern = "[0-9.]+", x = cpu_threshold)) {
    stop("Error: 4th argument is about the lowest CPU threshold where a process would be seem as idle if lower than.")
  } else {
    cpu_threshold_number <- gsub(pattern = "([0-9.]+).*", replacement = "\\1", x = cpu_threshold)
    cpu_threshold_number <- as.double(cpu_threshold_number)
    if(!grepl(pattern = "%", x = cpu_threshold)) {
      cpu_threshold_number <- cpu_threshold_number * 100
    }
    if(cpu_threshold_number > 100) {
      stop(paste0("Error: 4th argument '", cpu_threshold, "' cannot be larger than 100.0%."))
    }
  }
}

# Get the process table
process_table <- process_monitor()
process_table$`%CPU` <- as.double(process_table$`%CPU`)
# Caculate the execute time of each process
process_table$seconds <- sapply(X = strsplit(process_table$TIME, split = ":"),
                                FUN = function(time_frame) {
                                  time_frame <- sapply(X = time_frame, FUN = as.integer)
                                  hour   <- time_frame[1]
                                  minute <- time_frame[2]
                                  second <- time_frame[3]
                                  return(as.integer(lubridate::duration(hour = hour, minute = minute, second = second)))
                                })

if(check_type == "CPU") {
  have2kill_idx <- which(process_table$VSZ > usage_limit_number &
                         process_table$seconds > time_limit &
                         process_table$`%CPU` <= cpu_threshold_number &
                         !(process_table$USER %in% WHITE_LIST))
  have2kill_pid <- process_table[have2kill_idx,]$PID
  have2kill_pid <- as.integer(have2kill_pid)
} else if(check_type == "GPU") {
  gpu_process_table <- gpu_process_monitor()
  gpu_process_table <- gpu_process_table$process_df
  if(ncol(gpu_process_table) == 1) {
    have2kill_idx <- NULL
  } else {
    gpu_process_table$Usage <- as.character(gpu_process_table$Usage)
    gpu_process_table$Usage <- sapply(X = gpu_process_table$Usage,
                                      FUN = function(usage) {
                                        usage_limit_number <- gsub(pattern = "([0-9.]+).*", replacement = "\\1", x = usage) 
                                        usage_limit_number <- as.double(usage_limit_number)
                                        if(grepl(pattern = "GiB", x = usage)) {
                                          # In Gigabyte
                                          usage_limit_number <- usage_limit_number * (1024)^2
                                        } else if(grepl(pattern = "MiB", x = usage)) {
                                          # In Megabyte
                                          usage_limit_number <- usage_limit_number * (1024)^1
                                        }
                                        return(usage_limit_number)
                                      })
    gpu_process_table$seconds <- sapply(X = gpu_process_table$PID, 
                                        FUN = function(pid) {
                                          process_table$seconds[which(pid == process_table$PID)]
                                        })
    gpu_process_table$`%CPU` <- sapply(X = gpu_process_table$PID, 
                                        FUN = function(pid) {
                                          process_table$`%CPU`[which(pid == process_table$PID)]
                                        })
    have2kill_idx <- which(gpu_process_table$Usage > usage_limit_number &
                           gpu_process_table$seconds > time_limit &
                           gpu_process_table$`%CPU` <= cpu_threshold_number &
                           !(gpu_process_table$USER %in% WHITE_LIST))
    have2kill_pid <- gpu_process_table[have2kill_idx, ]$PID
    have2kill_pid <- as.integer(as.character(have2kill_pid))
  }
}

# Return PID which have to be killed
have2kill_pid