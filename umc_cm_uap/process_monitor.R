process_monitor = function() {
  # Get status of system process, use head and tail to filter some unnecessary content
  process_stat = system("ps axo user:30,pid,pcpu,pmem,vsz,rss,tty,stat,time,comm", intern = T)
  process_stat1 = process_stat
  # Take away spaces of in the starting of rows
  # process_stat = sub(x = process_stat, pattern = "^\\s+", replacement = "")
  # Change mutliple spaces to one space
  process_stat = gsub(x = process_stat, pattern = "\\s+", replacement = " ")
  # Split one string row into multiple sting columns
  process_stat = strsplit(process_stat, split = " ")
  # Align process states which are different length
  process_stat = align_stat(process_stat)
  # Convert a list of vector to a list of one row matrix
  process_stat_mtx = Map(x = process_stat[-1], f = function(x) matrix(x, 1, length(x)))
  # Convert list of vectors to a data frame
  process_stat_df = as.data.frame(do.call(rbind, process_stat[-1]))
  # Convert data.frame columns from factor to characters
  process_stat_df[] <- lapply(process_stat_df, as.character)
  # Set column names
  colnames(process_stat_df) = process_stat[[1]]
  # Process time column
  exec_more_than_24_hours_idx = which(grepl(pattern = "[0-9]*-.*", x =  process_stat_df$`TIME`))
  setdiff(seq_len(nrow(process_stat_df)), exec_more_than_24_hours_idx)
  
  process_stat_df$HOURS = sapply( X = process_stat_df$`TIME`,
                                  FUN = function(x) {
                                    # Init days
                                    days = 0
                                    # If contain days format in times
                                    if(grepl(pattern = "[0-9]*-.*", x = x)) {
                                      splitted_time = strsplit(x = x, split = "-")[[1]]
                                      days_str = splitted_time[1]
                                      # Update format of times
                                      x = splitted_time[2]
                                      days = as.integer(days_str)
                                    } 
                                    splitted_time = strsplit(x = x, split = ":")[[1]]
                                    hours_str = splitted_time[1]
                                    hours = as.integer(hours_str) + days*24
                                    return(hours)
                                  })
  return(process_stat_df)
}

align_stat = function(stat) {
  # Get lenght of state
  stat_len = sapply(X = stat, FUN = length)
  # Get minimum of them
  min_len = min(stat_len)
  # Find index which length of state larger than minimum
  need_2_be_aligned_idx = which(stat_len > min_len)
  for(i in need_2_be_aligned_idx) {
    # Get excess index
    excess_idx = setdiff(x = seq_len(stat_len[i]),
                         y = seq_len(min_len))
    # Combind excess columns to new one string, and then combine with this string
    stat[[i]][min_len] = paste(stat[[i]][min_len], 
                               paste(stat[[i]][excess_idx], collapse = " "), 
                               sep = " ")
    # Take unnecessary columns away
    stat[[i]] = stat[[i]][seq_len(min_len)]
  }
  return(stat)
}
