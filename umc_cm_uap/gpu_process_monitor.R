gpu_process_monitor = function() {
  # Set PATH to original setting
  if(Sys.getenv("PATH") != "$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin"){
    Sys.setenv(PATH = "$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin")
  }
  
  # Get status from GPU
  cmd = system("nvidia-smi", intern = T)
  
  # Get the information of NVIDIA-smi and driver
  gpu_info = gsub(pattern = "[|]", replacement = " ", cmd[3])
  gpu_info = stat_split(gpu_info)
  
  # Get 5, 6th row of command as the colname of the status data frame
  gpu_stat_colnames = stat_split(c(cmd[5], cmd[6]))
  
  # Get 8, 9, 11, 12th row of command as the data of the status data frame
  gpu_stat = grep(pattern = "^\\+-+\\+-+\\+-+\\+", x = cmd)
  stat = c()
  for (i in gpu_stat) {
    stat = c(stat, cmd[i - 2], cmd[i - 1])
  }
  gpu_stat = stat_split(stat)
  # The 4, 16th element are special case that can only be split by one space
  bus_id = grep(pattern = "^[[:digit:]]+:[[:digit:]]+:[[:digit:]]+\\.[[:digit:]]",
                x = gpu_stat)
  gpu_stat_split = lapply(gpu_stat[bus_id], function(x){
    stat = unlist(strsplit(x, split = "[ ]"))
    stat = unname(stat)
    return(stat)
  })
  
  # Reshape the status vector
  gpu_stat_bind = lapply(1:length(bus_id), function(i){
    return(c(gpu_stat[(1 + 12 * (i - 1)):(bus_id[i] - 1)], gpu_stat_split[[i]], gpu_stat[(bus_id[i] + 1):(12 * i)]))
  })
  gpu_stat = do.call(rbind, gpu_stat_bind)
  # Convert the status vector to a data frame
  gpu_stat = as.data.frame(gpu_stat)
  # Assign gpu_stat_colnames as the colname of status data frame
  colnames(gpu_stat) = gpu_stat_colnames
  
  # Get 17th row of command as the colname of the process data frame
  
  process_df_colnames = stat_split(cmd[grep(pattern = "Processes:", x = cmd) + 1])
  
  # Get 19th ~ last row of command as the data of the process data frame
  process_df = stat_split(cmd[(grep(pattern = "Processes:", x = cmd) + 3):(length(cmd) - 1)])
  process_df = as.data.frame(matrix(process_df, ncol = 5, byrow = T))
  if(process_df[1, 1] == "No running processes found"){
    process_df = as.data.frame(process_df[1, 1])
    colnames(process_df) = "Processes"
  }else{
    colnames(process_df) = process_df_colnames
    # Merge the process user and pid
    process_user = system("ps axo user:30,pid", intern = T)
    process_user_df = lapply(process_user, function(x){
      return(unlist(strsplit(x, split = "[ ]+")))
    })
    process_user_df_colnames = process_user_df[[1]]
    process_user_df = t(as.data.frame(process_user_df[2:length(process_user_df)]))
    colnames(process_user_df) = process_user_df_colnames
    process_df = merge(process_df, process_user_df, by = "PID")
    process_df = process_df[, c("GPU", "USER", "PID", "Type", "Process name", "Usage")]
  }
  
  return(list("gpu_info" = gpu_info, "gpu_stat_df" = gpu_stat,
              "process_df" = process_df))
}

stat_split = function(stat){
  # Split the string vector into slices
  stat = unlist(strsplit(stat, split = "[ |]{2,}"))
  # Remove empty string elements
  stat = stat[-which(stat == "")]
  return(stat)
}
