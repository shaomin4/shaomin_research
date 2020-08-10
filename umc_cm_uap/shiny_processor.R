#!/usr/bin/env Rscript

# ----
# rzmq
library(rzmq)
context = init.context()
socket = init.socket(context,"ZMQ_REP")
bind.socket(socket,"tcp://*:5555")

while(1) {
    msg <- receive.socket(socket)
    service <- msg$service
    if (service == "copy_file") {
      from <- msg$from
      to <- msg$to
      user <- msg$user
      cp_cmd <- paste("cp -f", from, to, sep=" ")
      cp_result <- system(command = cp_cmd, intern = T)
      chown_cmd <- paste("chown", paste(user, user, sep=":"), to, sep=" ")
      system(chown_cmd, intern = F)
      send.socket(socket, cp_result)
    } else if (service == "put_hdfs_file") {
      from <- msg$from
      to <- msg$to
      user <- msg$user
      file_name <- strsplit(x = from, split = "/")[[1]]
      file_name <- file_name[length(file_name)]
      print(user); print(from); print(to)
      hdfs_put_cmd = paste("runuser -l hadoop -c '/home/hadoop/hadoop/bin/hdfs dfs -put", from, to,"2>&1","'")
      hdfs_put_result = system(hdfs_put_cmd, intern = T)
      hdfs_chown_cmd = paste("runuser -l hadoop -c '/home/hadoop/hadoop/bin/hdfs dfs -chown", user, paste(to, file_name, sep="/"), "'")
      hdfs_chown_result = system(hdfs_chown_cmd, intern = T)
      #check_result = system("echo $?", intern = T)
      check_result = hdfs_put_result  #Change by shaomin on 11/7
      send.socket(socket, check_result)
    } else if (service == "chown_hdfs_file") {
      file <- msg$file
      user <- msg$user
      print(user); print(file);
      hdfs_chown_cmd = paste("runuser -l hadoop -c '/home/hadoop/hadoop/bin/hdfs dfs -chown", user,file, "'")
      hdfs_chown_result = system(hdfs_chown_cmd, intern = T)
      check_result = system("echo $?", intern = T)
      send.socket(socket, check_result)
    } else if (service == "authenticate") {
      account <- msg$username
      password <- msg$passwd
      auth_cmd <- paste("/srv/shiny-server/my_pam.py", account, password)
      auth_result <- system(command = auth_cmd, intern = T)
      send.socket(socket, auth_result)
    } else if (service == "kill_process") {
      pid <- msg$pid
      username <- msg$username
      owner <- msg$process_owner
      kill_succ <- FALSE
      if (username == "root" || username == owner) {
        system(paste("runuser -l ", username, "-c 'kill ", pid, "'"), intern = F)
        kill_result <- system("echo $?", intern = T)
        if (kill_result == "0") {
          kill_succ <- TRUE
        }
      }
      send.socket(socket, kill_succ)
    } else if (service == "save_file") {
      url <- msg$url
      path <- msg$path
      user <- msg$user
      download.file(url = url, destfile = path)
      chown_cmd <- paste("chown", paste(user, user, sep=":"), path, sep=" ")
      download_result <- system(chown_cmd, intern = T)
      send.socket(socket, download_result)
    }else if (service == "del_hdfs_file") {
      file <- msg$file
      user <- msg$user
      print(user); print(file);
      hdfs_del_cmd = paste("runuser -l hadoop -c '/home/hadoop/hadoop/bin/hdfs dfs -rm -r",file,"2>&1","'")
      hdfs_del_result = system(hdfs_del_cmd, intern = T)
      #check_result = system("echo $?", intern = T)
      check_result = hdfs_del_result  #Change by shaomin on 11/7
      send.socket(socket, check_result)
    }else if (service == "rename_hdfs_file") {
      from <- msg$from
      to <- msg$to
      user <- msg$user
      hdfs_rename_cmd = paste("runuser -l hadoop -c '/home/hadoop/hadoop/bin/hdfs dfs -mv", from, to,"2>&1","'")
      hdfs_rename_result = system(hdfs_rename_cmd, intern = T)
      hdfs_chown_cmd = paste("runuser -l hadoop -c '/home/hadoop/hadoop/bin/hdfs dfs -chown", user, paste(to, sep="/"), "'")
      hdfs_chown_result = system(hdfs_chown_cmd, intern = T)
      #check_result = system("echo $?", intern = T)
      check_result = hdfs_rename_result  #Change by shaomin on 11/7
      send.socket(socket, check_result)
    }else if (service == "move_hdfs_file") {
      from <- msg$from
      to <- msg$to
      user <- msg$user
      file_name <- strsplit(x = from, split = "/")[[1]]
      file_name <- file_name[length(file_name)]
      hdfs_move_cmd = paste("runuser -l hadoop -c '/home/hadoop/hadoop/bin/hdfs dfs -mv", from, to,"2>&1","'")
      hdfs_move_result = system(hdfs_move_cmd, intern = T)
      hdfs_chown_cmd = paste("runuser -l hadoop -c '/home/hadoop/hadoop/bin/hdfs dfs -chown", user, paste(to, file_name, sep="/"), "'")
      hdfs_chown_result = system(hdfs_chown_cmd, intern = T)
      #check_result = system("echo $?", intern = T)
      check_result = hdfs_move_result  #Change by shaomin on 11/7
      send.socket(socket, check_result)
    } else if (service == "copy_hdfs_file") {
      from <- msg$from
      to <- msg$to
      user <- msg$user
      file_name <- strsplit(x = from, split = "/")[[1]]
      file_name <- file_name[length(file_name)]
      hdfs_copy_cmd = paste("runuser -l hadoop -c '/home/hadoop/hadoop/bin/hdfs dfs -cp", from, to,"2>&1","'")
      hdfs_copy_result = system(hdfs_copy_cmd, intern = T)
      hdfs_chown_cmd = paste("runuser -l hadoop -c '/home/hadoop/hadoop/bin/hdfs dfs -chown", user, paste(to, file_name, sep="/"), "'")
      hdfs_chown_result = system(hdfs_chown_cmd, intern = T)
      #check_result = system("echo $?", intern = T)
      check_result = hdfs_copy_result  #Change by shaomin on 11/7
      send.socket(socket, check_result)
    } else if (service == "mkdir_hdfs_file") {
      file <- msg$file
      user <- msg$user
      hdfs_mkdir_cmd = paste("runuser -l hadoop -c '/home/hadoop/hadoop/bin/hdfs dfs -mkdir",file,"2>&1","'")
      hdfs_mkdir_result = system(hdfs_mkdir_cmd, intern = T)
      hdfs_chown_cmd = paste("runuser -l hadoop -c '/home/hadoop/hadoop/bin/hdfs dfs -chown", user,file, "'")
      hdfs_chown_result = system(hdfs_chown_cmd, intern = T)
      #check_result = system("echo $?", intern = T)
      check_result = hdfs_mkdir_result  #Change by shaomin on 11/7
      send.socket(socket, check_result)
    }else {
      send.socket(socket, "error")
    }
    
}