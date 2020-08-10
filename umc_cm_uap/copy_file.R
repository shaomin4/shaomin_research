library(rzmq)

copy_file <- function(from, to, user) {
  # Need to run /root/Download/copy_file_server.R in server-side
  context <- init.context()
  socket <- init.socket(context, "ZMQ_REQ")
  connect.socket(socket,"tcp://localhost:5555")
  send.socket(socket, data=list(service = "copy_file", from = from, to = to, user = user))
  copy_result <- receive.socket(socket)
  return(copy_result)
}


put_hdfs_file <- function(from, to, user) {
  # Need to run /root/Download/copy_file_server.R in server-side
  context <- init.context()
  socket <- init.socket(context, "ZMQ_REQ")
  connect.socket(socket,"tcp://localhost:5555")
  send.socket(socket, data=list(service = "put_hdfs_file", from = from, to = to, user = user))
  copy_result <- receive.socket(socket)
  return(copy_result)
}


chown_hdfs_file <- function(file, user) {
  # Need to run /root/Download/copy_file_server.R in server-side
  context <- init.context()
  socket <- init.socket(context, "ZMQ_REQ")
  connect.socket(socket,"tcp://localhost:5555")
  send.socket(socket, data=list(service = "chown_hdfs_file", file = file, user = user))
  copy_result <- receive.socket(socket)
  return(copy_result)
}

#Add by shaomin on 11/7
del_hdfs_file <- function(file, user){
  # Need to run /root/Download/copy_file_server.R in server-side
  context <- init.context()
  socket <- init.socket(context, "ZMQ_REQ")
  connect.socket(socket,"tcp://localhost:5555")
  send.socket(socket, data=list(service = "del_hdfs_file", file = file, user = user))
  del_result <- receive.socket(socket)
  return(del_result)
}
rename_hdfs_file <- function(from, to, user){
  # Need to run /root/Download/copy_file_server.R in server-side
  context <- init.context()
  socket <- init.socket(context, "ZMQ_REQ")
  connect.socket(socket,"tcp://localhost:5555")
  send.socket(socket, data=list(service = "rename_hdfs_file", from = from, to = to, user = user))
  del_result <- receive.socket(socket)
  return(del_result)
}
move_hdfs_file <- function(from, to, user){
  # Need to run /root/Download/copy_file_server.R in server-side
  context <- init.context()
  socket <- init.socket(context, "ZMQ_REQ")
  connect.socket(socket,"tcp://localhost:5555")
  send.socket(socket, data=list(service = "move_hdfs_file", from = from, to = to, user = user))
  del_result <- receive.socket(socket)
  return(del_result)
}
copy_hdfs_file <- function(from, to, user){
  # Need to run /root/Download/copy_file_server.R in server-side
  context <- init.context()
  socket <- init.socket(context, "ZMQ_REQ")
  connect.socket(socket,"tcp://localhost:5555")
  send.socket(socket, data=list(service = "copy_hdfs_file", from = from, to = to, user = user))
  del_result <- receive.socket(socket)
  return(del_result)
}
mkdir_hdfs_file <- function(file, user){
  # Need to run /root/Download/copy_file_server.R in server-side
  context <- init.context()
  socket <- init.socket(context, "ZMQ_REQ")
  connect.socket(socket,"tcp://localhost:5555")
  send.socket(socket, data=list(service = "mkdir_hdfs_file", file = file, user = user))
  del_result <- receive.socket(socket)
  return(del_result)
}
