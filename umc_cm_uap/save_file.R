library(rzmq)

save_file = function(url, user, path) {
  # Need to run /root/Download/copy_file_server.R in server-side
  context <- init.context()
  socket <- init.socket(context, "ZMQ_REQ")
  connect.socket(socket,"tcp://localhost:5555")
  send.socket(socket, data=list(service = "save_file", url = url, user = user, path = path))
  save_result <- receive.socket(socket)
  return(save_result)
}
