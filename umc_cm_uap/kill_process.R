library(rzmq)

kill_process <- function(pid, username, process_owner) {
  # Need to run /root/Download/kill_process_server.R in server-side
  context <- init.context()
  socket <- init.socket(context, "ZMQ_REQ")
  connect.socket(socket,"tcp://localhost:5555")
  send.socket(socket, data=list(service = "kill_process", pid=pid, username=username, process_owner=process_owner))
  kill_result <- receive.socket(socket)
  return(kill_result)
}