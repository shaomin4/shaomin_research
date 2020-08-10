library(rzmq)
# Need to run server.R in server-side

authenticate = function(username, passwd) {
  context <- init.context()
  socket <- init.socket(context,"ZMQ_REQ")
  connect.socket(socket,"tcp://localhost:5555")
  send.socket(socket, data=list(service = "authenticate", username=username,passwd=passwd))
  auth_result <- receive.socket(socket)
  auth_result <- eval(parse(text = toupper(auth_result)))
  return(auth_result)
}
