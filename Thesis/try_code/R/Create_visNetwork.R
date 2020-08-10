# visNetwork
library(visNetwork)

# Function to create edge.
edge_function <- function(topic_term,topic_value){
  edges_DF = data.frame()
  for(i in 1:ncol(topic_term)){
    for(j in 1:nrow(topic_term)){
      if(j ==1 ){
        edge_tmp <- data.frame(from = topic_term[j,i], to = colnames(topic_term)[i],value= topic_value[j,i])
      }else{
        edge_tmp <- rbind(edge_tmp,data.frame(from = topic_term[j,i], to = colnames(topic_term)[i],value= topic_value[j,i]))
      }
    }
    edges_DF <- rbind(edge_tmp,edges_DF)
  }
  return(edges_DF)
}


node_function <- function(edge){
  nodeid = unique(edge$from)
  nodes_term <- data.frame(id = nodeid, shape = c("circle"), label = nodeid, color.background = c("lightblue"),value = 1)
  
  topicid = unique(edge$to)
  nodes_topic <- data.frame(id = topicid, shape = c("box"), label = topicid, color.background = c("yellow"),value = 1)
  
  nodes_term <- rbind(nodes_term,nodes_topic)
}


# edge_df <- edge_function(layer_1_topic_10[[1]],layer_1_topic_10[[2]])
# nodes_term = node_function(edge_df)
# 
# 
# visNetwork(nodes_term, edge_df, width = "100%", height = "700px") %>% 
#   visEvents(type = "once", startStabilizing = "function() {
#             this.moveTo({scale:0.1})}") %>%
#   visPhysics(stabilization = TRUE) %>%
#   visNodes(color = list(background = "lightblue", 
#                         border = "darkblue",
#                         highlight = "yellow"),
#            shadow = list(enabled = TRUE, size = 10))  %>%
#   visLayout(randomSeed = 12)


