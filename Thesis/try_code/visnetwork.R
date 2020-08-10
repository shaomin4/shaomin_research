source("R/Create_visNetwork.R")

topic_11 = topic_term

subT_11 = w[[1]] # subTopic with term
rownames(subT_11) =  colnames(train_data)
colnames(subT_11) = paste("st_", 1:ncol(subT_11), sep = "")

st_t = w[[2]] # subTopic to topic
colnames(st_t) = paste("topic", 1:ncol(st_t), sep = "")
rownames(st_t) =  paste("st_", 1:nrow(st_t), sep = "")

t_top10 = find_topic(topic_11,10)
st_top10 =find_topic(subT_11,10)
colnames(st_top10$term) = paste("st_", 1:ncol(st_top10$term), sep = "")
st_t_top3 = find_topic(st_t,3)




t_edge = edge_function(t_top10$term,t_top10$value) # 主題與詞之間的關係
st_edge = edge_function(st_top10$term,st_top10$value) # 子主題與詞的關係
st_t_edge = edge_function(st_t_top3$term,st_t_top3$value) # 主題與子主題間的關係
same_term = which(st_edge$from %in% t_edge$from)
select_subT = which(st_edge$to %in% st_t_edge$from)

#edge = rbind(t_edge,st_edge[same_term,])
edge = rbind(st_edge[select_subT,],st_t_edge)

node = node_function(edge)


visNetwork(node, edge, width = "100%", height = "700px") %>%
  visEvents(type = "once", startStabilizing = "function() {
            this.moveTo({scale:0.1})}") %>%
  visPhysics(stabilization = TRUE) %>%
  visNodes(color = list(background = "lightblue",
                        border = "darkblue",
                        highlight = "yellow"),
           shadow = list(enabled = TRUE, size = 10))  %>%
  visLayout(randomSeed = 12)
