source("R/Create_visNetwork.R")
source("R/find_topic.R")

# Select column by topic in different year
get_topic <- function(topic_evo,pathOfTopic){
  get_year_df <-data.frame()
  for(i in 1:length(topic_evo)){
    print(i)
    topic = topic_evo[[i]]$topic_term[,pathOfTopic]
    if(length(get_year_df) == 0){
      get_year_df = topic
    }else{
      get_year_df = cbind(get_year_df,topic)
    }
  }
  return(get_year_df)
}

# Set up the topic 
choose_topic = 6

test = get_topic(topic_evo,choose_topic)
# Get top 10 term in each topic of year.
t_top10 = find_topic(test,10)
colnames(t_top10$term) = paste(2010+1:ncol(test),"_","topic_",choose_topic, sep = "")
colnames(t_top10$value) = paste(2010+1:ncol(test),"_","topic_",choose_topic, sep = "")


edge = edge_function(t_top10$term,t_top10$value) # 主題與詞之間的關係

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

# plot(g,label=network.vertex.names(g), interactive=T)
