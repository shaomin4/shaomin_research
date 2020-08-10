subtop10_terms_year <- lapply(seq_along(topic_evo), function(i) {
  subtop10_terms <- find_topic(topic_evo[[i]]$subtopic_term, 10)$term
})

subtop10_terms_year <- lapply(seq(10), function(x) {
  each_topic <- sapply(subtop10_terms_year, function(y) {
    paste0(y[, x], collapse = ",\n")
  })
  return(each_topic)
})

subtop10_terms <- lapply(topic_evo, function(x) {
  subtop10_terms <- find_topic(x$subtopic_term, 10)$term
  subtop10_terms <- apply(subtop10_terms, 2, function(x) paste0(x, collapse = ",\n"))
})


subtop10_terms <- do.call(what = cbind, args = subtop10_terms)
colnames(subtop10_terms) <- 2015:2019
#write.csv(subtop10_terms, file = "experiment/NAE/NAE_subtopic1530.csv")
View(subtop10_terms)


subtopic_topic <- lapply(seq_along(topic_evo), function(i) {
  subtopic_topic <- find_topic(topic_evo[[i]]$subtopic_weight, 10)$term
})

subtopic_topic <- lapply(seq(5), function(x) {
  each_one <- sapply(subtopic_topic, function(y) {
    paste0(y[, x], collapse = ",\n")
  })
  return(each_one)
})

subtopic_topic <- lapply(topic_evo, function(x) {
  subtopic_topic <- find_topic(x$subtopic_weight, 3)$term
  subtopic_topic <- apply(subtopic_topic, 2, function(x) paste0(x, collapse = ",\n"))
})
subtopic_topic <- do.call(what = cbind, args = subtopic_topic)
colnames(subtopic_topic) <- 2015:2019
View(subtopic_topic)
