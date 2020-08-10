# Vis ----------------------------------------------------------
source("R/termGJSD.R")
load("NAE11~19/topic_evo.RData")
topic_terms <- lapply(topic_evo, function (x) {
  x$h_head2
})

# Check term in dictionary ----
rownames(topic_terms[[1]])[grep(input_term, rownames(topic_terms[[1]]))]

# Narrow terms --------
input_term <- "cluster"
tt_evol<- monitorTerm(term = input_term, periods = c("2011","2013","2015","2017","2019"),
                      numOfPeriods = 5, numOfTopics = 10, p_term_topic_list = topic_terms)

GJSDs <- data.frame(year = c(2011,2013,2015,2017,2019), JSD = c(NA, tt_evol$GJSD), JSD_kt = c(NA, tt_evol$GJSD_kt))
ggplot(GJSDs, aes(year)) +
  geom_line(aes(year, JSD), color = "black") +
  geom_point(aes(year, JSD), shape=15, size = 5, color ="black") +
  geom_line(aes(year, JSD_kt), color = "red") +
  geom_point(aes(year, JSD_kt), color = "red", shape = 19, size = 5) +
  annotate("text", parse = TRUE, x = 2018.8, y=0.04, label= "italic(D[GJS])", size = 5, color = "black") +
  annotate("text", parse = TRUE, x = 2018.5, y=0.21, label= "italic(D[GJS [kt]])", size = 5, color = "red") +
  theme_bw(base_size = 15) +
  scale_x_continuous(name ="Year", breaks = c(2011,2013,2015,2017,2019)) +
  ylim(-.01, .4) +
  theme(text = element_text(size = 20))


# Broad terms --------
input_term <- "statistical learning theory"
tt_evol<- monitorTerm(term = input_term, periods = c("2011","2013","2015","2017","2019"),
                      numOfPeriods = 5, numOfTopics = 10, p_term_topic_list = topic_terms)

GJSDs <- data.frame(year = c(2011,2013,2015,2017,2019), JSD = c(NA, tt_evol$GJSD), JSD_kt = c(NA, tt_evol$GJSD_kt))
ggplot(GJSDs, aes(year)) +
  geom_line(aes(year, JSD), color = "black") +
  geom_point(aes(year, JSD), shape=15, size = 5, color ="black") +
  geom_line(aes(year, JSD_kt), color = "red") +
  geom_point(aes(year, JSD_kt), color = "red", shape = 19, size = 5) +
  annotate("text", parse = TRUE, x = 2018.8, y=0.48, label= "italic(D[GJS])", size = 5, color = "black") +
  annotate("text", parse = TRUE, x = 2018.5, y=0.21, label= "italic(D[GJS [kt]])", size = 5, color = "red") +
  theme_bw(base_size = 15) +
  scale_x_continuous(name ="Year", breaks = c(2011,2013,2015,2017,2019)) +
  ylim(-.01, .8) +
  theme(text = element_text(size = 20))


input_term <- "nonnegative matrix factorization"
tt_evol<- monitorTerm(term = input_term, periods = c("2011","2013","2015","2017","2019"),
                      numOfPeriods = 5, numOfTopics = 10, p_term_topic_list = topic_terms)

GJSDs <- data.frame(year = c(2011,2013,2015,2017,2019), JSD = c(NA, tt_evol$GJSD), JSD_kt = c(NA, tt_evol$GJSD_kt))
ggplot(GJSDs, aes(year)) +
  geom_line(aes(year, JSD), color = "black") +
  geom_point(aes(year, JSD), shape=15, size = 5, color ="black") +
  geom_line(aes(year, JSD_kt), color = "red") +
  geom_point(aes(year, JSD_kt), color = "red", shape = 19, size = 5) +
  annotate("text", parse = TRUE, x = 2018.8, y=0.31, label= "italic(D[GJS])", size = 5, color = "black") +
  annotate("text", parse = TRUE, x = 2018.5, y=0.21, label= "italic(D[GJS [kt]])", size = 5, color = "red") +
  theme_bw(base_size = 15) +
  scale_x_continuous(name ="Year", breaks = c(2011,2013,2015,2017,2019)) +
  ylim(-.01, .5) +
  theme(text = element_text(size = 20))


# convergent terms  ----
input_term <- "dropout"
tt_evol<- monitorTerm(term = input_term, periods = c("2011","2013","2015","2017","2019"),
                      numOfPeriods = 5, numOfTopics = 10, p_term_topic_list = topic_terms)

GJSDs <- data.frame(year = c(2011,2013,2015,2017,2019), JSD = c(NA, tt_evol$GJSD), JSD_kt = c(NA, tt_evol$GJSD_kt))
ggplot(GJSDs, aes(year)) +
  geom_line(aes(year, JSD), color = "black") +
  geom_point(aes(year, JSD), shape=15, size = 5, color ="black") +
  geom_line(aes(year, JSD_kt), color = "red") +
  geom_point(aes(year, JSD_kt), color = "red", shape = 19, size = 5) +
  annotate("text", parse = TRUE, x = 2018.8, y=0.55, label= "italic(D[GJS])", size = 5, color = "black") +
  annotate("text", parse = TRUE, x = 2018.5, y=0.31, label= "italic(D[GJS [kt]])", size = 5, color = "red") +
  theme_bw(base_size = 15) +
  scale_x_continuous(name ="Year", breaks = c(2011,2013,2015,2017,2019)) +
  ylim(-.01, .65) +
  theme(text = element_text(size = 20))



input_term <- "deep autoencoder"
tt_evol<- monitorTerm(term = input_term, periods = c("2011","2013","2015","2017","2019"),
                      numOfPeriods = 5, numOfTopics = 10, p_term_topic_list = topic_terms)

GJSDs <- data.frame(year = c(2011,2013,2015,2017,2019), JSD = c(NA, tt_evol$GJSD), JSD_kt = c(NA, tt_evol$GJSD_kt))
ggplot(GJSDs, aes(year)) +
  geom_line(aes(year, JSD), color = "black") +
  geom_point(aes(year, JSD), shape=15, size = 5, color ="black") +
  geom_line(aes(year, JSD_kt), color = "red") +
  geom_point(aes(year, JSD_kt), color = "red", shape = 19, size = 5) +
  annotate("text", parse = TRUE, x = 2018.8, y=0.55, label= "italic(D[GJS])", size = 5, color = "black") +
  annotate("text", parse = TRUE, x = 2018.5, y=0.31, label= "italic(D[GJS [kt]])", size = 5, color = "red") +
  theme_bw(base_size = 15) +
  scale_x_continuous(name ="Year", breaks = c(2011,2013,2015,2017,2019)) +
  ylim(-.01, .65) +
  theme(text = element_text(size = 20))


# divergent term ------
input_term <- "hidden semimarkov model"
tt_evol<- monitorTerm(term = input_term, periods = c("2011","2013","2015","2017","2019"),
                      numOfPeriods = 5, numOfTopics = 10, p_term_topic_list = topic_terms)

GJSDs <- data.frame(year = c(2011,2013,2015,2017,2019), JSD = c(NA, tt_evol$GJSD), JSD_kt = c(NA, tt_evol$GJSD_kt))
ggplot(GJSDs, aes(year)) +
  geom_line(aes(year, JSD), color = "black") +
  geom_point(aes(year, JSD), shape=15, size = 5, color ="black") +
  geom_line(aes(year, JSD_kt), color = "red") +
  geom_point(aes(year, JSD_kt), color = "red", shape = 19, size = 5) +
  annotate("text", parse = TRUE, x = 2018.8, y=0.15, label= "italic(D[GJS])", size = 5, color = "black") +
  annotate("text", parse = TRUE, x = 2018.5, y=0.29, label= "italic(D[GJS [kt]])", size = 5, color = "red") +
  theme_bw(base_size = 15) +
  scale_x_continuous(name ="Year", breaks = c(2011,2013,2015,2017,2019)) +
  ylim(-.01, .4) +
  theme(text = element_text(size = 20))


input_term <- "latent dirichlet allocation"
tt_evol<- monitorTerm(term = input_term, periods = c("2011","2013","2015","2017","2019"),
                      numOfPeriods = 5, numOfTopics = 10, p_term_topic_list = topic_terms)

GJSDs <- data.frame(year = c(2011,2013,2015,2017,2019), JSD = c(NA, tt_evol$GJSD), JSD_kt = c(NA, tt_evol$GJSD_kt))
ggplot(GJSDs, aes(year)) +
  geom_line(aes(year, JSD), color = "black") +
  geom_point(aes(year, JSD), shape=15, size = 5, color ="black") +
  geom_line(aes(year, JSD_kt), color = "red") +
  geom_point(aes(year, JSD_kt), color = "red", shape = 19, size = 5) +
  annotate("text", parse = TRUE, x = 2018.8, y=0.19, label= "italic(D[GJS])", size = 5, color = "black") +
  annotate("text", parse = TRUE, x = 2018.5, y=0.27, label= "italic(D[GJS [kt]])", size = 5, color = "red") +
  theme_bw(base_size = 15) +
  scale_x_continuous(name ="Year", breaks = c(2011,2013,2015,2017,2019)) +
  ylim(-.01, .4) +
  theme(text = element_text(size = 20))