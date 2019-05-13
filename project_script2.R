#For the opioid state
opioid_state <- c(" Ohio", " West Virginia", " Kentucky", " New Hampshire", " Maryland", 
                  " District of Columbia", " Connecticut", " Massachusetts", " Delaware"," Maine")

least_affected <- c(" Nebraska", " Hawaii", " Montana", " Texas", " Kansas", " California", " South Dakota",
                    " North Dakota", " Idaho"," Mississippi" )
#-----------
## Topic Model Analysis
#----------------

#Economic. trump, employ, industry
state_data$date <- as.Date(state_data$date)
state_ordered <- state_data[(order(state_data$date)),]

opioid_data <- state_ordered[(state_ordered$state %in% opioid_state),]

opioid_dfm <- dfm(opioid_data$contents, tolower = TRUE, remove_punct = TRUE, 
                  remove_numbers = TRUE, stem = TRUE, remove = stopwords('english'))

opioid_dfm_trimmed <- dfm_trim(opioid_dfm, min_termfreq = 5)

dim(opioid_dfm_trimmed)

k_optimize_blm <- FindTopicsNumber(
  opioid_dfm_trimmed,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 2017),
  mc.cores = detectCores(), # to usa all cores available
  verbose = TRUE
)
# Increase the topics, untill the point adding a point nothing betters
FindTopicsNumber_plot(k_optimize_blm)


opioid_tm <- LDA(opioid_dfm_trimmed, k = 36, method = "Gibbs", control = list(iter = 3000,seed = 10012))

# Extract the keywords for the most popular ones
topic_terms <- get_terms(opioid_tm, k=10)
print(topic_terms)
topic_terms20 <- get_terms(opioid_tm, k=20)
print(topic_terms20)
#Find the most likely topic for each document
#typeof(topics(opioid_tm))
opioid_data$lda <- topics(opioid_tm)

#beta topic: word distribution
opioid_topics <- tidy(opioid_tm, matrix = "beta") 
head(opioid_topics)


#Generate popular words for each topic
opioid_top_terms <- opioid_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

head(opioid_top_terms)

# Creates a plot of the weights and terms by topic
opioid_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# Creates a plot of features with greatest difference in word probabilities between two topics
opioid_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  filter(topic %in% c("topic1", "topic2")) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  arrange(-abs(log_ratio)) %>%
  slice(c(1:10,(nrow(.)-9):nrow(.))) %>%
  arrange(-log_ratio) %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  ggplot(aes(as.factor(term), log_ratio)) +
  geom_col(show.legend = FALSE) +
  xlab("Terms") + ylab("Log-Ratio") +
  coord_flip()


##document topic distribution gamma:
opioid_doc_topics <- opioid_tm@gamma

# Store the results of words over topics
#words_topics <- blm_tm@beta
# Only interested in topics
# Transpose the data so that the days are columns
opioid_doc_topics <- t(opioid_doc_topics) # transpose
dim(opioid_doc_topics)
opioid_doc_topics[1:5,1:5]

# Arrange topics
# Find the top topic per column (day)
max <- apply(opioid_doc_topics, 2, which.max)

# Write a function that finds the second max
which.max2 <- function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}

max2 <- apply(opioid_doc_topics, 2, which.max2)
max2 <- sapply(max2, max)

# Coding police shooting events
victim <- c("Freddie Gray", "Sandra Bland")
shootings <- mdy(c("04/12/2015","7/13/2015"))

# Combine data
top2 <- data.frame(top_topic = max, second_topic = max2, date = ymd(opioid_data$date))

# Plot
opioid_plot <- ggplot(top2, aes(x=date, y=top_topic, pch="First")) 

opioid_plot + geom_point(aes(x=date, y=second_topic, pch="Second") ) +theme_bw() + 
  ylab("Topic Number") + ggtitle("Top Opioid Affected States Topics") + geom_point() + xlab(NULL) + 
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 



#----------
## Expressivity with the titles
#------

#average_intersection = double(0)
##for (i in 1:30){
##  top_words <- topic_terms[,i]
#  topic_data <- opioid[opioid$lda == i,]
  
#  topic_dfm <- dfm(topic_data$title, tolower = T, remove_punct = T,remove_numbers = T, 
#                   stem = T, remove = stopwords('english'))
#  top_words_dfm <- dfm_select(topic_dfm, pattern = top_words)
#  average_intersection[i] <- sum(rowSums(top_words_dfm)/rowSums(topic_dfm))/(nrow(topic_data))
  
#}
#names(average_intersection) <- c(1:30)

average_intersection20 = double(0)
for (i in 1:36){
  top_words <- topic_terms20[,i]
  topic_data <- opioid_data[opioid_data$lda == i,]
  
  topic_dfm <- dfm(topic_data$title, tolower = T, remove_punct = T,remove_numbers = T, 
                   stem = T, remove = stopwords('english'))
  top_words_dfm <- dfm_select(topic_dfm, pattern = top_words)
  average_intersection20[i] <- sum(rowSums(top_words_dfm)/rowSums(topic_dfm))/(nrow(topic_data))
  
}

#Find the most popular 5 topics by ranking the topic
popular_topics <- sort(table(topics(opioid_tm)), decreasing = TRUE)[1:5]
popular_topics_idx <- names(popular_topics)
print(popular_topics)

#Topic model changing over time
for (i in 7:length(year_list)){
  subset_data <- opioid_data[opioid_data$year == year_list[i],]
  subset_dfm <- dfm(subset_data$contents, tolower = TRUE, remove_punct = TRUE, 
                    remove_numbers = TRUE, remove = c(stopwords('english'), 'attorney'))
  #subset_dfm_trimmed <- dfm_trim(subset_dfm, min_termfreq = 20, min_docfreq = 10)
  rowTotals <- apply(subset_dfm , 1, sum) 
  subset_dfm_trimmed <- subset_dfm[rowTotals> 0, ] 
  subset_tm <- LDA(subset_dfm_trimmed, k = 10, method = "Gibbs", control = list(iter = 3000,seed = 10012))
  
  topic_terms <- get_terms(subset_tm, k=15)
  
  popular_topics <- sort(table(topics(subset_tm)), decreasing = TRUE)[1:5]
  popular_idx <- as.integer(names(popular_topics))
  topic_ordered <- topic_terms[, popular_idx]
  print(topic_ordered)
}











least_affected <- c(" Nebraska", " Hawaii", " Montana", " Texas", " Kansas", " California", " South Dakota",
                    " North Dakota", " Idaho"," Mississippi" )
#-----------
## Topic Model Analysis
#----------------

#Economic. trump, employ, industry
state_data$date <- as.Date(state_data$date)
state_ordered <- state_data[(order(state_data$date)),]

opioid_data <- state_ordered[(state_ordered$state %in% least_affected),]

opioid_dfm <- dfm(opioid_data$contents, tolower = TRUE, remove_punct = TRUE, 
                  remove_numbers = TRUE, stem = TRUE, remove = stopwords('english'))

opioid_dfm_trimmed <- dfm_trim(opioid_dfm, min_termfreq = 10)

dim(opioid_dfm_trimmed)

k_optimize_blm <- FindTopicsNumber(
  opioid_dfm_trimmed,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 2017),
  mc.cores = detectCores(), # to usa all cores available
  verbose = TRUE
)
# Increase the topics, untill the point adding a point nothing betters
FindTopicsNumber_plot(k_optimize_blm)


opioid_tm <- LDA(opioid_dfm_trimmed, k = 36, method = "Gibbs", control = list(iter = 3000,seed = 10012))

# Extract the keywords for the most popular ones
topic_terms <- get_terms(opioid_tm, k=10)
print(topic_terms)
topic_terms20 <- get_terms(opioid_tm, k=20)
print(topic_terms20)
#Find the most likely topic for each document
#typeof(topics(opioid_tm))
opioid_data$lda <- topics(opioid_tm)

#beta topic: word distribution
opioid_topics <- tidy(opioid_tm, matrix = "beta") 
head(opioid_topics)


#Generate popular words for each topic
opioid_top_terms <- opioid_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

head(opioid_top_terms)

# Creates a plot of the weights and terms by topic
opioid_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# Creates a plot of features with greatest difference in word probabilities between two topics
opioid_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  filter(topic %in% c("topic1", "topic2")) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  arrange(-abs(log_ratio)) %>%
  slice(c(1:10,(nrow(.)-9):nrow(.))) %>%
  arrange(-log_ratio) %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  ggplot(aes(as.factor(term), log_ratio)) +
  geom_col(show.legend = FALSE) +
  xlab("Terms") + ylab("Log-Ratio") +
  coord_flip()


##document topic distribution gamma:
opioid_doc_topics <- opioid_tm@gamma

# Store the results of words over topics
#words_topics <- blm_tm@beta
# Only interested in topics
# Transpose the data so that the days are columns
opioid_doc_topics <- t(opioid_doc_topics) # transpose
dim(opioid_doc_topics)
opioid_doc_topics[1:5,1:5]

# Arrange topics
# Find the top topic per column (day)
max <- apply(opioid_doc_topics, 2, which.max)

# Write a function that finds the second max
which.max2 <- function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}

max2 <- apply(opioid_doc_topics, 2, which.max2)
max2 <- sapply(max2, max)

# Coding police shooting events
victim <- c("Freddie Gray", "Sandra Bland")
shootings <- mdy(c("04/12/2015","7/13/2015"))

# Combine data
top2 <- data.frame(top_topic = max, second_topic = max2, date = ymd(opioid_data$date))

# Plot
opioid_plot <- ggplot(top2, aes(x=date, y=top_topic, pch="First")) 

opioid_plot + geom_point(aes(x=date, y=second_topic, pch="Second") ) +theme_bw() + 
  ylab("Topic Number") + ggtitle("Top Opioid Less affected States Topics") + geom_point() + xlab(NULL) + 
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 



#----------
## Expressivity with the titles
#------

#average_intersection = double(0)
##for (i in 1:30){
##  top_words <- topic_terms[,i]
#  topic_data <- opioid[opioid$lda == i,]

#  topic_dfm <- dfm(topic_data$title, tolower = T, remove_punct = T,remove_numbers = T, 
#                   stem = T, remove = stopwords('english'))
#  top_words_dfm <- dfm_select(topic_dfm, pattern = top_words)
#  average_intersection[i] <- sum(rowSums(top_words_dfm)/rowSums(topic_dfm))/(nrow(topic_data))

#}
#names(average_intersection) <- c(1:30)

average_intersection20 = double(0)
for (i in 1:36){
  top_words <- topic_terms20[,i]
  topic_data <- opioid_data[opioid_data$lda == i,]
  
  topic_dfm <- dfm(topic_data$title, tolower = T, remove_punct = T,remove_numbers = T, 
                   stem = T, remove = stopwords('english'))
  top_words_dfm <- dfm_select(topic_dfm, pattern = top_words)
  average_intersection20[i] <- sum(rowSums(top_words_dfm)/rowSums(topic_dfm))/(nrow(topic_data))
  
}

#Find the most popular 5 topics by ranking the topic
popular_topics <- sort(table(topics(opioid_tm)), decreasing = TRUE)[1:5]
popular_topics_idx <- names(popular_topics)
print(popular_topics)









