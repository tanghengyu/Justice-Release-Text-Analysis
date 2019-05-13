library(bursts)
library(readtext)
library(quanteda)
library(quanteda.corpora)
library(dplyr)
library(lsa)
library(text2vec)
library(geometry)
library(rsvd)
library(Rtsne)
library(stm)
library(factoextra)
libraries <- c("ldatuning", "topicmodels", "ggplot2", "dplyr", 
               "rjson", "quanteda", "lubridate", "parallel", "doParallel", "tidytext", 
               "stringi", "tidyr")
lapply(libraries, require, character.only = TRUE)

press_data <- stream_in(file('combined.json'))

component_data <- press_data[(lengths(press_data$components) != 0),]
state_idx <- lapply(component_data$components, function(x) any(grepl('USAO+', x))) %>% unlist()
state_data <- component_data[state_idx,]

state_name <- character(0)
for (i in 1:nrow(state_data)){
  curr_comp_list <- unlist(state_data[i,]$components)
  usao_idx <- grep('USAO+', curr_comp_list)
  state_name[i] <- strsplit(strsplit(curr_comp_list[usao_idx], '-')[[1]][2], ',')[[1]][1]
}
state_data$state <- state_name
state_data <- state_data[!is.na(state_data$state),]
table(state_name) #Proportion of press-release related
sort(table(state_name), decreasing = TRUE)
top_state <- sort(table(state_name), decreasing = TRUE)[1:10]
# Use state data
con_state2009 <- c(" Alabama" , " Alaska" , " Arizona", " Arkansas", " Georgia", " Idaho", " Kansas", " Kentucky" ,
                   " Louisiana", " Mississippi", " Missouri"," Montana", " Nebraska", " Oklahoma", " North Dakota",
                   " South Carolina", " South Dakota", " Tennessee", " Texas", " Utah", " West Virginia", " Wyoming")

lib_state2009 <- c(" California"," Colorado", " Connecticut", " Delaware", " District of Columbia", " Florida", 
                   " Hawaii", " Illinois", " Indiana", "Iowa", " Maine", " Maryland", " Massachusetts" , " Michigan",
                  " Minnesota", " Nevada", " New Hampshire", " New Jersey", " New Mexico", " New York", 
                  " North Carolina", " Ohio", " Oregon", " Pennsylvania", " Rhode Island", " Vermont", " Virginia", 
                  " Washington", " Wisconsin")

con_state2016 <- c(" Alabama" , " Alaska" , " Arizona", " Arkansas", " Georgia", " Florida", " Idaho", 
                   " Indiana", " Iowa", " Kansas", " Kentucky", " Louisiana", " Michigan", " Mississippi",
                   " Missouri", " Montana", " Nebraska", " North Carolina", " North Dakota", " Ohio", " Oklahoma",
                   " Pennsylvania", " South Carolina", " South Dakota"," Tennessee", " Texas", " Utah",
                   " West Virginia", " Wyoming", " Wisconsin")

lib_state2016 <- c(" California"," Colorado", " Connecticut", " Delaware", " District of Columbia", " Hawaii", " Illinois",
                   " Maine", " Maryland", " Massachusetts", " Minnesota"," Nevada", " New Hampshire", " New Jersey",
                   " New Mexico", " New York", " Oregon"," Rhode Island", " Vermont", " Virginia"," Washington")

changed_state <- c(" Florida", " Indiana", " Iowa", " Michigan", " North Carolina", " Ohio", " Pennsylvania", 
                   " Wisconsin")




con2009 <- state_data[(state_data$state %in% con_state2009),]
lib2009 <- state_data[(state_data$state %in% lib_state2009),]
con2016 <- state_data[(state_data$state %in% con_state2016), ]
lib2016 <- state_data[(state_data$state %in% lib_state2016), ]

changed <- state_data[(state_data$state %in% changed_state), ]

##--------------------
# Use Embeddings to discover different name meanings behind
##--------------------
my_tokens <- as.list(tokens(char_tolower(con2009$contents), remove_punct = T))
my_it <- itoken(my_tokens, progressbar = FALSE)
con_vocab <- create_vocabulary(my_it, stopwords = stopwords('english'))
con_vocab <- prune_vocabulary(con_vocab, term_count_min = 20)

embedding_vectorizer <- vocab_vectorizer(con_vocab)
tcm <- create_tcm(my_it, embedding_vectorizer, skip_grams_window = 6, skip_grams_window_context = "symmetric")

glove <- GlobalVectors$new(word_vectors_size = 300, 
                           vocabulary = con_vocab, 
                           x_max = 100,
                           lambda = 1e-5)

word_vectors_main <- glove$fit_transform(tcm, 
                                         n_iter = 10,
                                         convergence_tol = 1e-3, 
                                         n_check_convergence = 1L,
                                         n_threads = RcppParallel::defaultNumThreads())

word_vectors_context <- glove$components
con_vectors <- word_vectors_main + t(word_vectors_context)
save(con_vectors, file='con_embeddings.RData')


load(file='con_embeddings.RData')
nearest_neighbors('opioid', con_vectors, N = 15, norm = "l2")
nearest_neighbors('gun', con_vectors, N = 15, norm = 'l2')
nearest_neighbors('drug', con_vectors, N = 15, norm = 'l2')
nearest_neighbors('alcohol', con_vectors, N = 15, norm = 'l2')
nearest_neighbors('fentanyl', con_vectors, N = 15, norm = 'l2')
nearest_neighbors('immigration', con_vectors, N = 15, norm = 'l2')
nearest_neighbors('religion', con_vectors, N = 15, norm = 'l2')
nearest_neighbors('nation', con_vectors, N = 15, norm = 'l2')
nearest_neighbors('job', con_vectors, N = 15, norm = 'l2')
nearest_neighbors('rights', con_vectors, N = 15, norm = 'l2')
nearest_neighbors('american', con_vectors, N = 15, norm = 'l2')
nearest_neighbors('president', con_vectors, N = 15, norm = 'l2')
nearest_neighbors('child', con_vectors, N = 15, norm = "l2")

#Liberal party embeddings:
my_tokens <- as.list(tokens(char_tolower(lib2016$contents), remove_punct = T))
my_it <- itoken(my_tokens, progressbar = FALSE)
lib_vocab <- create_vocabulary(my_it, stopwords = stopwords('english'))
lib_vocab <- prune_vocabulary(lib_vocab, term_count_min = 20)

embedding_vectorizer <- vocab_vectorizer(lib_vocab)
tcm <- create_tcm(my_it, embedding_vectorizer, skip_grams_window = 6, skip_grams_window_context = "symmetric")

glove <- GlobalVectors$new(word_vectors_size = 300, 
                           vocabulary = lib_vocab, 
                           x_max = 100,
                           lambda = 1e-5)

word_vectors_main <- glove$fit_transform(tcm, 
                                         n_iter = 10,
                                         convergence_tol = 1e-3, 
                                         n_check_convergence = 1L,
                                         n_threads = RcppParallel::defaultNumThreads())

word_vectors_context <- glove$components
lib_vectors <- word_vectors_main + t(word_vectors_context)
save(lib_vectors, file='lib_embeddings.RData')


load(file='lib_embeddings.RData')
#nearest_neighbors('opioid', lib_vectors, N = 15, norm = "l2")
nearest_neighbors('gun', lib_vectors, N = 15, norm = 'l2')
nearest_neighbors('drug', lib_vectors, N = 15, norm = 'l2')
nearest_neighbors('alcohol', lib_vectors, N = 15, norm = 'l2')
#nearest_neighbors('fentanyl', lib_vectors, N = 15, norm = 'l2')
nearest_neighbors('immigration', lib_vectors, N = 15, norm = 'l2')
nearest_neighbors('religion', lib_vectors, N = 15, norm = 'l2')
nearest_neighbors('nation', lib_vectors, N = 15, norm = 'l2')
nearest_neighbors('job', lib_vectors, N = 15, norm = 'l2')
nearest_neighbors('rights', lib_vectors, N = 15, norm = 'l2')
nearest_neighbors('american', lib_vectors, N = 15, norm = 'l2')
nearest_neighbors('president', lib_vectors, N = 15, norm = 'l2')
nearest_neighbors('child', lib_vectors, N = 15, norm = "l2")

#-------------------------
## PCA
## Change of social stands from the text
##
#-------------------------
year_data <- changed[changed$year %in% c(2017, 2018),]
year_dfm <- dfm(year_data$contents,
                remove_punct = T, remove_numbers = T,
                remove = stopwords("english"))
year_dfm <- dfm_trim(year_dfm, min_termfreq = 5)
year_mat <- convert(year_dfm, to = "matrix") # convert to matrix

# run pca center true to be default, scaling to be true
year_pca <- prcomp(year_mat, center = TRUE, scale = TRUE)

pc_loadings <- year_pca$rotation

fviz_eig(year_pca, addlabels = TRUE)
#var <- get_pca_var(content_pca_small)
#fviz_pca_var(content_pca_small, col.var = "black")

N <- 10
pc1_loading <- tibble(token = rownames(pc_loadings), loading = as.vector(pc_loadings[,1])) %>% arrange(-loading)
pc1_loading$loading <- scale(pc1_loading$loading, center = TRUE)
pc1_loading <- rbind(top_n(pc1_loading, N, loading),top_n(pc1_loading, -N, loading))
pc1_loading <- transform(pc1_loading, token = factor(token, levels = unique(token)))
pc1_loading

ggplot(pc1_loading, aes(token, loading)) + 
  geom_bar(stat = "identity", fill = ifelse(pc1_loading$loading <= 0, "grey20", "grey70")) +
  coord_flip() + 
  xlab("Tokens") + ylab("Token Leadings") + 
  ggtitle("Leading tokens on PC1 2018 ")+
  scale_colour_grey(start = .3, end = .7) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=18, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        legend.text=element_text(size=16),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "top",
        legend.spacing.x = unit(0.25, 'cm'),
        plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,0,0),"cm"))



# token loadings
for (i in 1:5){
  N <- 10
  pc1_loading <- tibble(token = rownames(pc_loadings), loading = as.vector(pc_loadings[,i])) %>% arrange(-loading)
  pc1_loading$loading <- scale(pc1_loading$loading, center = TRUE)
  pc1_loading <- rbind(top_n(pc1_loading, N, loading),top_n(pc1_loading, -N, loading))
  pc1_loading <- transform(pc1_loading, token = factor(token, levels = unique(token)))
  print(pc1_loading)
}


##------------------------
## Topics between two parties
## Possible topics over time
## LDA models
##-------------------------
subset_dfm <- dfm(con2009$contents, tolower = TRUE, remove_punct = TRUE, 
                  remove_numbers = TRUE, stem = TRUE, remove = stopwords('english'))
subset_dfm_trimmed <- dfm_trim(subset_dfm, min_termfreq = 15, min_docfreq = 5)
dim(subset_dfm_trimmed)

k_optimize_blm <- FindTopicsNumber(
  subset_dfm_trimmed,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 2017),
  mc.cores = detectCores(), # to usa all cores available
  verbose = TRUE
)
# Increase the topics, untill the point adding a point nothing betters
FindTopicsNumber_plot(k_optimize_blm)
save(k_optimize_blm, file='con2009_optimize_k.RData')

# Construct the LDA model
subset_tm_con <- LDA(subset_dfm_trimmed, k = 40, method = "Gibbs", control = list(iter = 3000,seed = 10012))
save(subset_tm_con, file = 'con_lad.RData')

topic_terms <- get_terms(subset_tm_con, k=10)
print(topic_terms)

topic_terms20 <- get_terms(subset_tm_con, k=20)
save(topic_terms20, file = 'con_lda_20.RData')
print(topic_terms20)
#Find the most likely topic for each document
##Section where title and content are compared
con2009$lda <- topics(subset_tm_con)


#beta topic: word distribution
con_topics <- tidy(subset_tm_con, matrix = "beta") 
head(con_topics)


#Generate popular words for each topic
con_top_terms <- con_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

head(opioid_top_terms)

# Creates a plot of the weights and terms by topic
con_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# Creates a plot of features with greatest difference in word probabilities between two topics
con_topics %>%
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
con_doc_topics <- subset_tm_con@gamma

# Store the results of words over topics
#words_topics <- blm_tm@beta
# Only interested in topics
# Transpose the data so that the days are columns
con_doc_topics <- t(con_doc_topics) # transpose
dim(con_doc_topics)
con_doc_topics[1:5,1:5]

# Arrange topics
# Find the top topic per column (day)
max <- apply(con_doc_topics, 2, which.max)

# Write a function that finds the second max
which.max2 <- function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}

max2 <- apply(con_doc_topics, 2, which.max2)
max2 <- sapply(max2, max)

# Coding police shooting events
victim <- c("Trump")
shootings <- mdy(c("8/11/2016"))

# Combine data
top2 <- data.frame(top_topic = max, second_topic = max2, date = ymd(con2009$date))

# Plot
con_plot <- ggplot(top2, aes(x=date, y=top_topic, pch="First")) 

con_plot + geom_point(aes(x=date, y=second_topic, pch="Second") ) +theme_bw() +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
    ylab("Topic Number") + ggtitle("Conservative States Topics Distribution") + geom_point() + xlab(NULL) + 
  geom_vline(xintercept=as.numeric(shootings[1]), color = "blue", linetype=4)
  +scale_shape_manual(values=c(18, 1), name = "Topic Rank") 

#Find the most popular 5 topics by ranking the topic
popular_topics <- sort(table(topics(subset_tm_con)), decreasing = TRUE)[1:10]
popular_topics_idx <- names(popular_topics)
print(popular_topics)








### Liberal Parties:
lib_subset_dfm <- dfm(lib2016$contents, tolower = TRUE, remove_punct = TRUE, 
                  remove_numbers = TRUE, stem = TRUE, remove = stopwords('english'))
lib_subset_dfm_trimmed <- dfm_trim(lib_subset_dfm, min_termfreq = 20, min_docfreq = 10)
dim(lib_subset_dfm_trimmed)

k_optimize_lib <- FindTopicsNumber(
  lib_subset_dfm_trimmed,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 2017),
  mc.cores = detectCores(), # to usa all cores available
  verbose = TRUE
)
# Increase the topics, untill the point adding a point nothing betters
FindTopicsNumber_plot(k_optimize_lib)
save(k_optimize_lib, file='lib2016_optimize_k.RData')



lib_subset_tm <- LDA(lib_subset_dfm_trimmed, k = 43, method = "Gibbs", control = list(iter = 3000,seed = 10012))

lib_topic_terms <- get_terms(lib_subset_tm, k=10)
print(lib_topic_terms)
lib_topic_terms20 <- get_terms(lib_subset_tm, k=20)
print(lib_topic_terms20)
save(lib_topic_terms20, file = 'liberal_lda_20.RData')
#Find the most likely topic for each document
typeof(topics(subset_tm))
##Section where title and content are compared
lib2016$lda <- topics(lib_subset_tm)


#Find the most popular 5 topics by ranking the topic
popular_topics <- sort(table(topics(lib_subset_tm)), decreasing = TRUE)[1:10]
popular_topics_idx <- names(popular_topics)
print(popular_topics)
#beta topic: word distribution


lib_topics <- tidy(lib_subset_tm, matrix = "beta") 

#Generate popular words for each topic
lib_top_terms <- lib_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


# Creates a plot of the weights and terms by topic
lib_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

lib_topics <- tidy(lib_subset_tm, matrix = "beta")
temp_topic <- mutate(lib_topics,topic = paste0('topic', topic))
temp_topic <- filter(temp_topic, topic %in% c('topic22'))
temp_topic <- spread(temp_topic, topic, beta)
temp_topic <- filter(temp_topic, topic22>0.001)

con_topics <- tidy(subset_tm_con, matrix = "beta") 
temp_con_topic <- mutate(con_topics,topic = paste0('topic', topic))
temp_con_topic <- filter(temp_con_topic, topic %in% c('topic33'))
temp_con_topic <- spread(temp_con_topic, topic, beta)
temp_con_topic <- filter(temp_con_topic, topic33>0.001)

temp_all <- mutate(lib_topics, topic = paste0("topic", topic))
temo_all <- filter(temp_all, topic %in% c("topic1", "topic2")) %>%
temp_all <- spread(temp_all, topic, beta) %>%
temp_all <- filter(temp_all, topic1 > .001 | topic2 > .001)

lib_topic <- as.data.frame(temp_topic)
con_topic <- as.data.frame(temp_con_topic)
lib_topic <- lib_topic[1:140,]
all_topic <- cbind(lib_topic, con_topic)
all_topic$log_ratio <- log2(all_topic$topic33/all_topic$topic22)

all_topic_ordered <- all_topic[order(-abs(all_topic$log_ratio)),]

all_topic_subset <- all_topic_ordered[c(1:10, 131:140),]
all_topic_subset <- all_topic_subset[order(-all_topic_subset$log_ratio),]
all_topic_subset$plot_term <- factor(all_topic_subset$term, levels = unique(all_topic_subset$term))
all_topic_subset$lib_term <- unique(all_topic_subset$term)


ggplot(all_topic_subset, aes(x = lib_term, y= log_ratio))

plot_temp <- cbind(all_topic_subset$lib_term, all_topic_subset$log_ratio)
colnames(plot_temp) <- c('term', 'log')
plot_temp <- as.data.frame(plot_temp)

log_value = c(5.04948230652108, 4.19944210311627, 3.7585489816856,3.53966492918464,3.50474847753054,0.117575892691487,
              0.116533633550008,    0.0860486068163672,   0.0564841492742285,   -0.00349399676636496,-0.0304264084013957,
              -0.0319272772489307, -0.0828804021522215, -0.102476148391787, -0.157814176371969, 
              -3.41443795802699, -3.51465685945228, -3.59952285420921, -3.71583162766372, -4.27673600406245)

term_value = c('drug', 'distribut', 'investig',  'feder', 'enforc', 'cartel', 'safer',
               'sinc' ,'portland', 'fugit','mexico','identifi', 'marijuana', 'coordin',
               'four',  'enterpris',  'arizona', 'mark','nine','justic')
draw_plot = data.frame(term = term_value, log = log_value, stringsAsFactors = FALSE)


new_plot = data.frame(con_term = con_topic$term, log = log2(con_topic$topic33/lib_topic$topic22), stringsAsFactors = FALSE)
new_plot = new_plot[order(-abs(new_plot$log)),]
new_plot_subset = new_plot[c(1:10, 131:140),]
new_plot_subset = new_plot_subset[order(-new_plot_subset$log),]

ggplot(data = new_plot_subset, aes(x=con_term, y = log))+geom_col(show.legend=FALSE) + coord_flip()

plot_temp$log_true <- as.double(plot_temp$log)
plot(all_topic_subset$plot_term, all_topic_subset$log_ratio)
# Creates a plot of features with greatest difference in word probabilities between two topics
lib_topics %>%
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
lib_doc_topics <- lib_subset_tm@gamma

# Store the results of words over topics
#words_topics <- blm_tm@beta
# Only interested in topics
# Transpose the data so that the days are columns
lib_doc_topics <- t(lib_doc_topics) # transpose
dim(lib_doc_topics)
lib_doc_topics[1:5,1:5]

# Arrange topics
# Find the top topic per column (day)
max <- apply(lib_doc_topics, 2, which.max)

# Write a function that finds the second max
which.max2 <- function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}

max2 <- apply(lib_doc_topics, 2, which.max2)
max2 <- sapply(max2, max)

# Coding police shooting events
victim <- c("Trump")
shootings <- mdy(c("8/11/2016"))

# Combine data
top2 <- data.frame(top_topic = max, second_topic = max2, date = ymd(lib2016$date))

# Plot
lib_plot <- ggplot(top2, aes(x=date, y=top_topic, pch="First")) 

 
lib_plot + geom_point(aes(x=date, y=second_topic, pch="Second") ) +theme_bw() +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
  ylab("Topic Number") + ggtitle("Liberal States Topics Distribution") + geom_point() + xlab(NULL) + 
  geom_vline(xintercept=as.numeric(shootings[1]), color = "blue", linetype=4)
+scale_shape_manual(values=c(18, 1), name = "Topic Rank") 

#Find the most popular 5 topics by ranking the topic
popular_topics <- sort(table(topics(subset_tm_con)), decreasing = TRUE)[1:10]
popular_topics_idx <- names(popular_topics)
print(popular_topics)

























con_year = sort(unique(con2016$year))
#Topic model changing over time
for (i in 2:length(con_year)){
  subset_data <- con2016[con2016$year == con_year[i],]
  subset_dfm <- dfm(subset_data$contents, tolower = TRUE, remove_punct = TRUE, 
                    remove_numbers = TRUE, stem = TRUE, remove = stopwords('english'))
  subset_dfm_trimmed <- dfm_trim(subset_dfm, min_termfreq = 10, min_docfreq = 5)
  rowTotals <- apply(subset_dfm_trimmed , 1, sum) 
  subset_dfm_trimmed <- subset_dfm_trimmed[rowTotals> 0, ] 
  subset_tm <- LDA(subset_dfm_trimmed, k = 10, method = "Gibbs", control = list(iter = 3000,seed = 10012))
  print(con_year[i])
  topic_terms <- get_terms(subset_tm, k=15)
  
  popular_topics <- sort(table(topics(subset_tm)), decreasing = TRUE)
  popular_idx <- as.integer(names(popular_topics))
  topic_ordered <- topic_terms[, popular_idx]
  print(topic_ordered)
}

##LSA:







# For topic component: further investigation -> does it corresponds well with the given topic?
tag_data <- press_data[(lengths(press_data$topics)!=0),]
tag_list <- unlist(tag_data$topics) %>% unique() %>% unlist()



#Immigrations
#Drugs
#Abortions if possible
average_intersection = double(0)
for (i in 1:30){
  top_words <- topic_terms[,i]
  topic_data <- subset_data[subset_data$lda == i,]
  
  topic_dfm <- dfm(topic_data$title, tolower = T, remove_punct = T,remove_numbers = T, 
                   stem = T, remove = stopwords('english'))
  top_words_dfm <- dfm_select(topic_dfm, pattern = top_words)
  average_intersection[i] <- sum(rowSums(top_words_dfm)/rowSums(topic_dfm))/(nrow(topic_data))
  
}
names(average_intersection) <- c(1:30)

average_intersection20 = double(0)
for (i in 1:30){
  top_words <- topic_terms20[,i]
  topic_data <- subset_data[subset_data$lda == i,]
  
  topic_dfm <- dfm(topic_data$title, tolower = T, remove_punct = T,remove_numbers = T, 
                   stem = T, remove = stopwords('english'))
  top_words_dfm <- dfm_select(topic_dfm, pattern = top_words)
  average_intersection20[i] <- sum(rowSums(top_words_dfm)/rowSums(topic_dfm))/(nrow(topic_data))
  
}