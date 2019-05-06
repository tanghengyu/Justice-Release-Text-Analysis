#To store the result recorded codes
#----------------------------------------
# Lexical Diversity of the text (DONE)
#---------------------------------------

## Lexical Diversity over years: 
ttr_result = double(0)
year_list = sort(unique(press_data$year))
for (i in 1:length(year_list)){
  print(year_list[i])
  year_data <-  press_data[(press_data$year == year_list[i]),]
  year_tokens <- tokens_wordstem(tokens_remove(tokens(char_tolower(year_data$contents), 
                                                      remove_punct=T, remove_symbol = T), stopwords('english')))
  total_tokens <- lengths(year_tokens)
  year_types <- ntype(year_tokens)
  ttr_result[i] <- mean(year_types/total_tokens, na.rm = T)
  
}
ttr_result

#TTR score of each contents by year
press_tokens <- tokens_remove(tokens(press_data$contents, remove_punct = TRUE), stopwords('english'))
# Num tokens per document
num_tokens <- lengths(press_tokens)
num_types <- ntype(press_tokens)
press_TTR <- num_types / num_tokens

head(press_TTR)

# 3.2 Mean per-document TTR scores by year(components?)

TTR_by_year <- aggregate(press_TTR, by = list(press_data$year), FUN = mean, na.rm = TRUE) %>% setNames(c("year", "TTR"))

plot(TTR_by_year, labels = names(TTR_by_year$TTR))


# Based on lexical diversity, although rather versatile, we can witness an increase in lexcial diversity
component_list <- unique(unlist(unique(press_data$components)))
component_list[grep('USAO+', component_list, value = F)] <- 'USAO'
component_list <- unique(component_list)

component_result = double(0)
for (i in 1:length(component_list)){
  print(component_list[i])
  component_data <- press_data[sapply(press_data$components, function(x) component_list[i] %in% x), ]
  component_tokens <- tokens_wordstem(tokens_remove(tokens(char_tolower(component_data$contents),
                                                           remove_punct = T, remove_symbol =  T), stopwords('english')))
  total_tokens <- lengths(component_tokens)
  comp_types <- ntype(component_tokens)
  component_result[i] <- mean(comp_types/total_tokens, na.rm = T)
}
component_df <- data.frame(component_name = component_list, TTR = component_result)
plot(component_df)
component_list[which(component_result == max(component_result))]
component_list[which(component_result == min(component_result))]

ggplot(component_df, aes(x = component_name, y = TTR))+ ggtitle("Division TTR Score")+geom_point()+ theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) 

press_data$date <- as.Date(press_data$date)
press_ordered <- press_data[(order(press_data$date)), ]
press_content <-press_ordered$contents
press_corpus <- corpus(press_content)
docvars(press_corpus)$date <- press_ordered$date
docvars(press_corpus)$year <- press_ordered$year

##Linguistic readbility/Complexity
# FRE and DC
#Calculate FRE and Dale Chall score to over the year.
year_fre <- textstat_readability(texts(press_corpus, groups = "year"), "Flesch") 
plot(year_fre)
colnames(year_fre) <- c('Year', 'Flesch')
ggplot(year_fre, aes(x = Year, y= Flesch))+ggtitle('Year FRE Score') + geom_point()+ theme(plot.title = element_text(hjust = 0.5)) 

year_dc <- textstat_readability(texts(press_corpus, groups = "year"), "Dale.Chall.old") 
plot(year_dc)
colnames(year_dc) <- c('Year', 'DC')
ggplot(year_dc, aes(x = Year, y= DC))+ggtitle('Year DC Score') + geom_point()+ theme(plot.title = element_text(hjust = 0.5)) 

#------------------
## Clustering (DONE)
## TF-IDF + PCA
#------------------

#tf-idf
# 4.1 tfidf - Frequency weighting
content_dfm <- dfm(press_data$contents, tolower = T, remove_punct = T, remove_symbols=T,
                   stem = T, remove_numbers = T, remove = stopwords('english'))
content_dfm <- dfm_trim(content_dfm, min_termfreq = 30, min_docfreq = 20)

weighted_dfm <- dfm_tfidf(content_dfm) # uses the absolute frequency of terms in each document
topfeatures(weighted_dfm)
topfeatures(weighted_dfm[nrow(weighted_dfm),])

normalized_dfm <- dfm_tfidf(content_dfm, scheme_tf = "prop") # Uses feature proportions within documents: divdes each term by the total count of features in the document
topfeatures(normalized_dfm)
topfeatures(normalized_dfm[nrow(normalized_dfm),])

###Reference: https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
# Clustering using tf-idf
total_word_freq <- colSums(normalized_dfm)
#Sort in decreasing order of total word freq
content_dfm_sorted <- normalized_dfm[,order(total_word_freq, decreasing = TRUE)]
t_content_dfm <- t(content_dfm_sorted)[1:500,]  
content_DistMat <- dist(t_content_dfm)
wordCluster <- hclust(content_DistMat)
clustering <- cutree(wordCluster, 10)
plot(wordCluster, labels = docnames(t_content_dfm),
     xlab="", main="Relative Term Frequency weighting")
rect.hclust(wordCluster, 10, border = "red")

content_dfm <- t(t_content_dfm)
p_words <- colSums(content_dfm) / sum(content_dfm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- content_dfm[ clustering == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})

cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)

cluster_summary

# plot a word cloud of one cluster as an example
wordcloud::wordcloud(words = names(cluster_words[[ 2 ]]), 
                     freq = cluster_words[[ 2]], 
                     max.words = 50, 
                     random.order = FALSE, 
                     colors = c("red", "yellow", "blue"),
                     main = "Top words in cluster 100")

#Over the year
for (year in year_list){
  year_data <- press_data[press_data$year == year,]
  content_dfm <- dfm(year_data$contents, tolower = T, remove_punct = T, remove_symbols=T,
                     stem = T, remove_numbers = T, remove = stopwords('english'))
  content_dfm <- dfm_trim(content_dfm, min_termfreq = 20, min_docfreq = 10)
  
  weighted_dfm <- dfm_tfidf(content_dfm) # uses the absolute frequency of terms in each document
  topfeatures(weighted_dfm)
  topfeatures(weighted_dfm[nrow(weighted_dfm),])
  
  normalized_dfm <- dfm_tfidf(content_dfm, scheme_tf = "prop") # Uses feature proportions within documents: divdes each term by the total count of features in the document
  topfeatures(normalized_dfm)
  topfeatures(normalized_dfm[nrow(normalized_dfm),])
  
  ###Reference: https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
  # Clustering using tf-idf
  total_word_freq <- colSums(normalized_dfm)
  #Sort in decreasing order of total word freq
  content_dfm_sorted <- normalized_dfm[,order(total_word_freq, decreasing = TRUE)]
  t_content_dfm <- t(content_dfm_sorted)[1:500,]  
  content_DistMat <- dist(t_content_dfm)
  wordCluster <- hclust(content_DistMat)
  clustering <- cutree(wordCluster, 10)
  plot(wordCluster, labels = docnames(t_content_dfm),
       xlab="", main="Relative Term Frequency weighting")
  rect.hclust(wordCluster, 10, border = "red")
  
  content_dfm <- t(t_content_dfm)
  p_words <- colSums(content_dfm) / sum(content_dfm)
  
  cluster_words <- lapply(unique(clustering), function(x){
    rows <- content_dfm[ clustering == x , ]
    
    # for memory's sake, drop all words that don't appear in the cluster
    rows <- rows[ , colSums(rows) > 0 ]
    
    colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
  })
  
  cluster_summary <- data.frame(cluster = unique(clustering),
                                size = as.numeric(table(clustering)),
                                top_words = sapply(cluster_words, function(d){
                                  paste(
                                    names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                    collapse = ", ")
                                }),
                                stringsAsFactors = FALSE)
  print(year)
  print(cluster_summary)
  
}


#Over different states:
for (state_name in names(top_state)){
  curr_state <- state_data[state_data$state == state_name,]
  state_dfm <- dfm(curr_state$contents, tolower = T, remove_punct = T, remove_symbols=T,
                   stem = T, remove_numbers = T, remove = stopwords('english'))
  state_dfm <- dfm_trim(state_dfm, min_termfreq = 20, min_docfreq = 10)
  
  weighted_dfm <- dfm_tfidf(state_dfm) # uses the absolute frequency of terms in each document
  #print(state_name)
  #print(topfeatures(weighted_dfm))
  #print(topfeatures(weighted_dfm[nrow(weighted_dfm),]))
  
  normalized_dfm <- dfm_tfidf(state_dfm, scheme_tf = "prop") # Uses feature proportions within documents: divdes each term by the total count of features in the document
  print(state_name)
  print(topfeatures(normalized_dfm))
  print(topfeatures(normalized_dfm[nrow(normalized_dfm),]))
  
  ###Reference: https://cran.r-project.org/web/packages/textmineR/vignettes/b_document_clustering.html
  # Clustering using tf-idf
  total_word_freq <- colSums(normalized_dfm)
  #Sort in decreasing order of total word freq
  content_dfm_sorted <- normalized_dfm[,order(total_word_freq, decreasing = TRUE)]
  t_content_dfm <- t(content_dfm_sorted)[1:min(500, dim(content_dfm_sorted)[1]),]  
  content_DistMat <- dist(t_content_dfm)
  wordCluster <- hclust(content_DistMat)
  clustering <- cutree(wordCluster, 10)
  plot(wordCluster, labels = docnames(t_content_dfm),
       xlab="", main="Relative Term Frequency weighting")
  rect.hclust(wordCluster, 10, border = "red")
  
  content_dfm <- t(t_content_dfm)
  p_words <- colSums(content_dfm) / sum(content_dfm)
  
  cluster_words <- lapply(unique(clustering), function(x){
    rows <- content_dfm[ clustering == x , ]
    
    # for memory's sake, drop all words that don't appear in the cluster
    rows <- rows[ , colSums(rows) > 0 ]
    
    colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
  })
  
  cluster_summary <- data.frame(cluster = unique(clustering),
                                size = as.numeric(table(clustering)),
                                top_words = sapply(cluster_words, function(d){
                                  paste(
                                    names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                    collapse = ", ")
                                }),
                                stringsAsFactors = FALSE)
  print(state_name)
  print(cluster_summary)
  
}


#PCA:
# makes it easy to work with PCA (great for visualization)
pca_data <- press_data[sample(nrow(press_data), 2000),]

content_dfm <- dfm(pca_data$contents, 
                   stem = T, 
                   remove_punct = T, remove_numbers = T, remove_symbols = T,
                   remove = stopwords("english"))

content_dfm <- dfm_trim(content_dfm, min_termfreq = 60, min_docfreq = 20)

content_mat <- convert(content_dfm, to = "matrix") # convert to matrix

# run pca center true to be default, scaling to be true
# Dont run this takes long
content_pca_small <- prcomp(content_mat, center = TRUE, scale = TRUE)

pc_loadings <- content_pca_small$rotation

fviz_eig(content_pca_small, addlabels = TRUE)
var <- get_pca_var(content_pca_small)
fviz_pca_var(content_pca_small, col.var = "black")

# token loadings
N <- 10
pc1_loading <- tibble(token = rownames(pc_loadings), loading = as.vector(pc_loadings[,1])) %>% arrange(-loading)
pc1_loading$loading <- scale(pc1_loading$loading, center = TRUE)
pc1_loading <- rbind(top_n(pc1_loading, N, loading),top_n(pc1_loading, -N, loading))
pc1_loading <- transform(pc1_loading, token = factor(token, levels = unique(token)))
pc1_loading
# plot top tokens according to absolute loading values
ggplot(pc1_loading, aes(token, loading)) + 
  geom_bar(stat = "identity", fill = ifelse(pc1_loading$loading <= 0, "grey20", "grey70")) +
  coord_flip() + 
  xlab("Tokens") + ylab("Tokens Leadings")+
  ggtitle("Content: Leading tokens on PC1") +
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
        plot.title = element_text(hjust = 0.5),
        legend.spacing.x = unit(0.25, 'cm'),
        plot.margin=unit(c(1,1,0,0),"cm"))

# Value of the rotated data: your "new", dimensionality reduced data
View(content_pca_small$x)  # each observation 
dim(content_pca$x)
nearest_neighbors <- function(query, low_dim_space, N = 5, norm = "l2"){
  cos_sim <- sim2(x = low_dim_space, y = low_dim_space[query, , drop = FALSE], method = "cosine", norm = norm)
  nn <- cos_sim <- cos_sim[order(-cos_sim),]
  return(names(nn)[2:(N + 1)])  # query is always the nearest neighbor hence dropped
}
# apply to document retrieval
nearest_neighbors(query = "drug", low_dim_space = content_pca_small$x, N = 1, norm = "l2")


#Run PCA on headings
#pca_data <- press_data[sample(nrow(press_data), 5000),]
#load(file = 'processed_press_data.RData')
#sub_data <- press_data[sample(nrow(press_data), 1000), ]
headline_dfm <- dfm(pca_data$title, 
                    remove_punct = T, remove_numbers = T,
                    remove = stopwords("english"))

#headline_dfm <- headline_dfm[,(order(colSums(headline_dfm), decreasing = TRUE))]
#headline_dfm <- headline_dfm[, 1:2000]

headline_mat <- convert(headline_dfm, to = "matrix") # convert to matrix

# run pca center true to be default, scaling to be trueh
headline_pca<- prcomp(headline_mat, center = TRUE, scale = TRUE)

pc_loadings <- headline_pca$rotation

fviz_eig(headline_pca, addlabels = TRUE)
var <- get_pca_var(content_pca_small)
fviz_pca_var(headline_pca, col.var = "black")

# token loadings
N <- 10
pc1_loading <- tibble(token = rownames(pc_loadings), loading = as.vector(pc_loadings[,1])) %>% arrange(-loading)
pc1_loading$loading <- scale(pc1_loading$loading, center = TRUE)
pc1_loading <- rbind(top_n(pc1_loading, N, loading),top_n(pc1_loading, -N, loading))
pc1_loading <- transform(pc1_loading, token = factor(token, levels = unique(token)))
pc1_loading


ggplot(pc1_loading, aes(token, loading)) + 
  geom_bar(stat = "identity", fill = ifelse(pc1_loading$loading <= 0, "grey20", "grey70")) +
  coord_flip() + 
  xlab("Tokens") + ylab("Tokens Leadings")+
  ggtitle("Title:Leading tokens on PC1") +
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
        plot.title = element_text(hjust = 0.5),
        legend.spacing.x = unit(0.25, 'cm'),
        plot.margin=unit(c(1,1,0,0),"cm"))


#Test on year data
year_data <- press_data[press_data$year == 2009,]
year_dfm <- dfm(year_data$contents,
                remove_punct = T, remove_numbers = T,
                remove = stopwords("english"))
year_dfm <- dfm_trim(year_dfm, min_termfreq = 60, min_docfreq = 20)
year_mat <- convert(year_dfm, to = "matrix") # convert to matrix

# run pca center true to be default, scaling to be true
year_pca <- prcomp(year_mat, center = TRUE, scale = TRUE)

pc_loadings <- year_pca$rotation

fviz_eig(year_pca, addlabels = TRUE)
#var <- get_pca_var(content_pca_small)
#fviz_pca_var(content_pca_small, col.var = "black")

# token loadings
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
  ggtitle("Leading tokens on PC1 2009 ")+
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

for (i in 1:5){
  N <- 10
  pc1_loading <- tibble(token = rownames(pc_loadings), loading = as.vector(pc_loadings[,i])) %>% arrange(-loading)
  pc1_loading$loading <- scale(pc1_loading$loading, center = TRUE)
  pc1_loading <- rbind(top_n(pc1_loading, N, loading),top_n(pc1_loading, -N, loading))
  pc1_loading <- transform(pc1_loading, token = factor(token, levels = unique(token)))
  print(pc1_loading)
}


#PCA for 2018 data
year_data <- press_data[press_data$year == 2018,]
year_dfm <- dfm(year_data$contents,
                remove_punct = T, remove_numbers = T,
                remove = stopwords("english"))
year_dfm <- dfm_trim(year_dfm, min_termfreq = 30, min_docfreq = 20)
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


#----------------
# Wordfish (DONE)
#----------------
# Wordfish for Immigration to show a shift in left and right ideology
#civil_topics <- c("Civil Rights","Immigration")
#civil_idx <- logical(0)  
#for (i in 1:dim(tag_data)[1]){
#  curr_data <- unlist(tag_data[i,]$topics)
#  civil_idx[i] <- ("Civil Rights" %in% curr_data || "Immigration" %in% curr_data)
#}

immigration_idx <- logical(0)  
for (i in 1:dim(tag_data)[1]){
  curr_data <- unlist(tag_data[i,]$topics)
  immigration_idx[i] <- ("Immigration" %in% curr_data)
}
immigration_data <- tag_data[immigration_idx,]
#Order by dates:
immigration_data$date <- as.Date(immigration_data$date)
immigration_data <- immigration_data[order(immigration_data$date),]
# We choose the left and right based on index 7 more left and index 498 as right

immigration_df <- data.frame(texts = immigration_data$contents, 
                             date = immigration_data$date, stringsAsFactors = FALSE)

immigration_df$text_label <- paste(immigration_df$date, c(1:85), sep = "_")

immigration_dfm <- dfm(immigration_df$texts, 
                       stem = T, remove_numbers = T,
                       remove = stopwords("english"), 
                       remove_punct = T)
#Spot the left and right index
right_idx <-  83
left_idx <-  13
immigration_dfm@Dimnames$docs <- immigration_df$text_label 
#fit the wordfish model
immigration_fish <- textmodel_wordfish(immigration_dfm, dir = c(left_idx,right_idx))
textplot_scale1d(immigration_fish)
#What about using only topics? can we 
immigration_df <- data.frame(texts = immigration_data$title, 
                             date = immigration_data$date, stringsAsFactors = FALSE)

immigration_df$text_label <- paste(immigration_df$date, c(1:85), sep = "_")

immigration_dfm <- dfm(immigration_df$texts,
                       remove = stopwords("english"), 
                       remove_punct = T)
#Spot the left and right index
right_idx <-  83
left_idx <-  13
immigration_dfm@Dimnames$docs <- immigration_df$text_label 
#fit the wordfish model
immigration_fish <- textmodel_wordfish(immigration_dfm, dir = c(left_idx,right_idx))
textplot_scale1d(immigration_fish)

#------------------
# Associate of words in LSA (DONE)
# In topics
#------------------

drug_content <- drug_data$contents
drug_dfm <- dfm(drug_content, tolower = T,remove = stopwords('english'), remove_punct = T, remove_numbers = T)

drug_lsa <- convert(drug_dfm, to = 'lsa') #Convert to lsa
drug_lsa <- lw_logtf(drug_lsa) * gw_idf(drug_lsa)
drug_lsa <- lsa(drug_lsa) 
drug_lsa_textmat <- as.textmatrix(drug_lsa) 
#Find words closely related to korea
opioid <- associate(drug_lsa_textmat, 'opioid', 'cosine', threshold = .4)
fentanyl <- associate(drug_lsa_textmat, 'fentanyl', 'cosine', threshold = .6)
heroin <- associate(drug_lsa_textmat, 'heroin', 'cosine', threshold = .4)
trafficking <- associate(drug_lsa_textmat, 'trafficking', 'cosine', threshold = .4)
ohio <- associate(drug_lsa_textmat, 'ohio', 'cosine', threshold = .4)
virginia <- associate(drug_lsa_textmat, 'virginia', 'cosine', threshold = .4)
crisis <- associate(drug_lsa_textmat, 'crisis', 'cosine', threshold = .4)
marijuana <- associate(drug_lsa_textmat, 'marijuana', 'cosine', threshold = .4)
methamphetamine <- associate(drug_lsa_textmat, 'methamphetamine', 'cosine', threshold = .4)

marijuana[1:10]
crisis[1:10]
opioid[1:10]
fentanyl[1:10]
heroin[1:10]
trafficking[1:10]
ohio[1:10]
virginia[1:10]
methamphetamine[1:10]
###Optional
drug <- associate(drug_lsa_textmat, 'drug', 'cosine', threshold = .4)
opioid <- associate(drug_lsa_textmat, 'opioid', 'cosine', threshold = .4)
fentanyl <- associate(drug_lsa_textmat, 'fentanyl', 'cosine', threshold = .6)
heroin <- associate(drug_lsa_textmat, 'heroin', 'cosine', threshold = .4)
trafficking <- associate(drug_lsa_textmat, 'trafficking', 'cosine', threshold = .4)
ohio <- associate(drug_lsa_textmat, 'ohio', 'cosine', threshold = .4)
virginia <- associate(drug_lsa_textmat, 'virginia', 'cosine', threshold = .4)
crisis <- associate(drug_lsa_textmat, 'crisis', 'cosine', threshold = .4)
marijuana <- associate(drug_lsa_textmat, 'marijuana', 'cosine', threshold = .4)
methamphetamine <- associate(drug_lsa_textmat, 'methamphetamine', 'cosine', threshold = .4)

print(marijuana[1:10])
print(crisis[1:10])
print(opioid[1:10])
print(fentanyl[1:10])
printheroin[1:10]
trafficking[1:10]
ohio[1:10]
virginia[1:10]
methamphetamine[1:10]
#####

#Over the title?
drug_content <- drug_data$title
drug_dfm <- dfm(drug_content, tolower = T,remove = stopwords('english'), remove_punct = T, remove_numbers = T)

drug_lsa <- convert(drug_dfm, to = 'lsa') #Convert to lsa
drug_lsa <- lw_logtf(drug_lsa) * gw_idf(drug_lsa)
drug_lsa <- lsa(drug_lsa) 
drug_lsa_textmat <- as.textmatrix(drug_lsa) 
#Find words closely related to korea
opioid <- associate(drug_lsa_textmat, 'opioid', 'cosine', threshold = .4)
fentanyl <- associate(drug_lsa_textmat, 'fentanyl', 'cosine', threshold = .6)
heroin <- associate(drug_lsa_textmat, 'heroin', 'cosine', threshold = .4)
trafficking <- associate(drug_lsa_textmat, 'trafficking', 'cosine', threshold = .4)
ohio <- associate(drug_lsa_textmat, 'ohio', 'cosine', threshold = .4)
virginia <- associate(drug_lsa_textmat, 'virginia', 'cosine', threshold = .4)
crisis <- associate(drug_lsa_textmat, 'crisis', 'cosine', threshold = .4)
marijuana <- associate(drug_lsa_textmat, 'marijuana', 'cosine', threshold = .4)
methamphetamine <- associate(drug_lsa_textmat, 'methamphetamine', 'cosine', threshold = .4)

marijuana[1:10]
crisis[1:10]
opioid[1:10]
fentanyl[1:10]
heroin[1:10]
trafficking[1:10]
methamphetamine[1:10]
#Over the year?

for (i in 2:length(year_list)){
  year_data <- press_data[press_data$year == year_list[i],]
  drug_year <- year_data$contents
  drug_dfm <- dfm(drug_year, stem = T, remove_symbols = T, tolower = T,remove = stopwords('english'), remove_punct = T, remove_numbers = T)
  drug_dfm <- dfm_trim(drug_dfm, min_termfreq = 20, min_docfreq = 10)
  drug_lsa <- convert(drug_dfm, to = 'lsa') #Convert to lsa
  drug_lsa <- lw_logtf(drug_lsa) * gw_idf(drug_lsa)
  drug_lsa <- lsa(drug_lsa) 
  drug_lsa_textmat <- as.textmatrix(drug_lsa) 
  save(drug_lsa_textmat, file = paste0(year_list[i], 'drug_lsa_textmat1.RData'))
  
}

drug_keywords <- c('drug', 'opioid', 'heroin', 'fentanyl',
                   'overdose', 'crisis', 'ohio', 'virginia', 
                   'trafficking', 'marijuana', 'epidemic')

for (i in 10:length(year_list)){
  print(year_list[i])
  load(file=paste0(year_list[i], 'drug_lsa_textmat1.RData'))
  
  temp <- associate(drug_lsa_textmat, 'daly', 'cosine', threshold = .1)
  print(temp[1:min(length(temp), 20)])
  
}

#------------------------
## stucture topic models (DONE)
## possible drugs and criminal?labor -> Too little data
#-----------------------

drug_topic <- c("Opioids","Drug Trafficking")
drug_idx <- logical(0)  
for (i in 1:dim(tag_data)[1]){
  curr_data <- unlist(tag_data[i,]$topics)
  drug_idx[i] <- ('Opioids' %in% curr_data || "Drug Trafficking" %in% curr_data)
}
drug_data <- tag_data[drug_idx,]
drug_data$label <- 'drug'

crime_topic <- c('Violent Crime', "Hate Crimes")
crime_idx <- logical(0)
for (i in 1:dim(tag_data)[1]){
  curr_data <- unlist(tag_data[i,]$topics)
  crime_idx[i] <- ('Violent Crime' %in% curr_data ||  "Hate Crimes" %in% curr_data)
}
crime_data <- tag_data[crime_idx,]
crime_data$label <- 'crime'

labor_idx <- logical(0)
for (i in 1:dim(tag_data)[1]){
  curr_data <- unlist(tag_data[i,]$topics)
  labor_idx[i] <- ("Labor & Employment" %in% curr_data)
}
labor_data <- tag_data[labor_idx,]
labor_data$label <- 'labor'


stm_data <- rbind(drug_data, crime_data)
small_meta <- stm_data%>% select(label, year) %>% mutate(year =  as.integer(year))

tokens <- as.list(tokens_wordstem(tokens(char_tolower(stm_data$contents), 
                                         remove_punct = T, remove_numbers = T)))
it <- itoken(tokens, progressbar = FALSE)
small_vocab <- create_vocabulary(it, stopwords = stopwords('english'))
small_vocab <- prune_vocabulary(small_vocab, term_count_min = 15)
small_vocab <- small_vocab$term

stm_dfm <- dfm(stm_data$contents, tolower = TRUE, remove_punct = TRUE, 
               remove_numbers = TRUE, stem = TRUE, select = small_vocab)

system.time(
  small_stm <- stm(stm_dfm, data= small_meta, content = ~label,
                   prevalence = ~label + s(year), K=0, init.type = 'Spectral' ))

save(small_stm2, file = 'stm_crime_labor.RData')
plot(small_stm, type="perspectives", topics = c(11))
plot(small_stm, type = 'summary')
plot(small_stm2, type = 'summary')
plot(small_stm2, type="perspectives", topics = c(3))
ef_data <- estimateEffect(c(11) ~ s(date) , small_stm2, meta = small_meta2)


plot(small_stm, type = 'summary')
plot(small_stm, type = 'label', topics = c(44))
plot(small_stm, type="perspectives", topics = c(59))
ef_data <- estimateEffect(c(44) ~(year) , small_stm, meta = small_meta)
plot(ef_data, "year", small_stm, topics = c(44), 
     method = "continuous", xaxt = "n", xlab = "Date")