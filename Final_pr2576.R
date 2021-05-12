# Text As Data - Final paper
# Uni: pr2576
# How aligned to the World leader speeches, are topics of the Journal of International Affairs
# ---------------------------------------------------------------------------------------------------------------                
# Install and Load Packages
  library(ggplot2)
  library(ggcorrplot)
  library(quanteda)
  library(quanteda.textmodels)
  library(sentimentr)
  library(caret)
  library(randomForest)
  library(rpart)
  library(rpart.plot)
  library(stm)
  library(dotwhisker)
  install.packages("lubridate")
  library(lubridate)

# ---------------------------------------------------------------------------------------------------------------                
# 0: Outlining steps
  setwd("C:/Users/Pranav Ramkumar/Desktop/Quant Series/TAD/PS/Final")
  base::load("Speeches.rdata")
  dim(data2)
  head(Speeches)
  summary(Speeches$Designation)
  table(Speeches$Designation)

  base::load("Editorials.rdata")
  dim(Editorials)
  head(Editorials)
# ---------------------------------------------------------------------------------------------------------------                
# Q1: Are the vocabularies of JIA Editors and World Leaders zipfian?
  #JIA Editors
  Editorials_corpus = corpus(Editorials, text_field = "Editorial")
  Editorials_dfm = dfm(Editorials_corpus, tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE,
                 remove = stopwords("english"))
  freqs = colSums(Editorials_dfm)
  words = colnames(Editorials_dfm)
  wordlist = data.frame(words, freqs)
  wordIndexes = order(wordlist[, "freqs"], decreasing = TRUE)
  wordlist = wordlist[wordIndexes, ]
  head(wordlist, 10)
  plot(wordlist$freqs , type = "l", lwd=2, main = "Rank frequency Plot", xlab="Rank", ylab ="Frequency")
  plot(wordlist$freqs , type = "l", log="xy", lwd=2, main = "Rank frequency Plot (Logged)", xlab="log-Rank", ylab ="log-Frequency")
  #World Leaders (as a group)
  Speeches_corpus = corpus(Speeches, text_field = "Text_English")
  Speeches_dfm = dfm(Speeches_corpus, tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE,
                 remove = stopwords("english"))
  freqs = colSums(Speeches_dfm)
  words = colnames(Speeches_dfm)
  wordlist = data.frame(words, freqs)
  wordIndexes = order(wordlist[, "freqs"], decreasing = TRUE)
  wordlist = wordlist[wordIndexes, ]
  head(wordlist, 10)
  plot(wordlist$freqs , type = "l", lwd=2, main = "Rank frequency Plot", xlab="Rank", ylab ="Frequency")
  plot(wordlist$freqs , type = "l", log="xy", lwd=2, main = "Rank frequency Plot (Logged)", xlab="log-Rank", ylab ="log-Frequency")
  #US Presidents
  Speeches_US_subset = Speeches[Speeches$Designation %in% c("President (United States)"),]
  Speeches_US_corpus = corpus(Speeches_US_subset, text_field = "Text_English")
  Speeches_US_dfm = dfm(Speeches_US_corpus, tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE,
                 remove = stopwords("english"))
  freqs = colSums(Speeches_US_dfm)
  words = colnames(Speeches_US_dfm)
  wordlist = data.frame(words, freqs)
  wordIndexes = order(wordlist[, "freqs"], decreasing = TRUE)
  wordlist = wordlist[wordIndexes, ]
  head(wordlist, 10)
  plot(wordlist$freqs , type = "l", lwd=2, main = "Rank frequency Plot", xlab="Rank", ylab ="Frequency")
  plot(wordlist$freqs , type = "l", log="xy", lwd=2, main = "Rank frequency Plot (Logged)", xlab="log-Rank", ylab ="log-Frequency")
  #German Chancellors
  Speeches_DE_subset = Speeches[Speeches$Designation %in% c("Chancellor (Germany)"),]
  Speeches_DE_corpus = corpus(Speeches_DE_subset, text_field = "Text_English")
  Speeches_DE_dfm = dfm(Speeches_DE_corpus, tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE,
                 remove = stopwords("english"))
  freqs = colSums(Speeches_DE_dfm)
  words = colnames(Speeches_DE_dfm)
  wordlist = data.frame(words, freqs)
  wordIndexes = order(wordlist[, "freqs"], decreasing = TRUE)
  wordlist = wordlist[wordIndexes, ]
  head(wordlist, 10)
  plot(wordlist$freqs , type = "l", lwd=2, main = "Rank frequency Plot", xlab="Rank", ylab ="Frequency")
  plot(wordlist$freqs , type = "l", log="xy", lwd=2, main = "Rank frequency Plot (Logged)", xlab="log-Rank", ylab ="log-Frequency")
  #UK Prime Ministers
  Speeches_UK_subset = Speeches[Speeches$Designation %in% c("Prime Minister (United Kingdom)"),]
  Speeches_UK_corpus = corpus(Speeches_UK_subset, text_field = "Text_English")
  Speeches_UK_dfm = dfm(Speeches_UK_corpus, tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE,
                 remove = stopwords("english"))
  freqs = colSums(Speeches_UK_dfm)
  words = colnames(Speeches_UK_dfm)
  wordlist = data.frame(words, freqs)
  wordIndexes = order(wordlist[, "freqs"], decreasing = TRUE)
  wordlist = wordlist[wordIndexes, ]
  head(wordlist, 10)
  plot(wordlist$freqs , type = "l", lwd=2, main = "Rank frequency Plot", xlab="Rank", ylab ="Frequency")
  plot(wordlist$freqs , type = "l", log="xy", lwd=2, main = "Rank frequency Plot (Logged)", xlab="log-Rank", ylab ="log-Frequency")
  #Secretary Generals
  Speeches_SG_subset = Speeches[Speeches$Designation %in% c("Secretary General (United Nations)"),]
  Speeches_SG_corpus = corpus(Speeches_SG_subset, text_field = "Text_English")
  Speeches_SG_dfm = dfm(Speeches_SG_corpus, tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE,
                 remove = stopwords("english"))
  freqs = colSums(Speeches_SG_dfm)
  words = colnames(Speeches_SG_dfm)
  wordlist = data.frame(words, freqs)
  wordIndexes = order(wordlist[, "freqs"], decreasing = TRUE)
  wordlist = wordlist[wordIndexes, ]
  head(wordlist, 10)
  plot(wordlist$freqs , type = "l", lwd=2, main = "Rank frequency Plot", xlab="Rank", ylab ="Frequency")
  plot(wordlist$freqs , type = "l", log="xy", lwd=2, main = "Rank frequency Plot (Logged)", xlab="log-Rank", ylab ="log-Frequency")
  #Presidents of the General Assembly
  Speeches_PGA_subset = Speeches[Speeches$Designation %in% c("President of the General Assembly"),]
  Speeches_PGA_corpus = corpus(Speeches_PGA_subset, text_field = "Text_English")
  Speeches_PGA_dfm = dfm(Speeches_PGA_corpus, tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE,
                 remove = stopwords("english"))
  freqs = colSums(Speeches_PGA_dfm)
  words = colnames(Speeches_PGA_dfm)
  wordlist = data.frame(words, freqs)
  wordIndexes = order(wordlist[, "freqs"], decreasing = TRUE)
  wordlist = wordlist[wordIndexes, ]
  head(wordlist, 10)
  plot(wordlist$freqs , type = "l", lwd=2, main = "Rank frequency Plot", xlab="Rank", ylab ="Frequency")
  plot(wordlist$freqs , type = "l", log="xy", lwd=2, main = "Rank frequency Plot (Logged)", xlab="log-Rank", ylab ="log-Frequency")
# ---------------------------------------------------------------------------------------------------------------                  
# Q2: What are the most popular words and bigrams?
  #JIA Editors
  Editorials_tokens = tokens(Editorials_corpus, what="word", remove_numbers = TRUE, 
                      remove_punct = TRUE, remove_url = TRUE, 
                      remove_symbols = TRUE)
  Editorials_tokens = tokens_tolower(Editorials_tokens)
  Editorials_tokens = tokens_wordstem(Editorials_tokens, language = "en")
  Editorials_tokens = tokens_select(Editorials_tokens, pattern = stopwords('en'), selection = 'remove')
  Editorials_dfm = dfm(Editorials_tokens)
  Editorials_dfm = dfm_trim(Editorials_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Editorials_dfm, 20)
  textplot_wordcloud(Editorials_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  Editorials_tokens_bigrams = tokens_ngrams(Editorials_tokens)
  Editorials_bigrams_dfm = dfm(Editorials_tokens_bigrams)
  Editorials_bigrams_dfm = dfm_trim(Editorials_bigrams_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Editorials_bigrams_dfm, 20)
  textplot_wordcloud(Editorials_bigrams_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  #World Leaders
  Speeches_tokens = tokens(Speeches_corpus, what="word", remove_numbers = TRUE, 
                             remove_punct = TRUE, remove_url = TRUE, 
                             remove_symbols = TRUE)
  Speeches_tokens = tokens_tolower(Speeches_tokens)
  Speeches_tokens = tokens_wordstem(Speeches_tokens, language = "en")
  Speeches_tokens = tokens_select(Speeches_tokens, pattern = stopwords('en'), selection = 'remove')
  Speeches_dfm = dfm(Speeches_tokens)
  Speeches_dfm = dfm_trim(Speeches_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Speeches_dfm, 20)
  textplot_wordcloud(Speeches_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  Speeches_tokens_bigrams = tokens_ngrams(Speeches_tokens)
  Speeches_bigrams_dfm = dfm(Speeches_tokens_bigrams)
  Speeches_bigrams_dfm = dfm_trim(Speeches_bigrams_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Speeches_bigrams_dfm, 20)
  textplot_wordcloud(Speeches_bigrams_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  #US Presidents
  Speeches_US_tokens = tokens(Speeches_US_corpus, what="word", remove_numbers = TRUE, 
                             remove_punct = TRUE, remove_url = TRUE, 
                             remove_symbols = TRUE)
  Speeches_US_tokens = tokens_tolower(Speeches_US_tokens)
  Speeches_US_tokens = tokens_wordstem(Speeches_US_tokens, language = "en")
  Speeches_US_tokens = tokens_select(Speeches_US_tokens, pattern = stopwords('en'), selection = 'remove')
  Speeches_US_dfm = dfm(Speeches_US_tokens)
  Speeches_US_dfm = dfm_trim(Speeches_US_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Speeches_US_dfm, 20)
  textplot_wordcloud(Speeches_US_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  Speeches_US_tokens_bigrams = tokens_ngrams(Speeches_US_tokens)
  Speeches_US_bigrams_dfm = dfm(Speeches_US_tokens_bigrams)
  Speeches_US_bigrams_dfm = dfm_trim(Speeches_US_bigrams_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Speeches_US_bigrams_dfm, 20)
  textplot_wordcloud(Speeches_US_bigrams_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  #German Chancellors
  Speeches_DE_tokens = tokens(Speeches_DE_corpus, what="word", remove_numbers = TRUE, 
                              remove_punct = TRUE, remove_url = TRUE, 
                              remove_symbols = TRUE)
  Speeches_DE_tokens = tokens_tolower(Speeches_DE_tokens)
  Speeches_DE_tokens = tokens_wordstem(Speeches_DE_tokens, language = "en")
  Speeches_DE_tokens = tokens_select(Speeches_DE_tokens, pattern = stopwords('en'), selection = 'remove')
  Speeches_DE_dfm = dfm(Speeches_DE_tokens)
  Speeches_DE_dfm = dfm_trim(Speeches_DE_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Speeches_DE_dfm, 20)
  textplot_wordcloud(Speeches_DE_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  Speeches_DE_tokens_bigrams = tokens_ngrams(Speeches_DE_tokens)
  Speeches_DE_bigrams_dfm = dfm(Speeches_DE_tokens_bigrams)
  Speeches_DE_bigrams_dfm = dfm_trim(Speeches_DE_bigrams_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Speeches_DE_bigrams_dfm, 20)
  textplot_wordcloud(Speeches_DE_bigrams_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  #UK Prime Ministers
  Speeches_UK_tokens = tokens(Speeches_UK_corpus, what="word", remove_numbers = TRUE, 
                              remove_punct = TRUE, remove_url = TRUE, 
                              remove_symbols = TRUE)
  Speeches_UK_tokens = tokens_tolower(Speeches_UK_tokens)
  Speeches_UK_tokens = tokens_wordstem(Speeches_UK_tokens, language = "en")
  Speeches_UK_tokens = tokens_select(Speeches_UK_tokens, pattern = stopwords('en'), selection = 'remove')
  Speeches_UK_dfm = dfm(Speeches_UK_tokens)
  Speeches_UK_dfm = dfm_trim(Speeches_UK_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Speeches_UK_dfm, 20)
  textplot_wordcloud(Speeches_UK_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  Speeches_UK_tokens_bigrams = tokens_ngrams(Speeches_UK_tokens)
  Speeches_UK_bigrams_dfm = dfm(Speeches_UK_tokens_bigrams)
  Speeches_UK_bigrams_dfm = dfm_trim(Speeches_UK_bigrams_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Speeches_UK_bigrams_dfm, 20)
  textplot_wordcloud(Speeches_UK_bigrams_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  #Secretary Generals
  Speeches_SG_tokens = tokens(Speeches_SG_corpus, what="word", remove_numbers = TRUE, 
                              remove_punct = TRUE, remove_url = TRUE, 
                              remove_symbols = TRUE)
  Speeches_SG_tokens = tokens_tolower(Speeches_SG_tokens)
  Speeches_SG_tokens = tokens_wordstem(Speeches_SG_tokens, language = "en")
  Speeches_SG_tokens = tokens_select(Speeches_SG_tokens, pattern = stopwords('en'), selection = 'remove')
  Speeches_SG_dfm = dfm(Speeches_SG_tokens)
  Speeches_SG_dfm = dfm_trim(Speeches_SG_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Speeches_SG_dfm, 20)
  textplot_wordcloud(Speeches_SG_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  Speeches_SG_tokens_bigrams = tokens_ngrams(Speeches_SG_tokens)
  Speeches_SG_bigrams_dfm = dfm(Speeches_SG_tokens_bigrams)
  Speeches_SG_bigrams_dfm = dfm_trim(Speeches_SG_bigrams_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Speeches_SG_bigrams_dfm, 20)
  textplot_wordcloud(Speeches_SG_bigrams_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  #Presidents of the General Assembly
  Speeches_PGA_tokens = tokens(Speeches_PGA_corpus, what="word", remove_numbers = TRUE, 
                              remove_punct = TRUE, remove_url = TRUE, 
                              remove_symbols = TRUE)
  Speeches_PGA_tokens = tokens_tolower(Speeches_PGA_tokens)
  Speeches_PGA_tokens = tokens_wordstem(Speeches_PGA_tokens, language = "en")
  Speeches_PGA_tokens = tokens_select(Speeches_PGA_tokens, pattern = stopwords('en'), selection = 'remove')
  Speeches_PGA_dfm = dfm(Speeches_PGA_tokens)
  Speeches_PGA_dfm = dfm_trim(Speeches_PGA_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Speeches_PGA_dfm, 20)
  textplot_wordcloud(Speeches_PGA_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
  Speeches_PGA_tokens_bigrams = tokens_ngrams(Speeches_PGA_tokens)
  Speeches_PGA_bigrams_dfm = dfm(Speeches_PGA_tokens_bigrams)
  Speeches_PGA_bigrams_dfm = dfm_trim(Speeches_PGA_bigrams_dfm, min_termfreq = 10, max_termfreq = 400)
  topfeatures(Speeches_PGA_bigrams_dfm, 20)
  textplot_wordcloud(Speeches_PGA_bigrams_dfm, min_count = 5, random_order = FALSE,
                     rotation = .25,color = RColorBrewer::brewer.pal(8, "Dark2"))
# ---------------------------------------------------------------------------------------------------------------                
# Q3: In what context are certain popular keywords used?
  # JIA Editors
  head(kwic(x=Editorials_tokens_bigrams, pattern="unit_state", window = 3))
  head(kwic(x=Editorials_tokens_bigrams, pattern="unit_nation", window = 3))
  head(kwic(x=Editorials_tokens_bigrams, pattern="soviet_union", window = 3))
  head(kwic(x=Editorials_tokens_bigrams, pattern="european_union", window = 3))
  head(kwic(x=Editorials_tokens_bigrams, pattern="middl_east", window = 3))
  head(kwic(x=Editorials_tokens_bigrams, pattern="develop_countri", window = 3))
  head(kwic(x=Editorials_tokens_bigrams, pattern="intern_affair", window = 3))
  head(kwic(x=Editorials_tokens_bigrams, pattern="peac_secur", window = 3))
  head(kwic(x=Editorials_tokens_bigrams, pattern="foreign_polici", window = 3))
  head(kwic(x=Editorials_tokens_bigrams, pattern="human_right", window = 3))
  head(kwic(x=Editorials_tokens_bigrams, pattern="econom_develop", window = 3))
  head(kwic(x=Editorials_tokens_bigrams, pattern="climat_chang", window = 3))
  # World Leaders
  head(kwic(x=Speeches_tokens_bigrams, pattern="unit_state", window = 3))
  head(kwic(x=Speeches_tokens_bigrams, pattern="unit_nation", window = 3))
  head(kwic(x=Speeches_tokens_bigrams, pattern="soviet_union", window = 3))
  head(kwic(x=Speeches_tokens_bigrams, pattern="european_union", window = 3))
  head(kwic(x=Speeches_tokens_bigrams, pattern="middl_east", window = 3))
  head(kwic(x=Speeches_tokens_bigrams, pattern="develop_countri", window = 3))
  head(kwic(x=Speeches_tokens_bigrams, pattern="intern_affair", window = 3))
  head(kwic(x=Speeches_tokens_bigrams, pattern="peac_secur", window = 3))
  head(kwic(x=Speeches_tokens_bigrams, pattern="foreign_polici", window = 3))
  head(kwic(x=Speeches_tokens_bigrams, pattern="human_right", window = 3))
  head(kwic(x=Speeches_tokens_bigrams, pattern="econom_develop", window = 3))
  head(kwic(x=Speeches_tokens_bigrams, pattern="climat_chang", window = 3))
  # US Presidents
  head(kwic(x=Speeches_US_tokens_bigrams, pattern="unit_state", window = 3))
  head(kwic(x=Speeches_US_tokens_bigrams, pattern="unit_nation", window = 3))
  head(kwic(x=Speeches_US_tokens_bigrams, pattern="soviet_union", window = 3))
  head(kwic(x=Speeches_US_tokens_bigrams, pattern="european_union", window = 3))
  head(kwic(x=Speeches_US_tokens_bigrams, pattern="middl_east", window = 3))
  head(kwic(x=Speeches_US_tokens_bigrams, pattern="develop_countri", window = 3))
  head(kwic(x=Speeches_US_tokens_bigrams, pattern="intern_affair", window = 3))
  head(kwic(x=Speeches_US_tokens_bigrams, pattern="peac_secur", window = 3))
  head(kwic(x=Speeches_US_tokens_bigrams, pattern="foreign_polici", window = 3))
  head(kwic(x=Speeches_US_tokens_bigrams, pattern="human_right", window = 3))
  head(kwic(x=Speeches_US_tokens_bigrams, pattern="econom_develop", window = 3))
  head(kwic(x=Speeches_US_tokens_bigrams, pattern="climat_chang", window = 3))
  # German Chancellors
  head(kwic(x=Speeches_DE_tokens_bigrams, pattern="unit_state", window = 3))
  head(kwic(x=Speeches_DE_tokens_bigrams, pattern="unit_nation", window = 3))
  head(kwic(x=Speeches_DE_tokens_bigrams, pattern="soviet_union", window = 3))
  head(kwic(x=Speeches_DE_tokens_bigrams, pattern="european_union", window = 3))
  head(kwic(x=Speeches_DE_tokens_bigrams, pattern="middl_east", window = 3))
  head(kwic(x=Speeches_DE_tokens_bigrams, pattern="develop_countri", window = 3))
  head(kwic(x=Speeches_DE_tokens_bigrams, pattern="intern_affair", window = 3))
  head(kwic(x=Speeches_DE_tokens_bigrams, pattern="peac_secur", window = 3))
  head(kwic(x=Speeches_DE_tokens_bigrams, pattern="foreign_polici", window = 3))
  head(kwic(x=Speeches_DE_tokens_bigrams, pattern="human_right", window = 3))
  head(kwic(x=Speeches_DE_tokens_bigrams, pattern="econom_develop", window = 3))
  head(kwic(x=Speeches_DE_tokens_bigrams, pattern="climat_chang", window = 3))
  # UK Prime Ministers
  head(kwic(x=Speeches_UK_tokens_bigrams, pattern="unit_state", window = 3))
  head(kwic(x=Speeches_UK_tokens_bigrams, pattern="unit_nation", window = 3))
  head(kwic(x=Speeches_UK_tokens_bigrams, pattern="soviet_union", window = 3))
  head(kwic(x=Speeches_UK_tokens_bigrams, pattern="european_union", window = 3))
  head(kwic(x=Speeches_UK_tokens_bigrams, pattern="middl_east", window = 3))
  head(kwic(x=Speeches_UK_tokens_bigrams, pattern="develop_countri", window = 3))
  head(kwic(x=Speeches_UK_tokens_bigrams, pattern="intern_affair", window = 3))
  head(kwic(x=Speeches_UK_tokens_bigrams, pattern="peac_secur", window = 3))
  head(kwic(x=Speeches_UK_tokens_bigrams, pattern="foreign_polici", window = 3))
  head(kwic(x=Speeches_UK_tokens_bigrams, pattern="human_right", window = 3))
  head(kwic(x=Speeches_UK_tokens_bigrams, pattern="econom_develop", window = 3))
  head(kwic(x=Speeches_UK_tokens_bigrams, pattern="climat_chang", window = 3))
  # Secretary Generals
  head(kwic(x=Speeches_SG_tokens_bigrams, pattern="unit_state", window = 3))
  head(kwic(x=Speeches_SG_tokens_bigrams, pattern="unit_nation", window = 3))
  head(kwic(x=Speeches_SG_tokens_bigrams, pattern="soviet_union", window = 3))
  head(kwic(x=Speeches_SG_tokens_bigrams, pattern="european_union", window = 3))
  head(kwic(x=Speeches_SG_tokens_bigrams, pattern="middl_east", window = 3))
  head(kwic(x=Speeches_SG_tokens_bigrams, pattern="develop_countri", window = 3))
  head(kwic(x=Speeches_SG_tokens_bigrams, pattern="intern_affair", window = 3))
  head(kwic(x=Speeches_SG_tokens_bigrams, pattern="peac_secur", window = 3))
  head(kwic(x=Speeches_SG_tokens_bigrams, pattern="foreign_polici", window = 3))
  head(kwic(x=Speeches_SG_tokens_bigrams, pattern="human_right", window = 3))
  head(kwic(x=Speeches_SG_tokens_bigrams, pattern="econom_develop", window = 3))
  head(kwic(x=Speeches_SG_tokens_bigrams, pattern="climat_chang", window = 3))
  # Presidents of the General Assembly
  head(kwic(x=Speeches_PGA_tokens_bigrams, pattern="unit_state", window = 3))
  head(kwic(x=Speeches_PGA_tokens_bigrams, pattern="unit_nation", window = 3))
  head(kwic(x=Speeches_PGA_tokens_bigrams, pattern="soviet_union", window = 3))
  head(kwic(x=Speeches_PGA_tokens_bigrams, pattern="european_union", window = 3))
  head(kwic(x=Speeches_PGA_tokens_bigrams, pattern="middl_east", window = 3))
  head(kwic(x=Speeches_PGA_tokens_bigrams, pattern="develop_countri", window = 3))
  head(kwic(x=Speeches_PGA_tokens_bigrams, pattern="intern_affair", window = 3))
  head(kwic(x=Speeches_PGA_tokens_bigrams, pattern="peac_secur", window = 3))
  head(kwic(x=Speeches_PGA_tokens_bigrams, pattern="foreign_polici", window = 3))
  head(kwic(x=Speeches_PGA_tokens_bigrams, pattern="human_right", window = 3))
  head(kwic(x=Speeches_PGA_tokens_bigrams, pattern="econom_develop", window = 3))
  head(kwic(x=Speeches_PGA_tokens_bigrams, pattern="climat_chang", window = 3))
  
# ---------------------------------------------------------------------------------------------------------------              
# Q4: What features co-occur in each corpus? 
  # JIA Editorials
  Editorials_fcm = fcm(Editorials_dfm, context = "document", count="frequency")
  dim(Editorials_fcm)
  head(Editorials_fcm)
  features = names(topfeatures(Editorials_fcm, 50))
  fcm_select(Editorials_fcm, pattern = features) %>%
    textplot_network(min_freq = 0.9, edge_size = 2, edge_color = "#d14a1d", vertex_color = "#d11d1d", vertex_labelcolor = "#000000")
  # World Leaders Speeches
  Speeches_fcm = fcm(Speeches_dfm, context = "document", count="frequency")
  dim(Speeches_fcm)
  head(Speeches_fcm)
  features = names(topfeatures(Speeches_fcm, 50))
  fcm_select(Speeches_fcm, pattern = features) %>%
    textplot_network(min_freq = 0.9, edge_size = 2, edge_color = "#d14a1d", vertex_color = "#d11d1d", vertex_labelcolor = "#000000")
# ---------------------------------------------------------------------------------------------------------------            
# Q5: Is there any similarity between the documents?
  #JIA Editorials
  cosine_similarities = textstat_simil(x = Editorials_dfm, method = "cosine", margin = "documents")
  dim(cosine_similarities)
  rownames(cosine_similarities) = 1:nrow(cosine_similarities)
  colnames(cosine_similarities) = 1:ncol(cosine_similarities)
  ggcorrplot(as.matrix(cosine_similarities)) + scale_fill_gradient2(limit = c(0,1))
  df2 = as.data.frame(cosine_similarities)
  df2[which.max(df2$cosine),]
  #World Leader Speeches
  cosine_similarities = textstat_simil(x = Speeches_dfm, method = "cosine", margin = "documents")
  dim(cosine_similarities)
  rownames(cosine_similarities) = 1:nrow(cosine_similarities)
  colnames(cosine_similarities) = 1:ncol(cosine_similarities)
  ggcorrplot(as.matrix(cosine_similarities)) + scale_fill_gradient2(limit = c(0,1))
  df2 = as.data.frame(cosine_similarities)
  df2[which.max(df2$cosine),]
# ---------------------------------------------------------------------------------------------------------------          
# Q6: Which group of leaders are most about "peace, security, fairness, equality and happiness"?
  my_dictionary = c("peace security fairness equality happiness")
  CleanDictionary = function(x){
    x = tokens(x,  remove_numbers = TRUE, remove_punct = TRUE)
    x = tokens_wordstem(x)
    x = tokens_select(x, pattern = stopwords('en'), selection = 'remove')
    x = tokens_tolower(x)
    return(as.character(x)) 
  }
  my_dictionary_cleaned = CleanDictionary(my_dictionary)
  
  #Who talks more about the issue?
  #JIA Editors on the issue
  Editorials$Designation <- "Editor"
  Editorials_corpus = corpus(Editorials, text_field = "Editorial")
  Editorials_dfm = dfm(Editorials_corpus, tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE,
                       remove = stopwords("english"))
  JIA_on_the_issue = sum(Editorials_dfm[docvars(Editorials_dfm)$Designation == "Editor", colnames(Editorials_dfm) %in% my_dictionary_cleaned])
  JIA_total_words = sum(Editorials_dfm[docvars(Editorials_dfm)$Designation == "Editor",])
  JIA_prop_on_issue = JIA_on_the_issue/JIA_total_words
  
  #US Presidents on the issue
  US_on_the_issue = sum(Speeches_dfm[docvars(Speeches_dfm)$Designation == "President (United States)", colnames(Speeches_dfm) %in% my_dictionary_cleaned])
  US_total_words = sum(Speeches_dfm[docvars(Speeches_dfm)$Designation == "President (United States)",])
  US_prop_on_issue = US_on_the_issue/US_total_words
  
  #German Chancellors on the issue
  DE_on_the_issue = sum(Speeches_dfm[docvars(Speeches_dfm)$Designation == "Chancellor (Germany)", colnames(Speeches_dfm) %in% my_dictionary_cleaned])
  DE_total_words = sum(Speeches_dfm[docvars(Speeches_dfm)$Designation == "Chancellor (Germany)",])
  DE_prop_on_issue = DE_on_the_issue/DE_total_words
  
  #UK Prime Ministers on the issue
  UK_on_the_issue = sum(Speeches_dfm[docvars(Speeches_dfm)$Designation == "Prime Minister (United Kingdom)", colnames(Speeches_dfm) %in% my_dictionary_cleaned])
  UK_total_words = sum(Speeches_dfm[docvars(Speeches_dfm)$Designation == "Prime Minister (United Kingdom)",])
  UK_prop_on_issue = UK_on_the_issue/UK_total_words
  
  #Secretary General on the issue
  SG_on_the_issue = sum(Speeches_dfm[docvars(Speeches_dfm)$Designation == "Secretary General (United Nations)", colnames(Speeches_dfm) %in% my_dictionary_cleaned])
  SG_total_words = sum(Speeches_dfm[docvars(Speeches_dfm)$Designation == "Secretary General (United Nations)",])
  SG_prop_on_issue = SG_on_the_issue/SG_total_words
  
  #PGAs on the issue
  PGA_on_the_issue = sum(Speeches_dfm[docvars(Speeches_dfm)$Designation == "President of the General Assembly", colnames(Speeches_dfm) %in% my_dictionary_cleaned])
  PGA_total_words = sum(Speeches_dfm[docvars(Speeches_dfm)$Designation == "President of the General Assembly",])
  PGA_prop_on_issue = PGA_on_the_issue/PGA_total_words
  
  results = data.frame(network = c("JIA", "US", "DE", "UK", "SG", "PGA"), 
                       prop_on_issue = c(JIA_prop_on_issue, US_prop_on_issue, DE_prop_on_issue,
                                         UK_prop_on_issue, SG_prop_on_issue, PGA_prop_on_issue))
  ## Plot the results
  ggplot(results, aes(x=reorder(network, -prop_on_issue), y=prop_on_issue))+
    geom_bar(stat="identity") + theme_minimal() + xlab("Category") + 
    ylab("Attention to issue (proportion)")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
# ---------------------------------------------------------------------------------------------------------------          
# Q7: What are the sentiments and emotions expressed in the world leader speeches and JIA editorials?
# Uses the Jockers, M. L. (2017). Syuzhet sentiment dictionary
  #JIA Editors
  base::load("sentiment_dictionary.rdata")
  Editorials_text = get_sentences(Editorials$Editorial)
  Editorials_sentiment = sentiment_by(Editorials_text, by=Editorials$Designation)
  plot(Editorials_sentiment) 
  ggplot(Editorials_sentiment, aes(x=Designation, y=ave_sentiment, fill=Designation))+
    geom_bar(position="dodge",stat="identity") + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  Editorials_emotion = emotion_by(Editorials_text, by=Editorials$Designation)
  ggplot(Editorials_emotion, aes(x=emotion_type, y=ave_emotion, fill=Designation))+
    geom_bar(position="dodge",stat="identity") + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  #World Leaders
  Speeches_text = get_sentences(Speeches$Text_English)
  Speeches_sentiment = sentiment_by(Speeches_text, by=Speeches$Designation)
  plot(Speeches_sentiment) 
  ggplot(Speeches_sentiment, aes(x=Designation, y=ave_sentiment, fill=Designation))+
    geom_bar(position="dodge",stat="identity") + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
  Speeches_emotion = emotion_by(Speeches_text, by=Speeches$Designation)
  ggplot(Speeches_emotion, aes(x=emotion_type, y=ave_emotion, fill=Designation))+
    geom_bar(position="dodge",stat="identity") + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ---------------------------------------------------------------------------------------------------------------          
# Q8: Can a Machine Learning model accurately identify a JIA editorial from a World leader Speech? Which model performs best?
  dataset1 <- data.frame("text" = Editorials$Editorial, "editor" = 1)
  dataset2 <- data.frame("text" = Speeches$Text_English, "editor" = 0)
  dataset <- rbind(dataset1, dataset2)
  
  #Explore and set up training and testing data sets
  dataset[1,]
  summary(dataset$editor)
  table(dataset$editor) 
  data_corp = corpus(dataset)
  docvars(data_corp, "id_numeric") = 1:ndoc(data_corp)
  summary(data_corp)
  set.seed(12345)
  # Random 200 into training dataset
  id_train = sample(x=1:nrow(dataset), 200, replace = FALSE)
  dfmat_training = corpus_subset(data_corp, id_numeric %in% id_train) %>%
    dfm(stem = TRUE, remove = stopwords("english"),
        remove_punct = TRUE, remove_numbers = TRUE, tolower=TRUE) %>%
    dfm_trim(min_termfreq = 2, max_termfreq = 300)
  dim(dfmat_training)
  # Remaining 91 in the testing dataset
  dfmat_test = corpus_subset(data_corp, !id_numeric %in% id_train) %>%
    dfm(stem = TRUE, remove = stopwords("english"),
        remove_punct = TRUE, remove_numbers = TRUE, tolower=TRUE)  %>%
    dfm_trim(min_termfreq = 2, max_termfreq = 300)
  dim(dfmat_test)
  
  
  #NAIVE BAYES
  classify_nb = textmodel_nb(x = dfmat_training, y = docvars(dfmat_training, "editor"))
  summary(classify_nb, n=20)
  dfmat_matched = dfm_match(dfmat_test, features = featnames(dfmat_training))
  dim(dfmat_matched)
  predicted_class = predict(classify_nb, newdata = dfmat_matched, type="class")
  actual_class = docvars(dfmat_matched, "editor")
  tab_class = table(predicted_class, actual_class)
  confusionMatrix(tab_class, mode = "everything")
  
  #DECISION TREE
  dfmat_training_df = convert(dfmat_training, to="data.frame")
  dfmat_test_df = convert(dfmat_matched, to="data.frame")
  dfmat_training_df$editor <- as.factor(docvars(dfmat_training, "editor"))
  colnames(dfmat_training_df) = make.names(colnames(dfmat_training_df))
  colnames(dfmat_test_df) = make.names(colnames(dfmat_test_df))
  classify_tree = rpart(formula = editor ~ ., data=dfmat_training_df[,!colnames(dfmat_training_df) == "doc_id"])
  predicted_class = predict(classify_tree, newdata = dfmat_test_df[,!colnames(dfmat_test_df) == "doc_id"], type="class")
  tab_class = table(predicted_class, actual_class)
  confusionMatrix(tab_class, mode = "everything")
  rpart.plot(classify_tree, roundint=F)
  
  #RANDOM FOREST
  colnames(dfmat_training_df) = make.names(colnames(dfmat_training_df))
  colnames(dfmat_test_df) = make.names(colnames(dfmat_test_df))
  classify_rf = randomForest(formula = editor ~ ., data=dfmat_training_df[,!colnames(dfmat_training_df) == "doc_id"], nTree = 10)
  predicted_class = predict(classify_rf, newdata = dfmat_test_df[,!colnames(dfmat_test_df) == "doc_id"], type="class")
  tab_class_rf = table(predicted_class, actual_class)
  confusionMatrix(tab_class_rf, mode = "everything")
  
# ---------------------------------------------------------------------------------------------------------------          
# Q9a: What topic are expressed in the JIA Editorials corpus?
  Editorials_corpus = corpus(Editorials,
                             text_field = "Editorial")
  Editorials_dfm = dfm(Editorials_corpus, stem = TRUE, remove = stopwords("english"),
                       remove_punct = TRUE, remove_numbers = TRUE, tolower=TRUE) 
  Editorials_dfm = dfm_trim(Editorials_dfm, min_termfreq = 2, max_termfreq = 300)
  # LDA Topic Model
  out_topics = stm(Editorials_dfm, K = 10, init.type = "Spectral")
  labelTopics(out_topics, n=7)
  plot(out_topics, type = "summary", xlim = c(0, 1), n=8, labeltype = "frex")
  examples_topic = findThoughts(out_topics, texts = Editorials$Editorial, n = 1, topics = 10)
  examples_topic
# ---------------------------------------------------------------------------------------------------------------
# Q9b: What topic are expressed in the World Leader Speeches corpus?
  table(Speeches$Designation)
  Speeches_corpus = corpus(Speeches,
                           text_field = "Text_English")
  Speeches_dfm = dfm(Speeches_corpus, stem = TRUE, remove = stopwords("english"),
                         remove_punct = TRUE, remove_numbers = TRUE, tolower=TRUE) 
  Speeches_dfm = dfm_trim(Speeches_dfm, min_termfreq = 2, max_termfreq = 300)
  # LDA Topic Model
  out_topics = stm(Speeches_dfm, K = 10, init.type = "Spectral")
  labelTopics(out_topics, n=7)
  plot(out_topics, type = "summary", xlim = c(0, 1), n=8, labeltype = "frex")
  examples_topic = findThoughts(out_topics, texts = Speeches$Text_English, n = 1, topics = 9)
  examples_topic
# ---------------------------------------------------------------------------------------------------------------          
# Q9c: What do US Presidents talk about?  
  Speeches_US_subset = Speeches[Speeches$Designation %in% c("President (United States)"),]
  Speeches_US_corpus = corpus(Speeches_US_subset,
                           text_field = "Text_English")
  Speeches_US_dfm = dfm(Speeches_US_corpus, stem = TRUE, remove = stopwords("english"),
                     remove_punct = TRUE, remove_numbers = TRUE, tolower=TRUE) 
  Speeches_US_dfm = dfm_trim(Speeches_US_dfm, min_termfreq = 2, max_termfreq = 300)
  # LDA Topic Model
  out_topics = stm(Speeches_US_dfm, K = 5, init.type = "Spectral")
  labelTopics(out_topics, n=7)
  plot(out_topics, type = "summary", xlim = c(0, 1), n=8, labeltype = "frex")
  examples_topic = findThoughts(out_topics, texts = Speeches_US_subset$Text_English, n = 1, topics = 5)
  examples_topic
# ---------------------------------------------------------------------------------------------------------------          
# Q9d: What do German Chancellors talk about?  
  Speeches_DE_subset = Speeches[Speeches$Designation %in% c("Chancellor (Germany)"),]
  Speeches_DE_corpus = corpus(Speeches_DE_subset,
                              text_field = "Text_English")
  Speeches_DE_dfm = dfm(Speeches_DE_corpus, stem = TRUE, remove = stopwords("english"),
                        remove_punct = TRUE, remove_numbers = TRUE, tolower=TRUE) 
  Speeches_DE_dfm = dfm_trim(Speeches_DE_dfm, min_termfreq = 2, max_termfreq = 300)
  # LDA Topic Model
  out_topics = stm(Speeches_DE_dfm, K = 5, init.type = "Spectral")
  labelTopics(out_topics, n=7)
  plot(out_topics, type = "summary", xlim = c(0, 1), n=8, labeltype = "frex")
  examples_topic = findThoughts(out_topics, texts = Speeches_DE_subset$Text_English, n = 1, topics = 5)
  examples_topic
# ---------------------------------------------------------------------------------------------------------------        
# Q9e: What do British Prime Ministers talk about?  
  Speeches_UK_subset = Speeches[Speeches$Designation %in% c("Prime Minister (United Kingdom)"),]
  Speeches_UK_corpus = corpus(Speeches_UK_subset,
                              text_field = "Text_English")
  Speeches_UK_dfm = dfm(Speeches_UK_corpus, stem = TRUE, remove = stopwords("english"),
                        remove_punct = TRUE, remove_numbers = TRUE, tolower=TRUE) 
  Speeches_UK_dfm = dfm_trim(Speeches_UK_dfm, min_termfreq = 2, max_termfreq = 300)
  # LDA Topic Model
  out_topics = stm(Speeches_UK_dfm, K = 5, init.type = "Spectral")
  labelTopics(out_topics, n=7)
  plot(out_topics, type = "summary", xlim = c(0, 1), n=8, labeltype = "frex")
  examples_topic = findThoughts(out_topics, texts = Speeches_UK_subset$Text_English, n = 1, topics = 5)
  examples_topic
# ---------------------------------------------------------------------------------------------------------------        
# Q9f: What do Secretary Generals talk about?  
  Speeches_SG_subset = Speeches[Speeches$Designation %in% c("Secretary General (United Nations)"),]
  Speeches_SG_corpus = corpus(Speeches_SG_subset,
                              text_field = "Text_English")
  Speeches_SG_dfm = dfm(Speeches_SG_corpus, stem = TRUE, remove = stopwords("english"),
                        remove_punct = TRUE, remove_numbers = TRUE, tolower=TRUE) 
  Speeches_SG_dfm = dfm_trim(Speeches_SG_dfm, min_termfreq = 2, max_termfreq = 300)
  # LDA Topic Model
  out_topics = stm(Speeches_SG_dfm, K = 5, init.type = "Spectral")
  labelTopics(out_topics, n=7)
  plot(out_topics, type = "summary", xlim = c(0, 1), n=8, labeltype = "frex")
  examples_topic = findThoughts(out_topics, texts = Speeches_SG_subset$Text_English, n = 1, topics = 4)
  examples_topic
# ---------------------------------------------------------------------------------------------------------------      
# Q9g: What do Presidents of the General Assembly talk about?  
  Speeches_PGA_subset = Speeches[Speeches$Designation %in% c("President of the General Assembly"),]
  Speeches_PGA_corpus = corpus(Speeches_PGA_subset,
                              text_field = "Text_English")
  Speeches_PGA_dfm = dfm(Speeches_PGA_corpus, stem = TRUE, remove = stopwords("english"),
                        remove_punct = TRUE, remove_numbers = TRUE, tolower=TRUE) 
  Speeches_PGA_dfm = dfm_trim(Speeches_PGA_dfm, min_termfreq = 2, max_termfreq = 300)
  # LDA Topic Model
  out_topics = stm(Speeches_PGA_dfm, K = 5, init.type = "Spectral")
  labelTopics(out_topics, n=7)
  plot(out_topics, type = "summary", xlim = c(0, 1), n=8, labeltype = "frex")
  examples_topic = findThoughts(out_topics, texts = Speeches_PGA_subset$Text_English, n = 1, topics = 5)
  examples_topic
# ---------------------------------------------------------------------------------------------------------------      
# Q10: How do the topics, sentiments and emotions of World Leader Speeches vary over time?
  Speeches$date1 <- year <- ymd(sprintf("%d-01-01",Speeches$Date))
  Speeches$date_num = as.numeric(Speeches$date1)
  Speeches_corpus = corpus(Speeches, text_field = "Text_English")
  Speeches_dfm = dfm(Speeches_corpus, tolower = TRUE, remove_punct = TRUE, remove_numbers = TRUE, stem = TRUE,
                     remove = stopwords("english"))
  Speeches_dfm = dfm_trim(Speeches_dfm, min_termfreq = 2, max_termfreq = 300)
  sentiment = sentiment_by(Speeches$Text_English)
  summary(sentiment$ave_sentiment)
  Speeches$ave_sentiment = sentiment$ave_sentiment
  #STM Topic Model
  out_topics_prevalence = stm(Speeches_dfm, K = 5, prevalence= ~ Designation + date_num, init.type = "Spectral" )
  labelTopics(out_topics_prevalence, n=7)
  plot(out_topics_prevalence, type = "summary", xlim = c(0, 1), n=5, labeltype = "frex")
  # Name topics
  topic_names = c("1. MDGs and Global Summits", "2. International Cooperation", "3. Function and Democratic Process", "4. US-Germany bilateralism", "5. UK's relationship with the EU and the commonwealth")
  # Bridging code after STM before estimate effect
  row_totals = apply(Speeches_dfm, 1, sum)
  Speeches_dfm = Speeches_dfm[row_totals>0,]
  # Estimate effect
  content_Sentiment = estimateEffect(formula = 1:5 ~ Designation + date_num, stmobj = out_topics_prevalence, 
                                     metadata = docvars(Speeches_dfm), uncertainty = "Global")
  summary(content_Sentiment)
  #Visualization of the regression results
  estimate = rep(NA, 5)
  std.error = rep(NA, 5)
  for(i in 1:length(summary(content_Sentiment)$tables)){
    estimate[i] = summary(content_Sentiment)$tables[[i]][2,1]
    std.error[i] = summary(content_Sentiment)$tables[[i]][2,2]
  }
  stm_results = data.frame("term" = topic_names, "estimate" = estimate, "std.error"= std.error)
  #Viz1 - DW Plot
  dwplot(stm_results, 
         vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(size = 2.5)) +
    theme_bw() + xlab("Negative . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  . . . . . . . . Positive") +
    theme(text = element_text(size=12), axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "none") +
    scale_colour_grey(start = 0, end = 0)
  #Viz2 - DW Plot
  plot(x=content_Sentiment, covariate="date_num", method = "continuous", topics = 1:5, 
       model=out_topics_prevalence, xaxt="n")
  axis(1, at = Speeches$date_num, labels = Speeches$date1)
# ---------------------------------------------------------------------------------------------------------------
