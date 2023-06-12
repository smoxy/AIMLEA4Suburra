################################################################################
#####                                                                      #####
#####                             PRE-PROCESSING                           #####
#####                                                                      #####
################################################################################
install.packages("parallel",Ncpus = 4)
library(parallel)
cores<-(as.numeric(parallel::detectCores()-1))
HOME <- "/mnt/volume_fra1_01/AIMLEA4Suburra/R"
#HOME <- "/home/smoxy/AIMLEA4Suburra/R/"

################################################################################
#####                        Functions for packages                        #####
installAndLoadPackages <- function(packageNames, cores) {
  # Convert packageNames to a character vector if it's a single package name
  if (is.character(packageNames)) {
    packageNames <- as.character(packageNames)
  }
  
  # Check if each package is already installed
  notInstalledPackages <- packageNames[!sapply(packageNames, requireNamespace, quietly = TRUE)]
  
  # Install any packages that are not already installed
  if (length(notInstalledPackages) > 0) {
    install.packages(notInstalledPackages, dependencies = TRUE, Ncpus = cores)
  }
  
  # Load all the packages
  invisible(sapply(packageNames, library, character.only = TRUE, quietly = TRUE))
  
  # Print a message indicating the packages that were installed or already loaded
  installedPackages <- packageNames[!sapply(packageNames, requireNamespace, quietly = TRUE)]
  alreadyLoadedPackages <- packageNames[sapply(packageNames, requireNamespace, quietly = TRUE)]
  
  if (length(installedPackages) > 0) {
    message(paste0("Package(s) '", paste(installedPackages, collapse = "', '"), "' were successfully installed and loaded."))
  }
  if (length(alreadyLoadedPackages) > 0) {
    message(paste0("Package(s) '", paste(alreadyLoadedPackages, collapse = "', '"), "' were already installed and loaded."))
  }
}

find_unique_words <- function(df) {
  # First, we convert the dataframe to a tidy text format
  tidy_text <- df %>%
    unnest_tokens(word, text) %>% # Tokenize the texts
    group_by(character) %>% # Group by 'character' column
    summarise(text = paste(word, collapse = ' ')) %>% # Collapse the words back into texts
    unnest_tokens(word, text) %>% # Tokenize the texts again
    distinct() # Remove duplicate words within each group

  # Then, we find the unique words in each character class
  unique_words <- tidy_text %>%
    left_join(tidy_text %>% count(word), by = 'word') %>% # Count the total occurrence of each word
    filter(n == 1) %>% # Filter for words that occur only once
    select(-n) # Remove the 'n' column

  return(unique_words)
}

installAndLoadPackages(c("reshape2", "magrittr", "plyr", "dplyr", "tidyr",
                         "ggplot2", "tm", "SnowballC", "slam", "e1071", "feather",
                         "stopwords", "udpipe", "parallel", "httr", "stringr",
                         "stringr", "text2vec", "tidytext", "purrr"), cores = cores)

library(languageserver)
#library(readr)
#library(lubridate)
Sys.setenv(LANG = "en")

## Installazione lingua italiana per la lemmatizzazione
#install.koRpus.lang(
#  lang = "it",
#  repos = "https://undocumeantit.github.io/repos/l10n/"
#  )
#set.kRp.env(TT.cmd=paste(path.expand(getwd()),"tree-tagger-italian"))

################################### NOTE #######################################
# Se l'obiettivo è eseguire l'analisi del testo su un grande corpus di testo
#  italiano, potrebbe essere utile considerare l'uso di Python con la
#   libreria spaCy, che fornisce un robusto supporto per la
#    lemmatizzazione in molte lingue, inclusa l'italiana.
################################################################################

################################################################################
#####                           Load Dataframe                             #####
suburra <- read.csv("../DATA/01_Suburra_data.csv", stringsAsFactors = FALSE)
suburra_collapsed <- read.csv("../DATA/02_Suburra_data_collapsed.csv", stringsAsFactors = FALSE)
suburra_hybrid <- read.csv("../DATA/03_Suburra_data_hybrid.csv", stringsAsFactors = FALSE)



################################################################################
#####                          Analisi dei dati                            #####
#summary(suburra)
#summary(suburra_collapsed)

# Personaggi
#personaggi <- unique(suburra$character)
#personaggi <- sort(personaggi)


normal <- suburra %>%
  group_by(character, is_male) %>%
  dplyr::summarize(
    original = n()
  ) %>%
  arrange(desc(original)) %>%
  ungroup()


collapsed_df <- suburra_collapsed %>%
  group_by(character) %>%
  dplyr::summarize(
    collapsed = n()
  ) %>%
  arrange(desc(collapsed)) %>%
  ungroup()

hybrid_df <- suburra_hybrid %>%
  group_by(character, is_male) %>%
  dplyr::summarize(
    hybrid = n()
  ) %>%
  arrange(desc(hybrid)) %>%
  ungroup() %>%
  top_n(10, hybrid) %>%
  arrange(character) %>%
  as.data.frame()

hybrid_df$is_male <- c(ifelse(hybrid_df$is_male==0,F,T))
hybrid_df$collapsed <- c(ifelse(hybrid_df$character %in% c("Alberto Anacleti", "Amedeo Cinaglia", "Aureliano Adami", "Gabriele Marchilli", "Samurai Valerio", "Sara Monaschi", "Livia Adami"),T,F))

#data <- normal %>% left_join(hybrid_df, by = join_by(character))
data <- collapsed_df %>% left_join(normal, by = join_by(character))
data %<>% top_n(10, original)
data_bck <- data %>%
  select(-c(collapsed, original)) %>%
  arrange(character)

melted_data <- melt(data, id.vars = "character", measure.vars = c("collapsed", "original"))
melted_data %<>% 
  left_join(data_bck, by = join_by(character)) %>% 
  arrange(character)

#numero di battute per personaggio per database

# Compute labels and colors
labels <- ifelse(hybrid_df$character=="Contessa Sveva Della Rocca Croce", "Contessa Sveva", hybrid_df$character)
colors <- ifelse(data_bck$is_male == 0, "#e877f2", "#6088d6")

ggplot(melted_data, aes(x = character, y = value, fill = variable)) +
  geom_text(aes(label = value), vjust = -0.5, color = "black", size = 4, position = position_dodge(width = 0.8)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  scale_x_discrete(labels = labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14, color = colors)) +
  ggtitle("Number of script lines of top 10 characters in each dataset") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  labs(x = "Character", y = "# Script Lines", fill = "Dataset") +
  geom_line() +
  geom_hline(yintercept = c(max(hybrid_df$hybrid),min(hybrid_df$hybrid)))
#+
#  ggplot2::annotate(geom = "text", x = 2.5, y = max(hybrid_df$hybrid)+10, label = "max value") +
#  ggplot2::annotate(geom = "text", x = 9.8, y = min(hybrid_df$hybrid)-10, label = "min value")


################################### HYBRID DF ##################################
labels <- ifelse(hybrid_df$character=="Contessa Sveva Della Rocca Croce", "Contessa Sveva", hybrid_df$character)
colors <- ifelse(hybrid_df$is_male, "#6088d6", "#e877f2")

ggplot(hybrid_df, aes(x = character, y = hybrid, fill = collapsed)) +
  geom_text(aes(label = hybrid), vjust = -0.5, color = "black", size = 4, position = position_dodge(width = 0.8)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  scale_x_discrete(labels = labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14, color = colors)) +
  ggtitle("Number of script lines of top 10 characters in hybrid dataset", subtitle = "The first seven characters have consecutive rows collapsed") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5)) +
  labs(x = "Character", y = "# Script Lines", fill = "Rows collapsed")



# Function to convert ticks to time
convert_ticks_to_time <- function(ticks){
  tick_rate = 10000000
  seconds = ticks / tick_rate
  minutes = floor(seconds / 60)
  seconds = seconds %% 60
  milliseconds = floor((seconds - floor(seconds)) * 1000)
  seconds = floor(seconds)
  time = sprintf("%02d:%02d.%03d", minutes, seconds, milliseconds)
  return(time)
}


character_speak_time <- suburra %>%
  group_by(character) %>%
  dplyr::summarize(
    speak_time = sum(duration),
    time = convert_ticks_to_time(speak_time)
  ) %>%
  arrange(desc(speak_time)) %>%
  ungroup() %>%
  top_n(10, speak_time)

character_speak_time


################################################################################
#####                          Dataframe Settings                          #####
setwd(HOME)
suburra <- read.csv("../DATA/01_Suburra_data.csv", stringsAsFactors = FALSE, encoding="UTF-8")
suburra_collapsed <- read.csv("../DATA/02_Suburra_data_collapsed.csv", stringsAsFactors = FALSE, encoding="UTF-8")
suburra_hybrid <- read.csv("../DATA/03_Suburra_data_hybrid.csv", stringsAsFactors = FALSE, encoding="UTF-8")

suburra <- suburra[,-c(3, 8, 11)]
suburra_collapsed <- suburra_collapsed[,-c(3, 8, 11)]
suburra_hybrid <- suburra_hybrid[,-c(3, 8, 11)]


suburra$doc_id <- c(1:nrow(suburra))
suburra_collapsed$doc_id <- c(1:nrow(suburra_collapsed))
suburra_hybrid$doc_id <- c(1:nrow(suburra_hybrid))


suburra$character <- as.factor(suburra$character)
suburra_collapsed$character <- as.factor(suburra_collapsed$character)
suburra_hybrid$character <- as.factor(suburra_hybrid$character)

suburra$episode <- as.factor(suburra$episode)
suburra_collapsed$episode <- as.factor(suburra_collapsed$episode)
suburra_hybrid$episode <- as.factor(suburra_hybrid$episode)

#suburra$begin <- as.integer(suburra$begin)
#suburra_collapsed$begin <- as.integer(suburra_collapsed$begin)
#suburra_hybrid$begin <- as.integer(suburra_hybrid$begin)
#                                        
#suburra$end <- as.integer(suburra$end)
#suburra_collapsed$end <- as.integer(suburra_collapsed$end)
#suburra_hybrid$end <- as.integer(suburra_hybrid$end)

suburra$duration <- as.integer(suburra$duration)
suburra_collapsed$duration <- as.integer(suburra_collapsed$duration)
suburra_hybrid$duration <- as.integer(suburra_hybrid$duration)

suburra$bad_words <- as.factor(suburra$bad_words)
suburra_collapsed$bad_words <- as.factor(suburra_collapsed$bad_words)
suburra_hybrid$bad_words <- as.factor(suburra_hybrid$bad_words)

suburra$is_male <- as.factor(suburra$is_male)
suburra_collapsed$is_male <- as.factor(suburra_collapsed$is_male)
suburra_hybrid$is_male <- as.factor(suburra_hybrid$is_male)

# Vector of allowed character names (TOP 10)
allowed_characters <- c("Alberto Anacleti", "Amedeo Cinaglia", "Aureliano Adami", "Gabriele Marchilli", "Samurai Valerio", "Sara Monaschi", "Livia Adami", "Contessa Sveva Della Rocca Croce", "Angelica Sale", "Manfredi Anacleti")

# Drop rows where df$character is not in the allowed_characters vector
suburra <- suburra[suburra$character %in% allowed_characters, ]
suburra_collapsed <- suburra_collapsed[suburra_collapsed$character %in% allowed_characters, ]
suburra_hybrid <- suburra_hybrid[suburra_hybrid$character %in% allowed_characters, ]


suburra$character           <- droplevels(suburra$character)
suburra_collapsed$character <- droplevels(suburra_collapsed$character)
suburra_hybrid$character    <- droplevels(suburra_hybrid$character)


get_badwords <- function(){
  bad_w1 <- httr::content(httr::GET("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/it"), "text")
  bad_w2 <- httr::content(httr::GET("https://raw.githubusercontent.com/napolux/paroleitaliane/master/paroleitaliane/lista_badwords.txt"), "text")
  badwords <- unique(c(stringr::str_split(bad_w1, "\n")[[1]], stringr::str_split(bad_w2, "\n")[[1]]))
  return(badwords)
}

# 2 set bad words
set_badwords <- function(df){
  badwords <- get_badwords()
  df <- df %>% mutate(bad_words = purrr::map_int(stringr::str_split(script_line, " "), ~ifelse(any(.x %in% badwords), 1, 0)))
  return(df)
}

df <- set_badwords(suburra)
df_collapsed <- set_badwords(suburra_collapsed)
df_hybrid <- set_badwords(suburra_hybrid)

df$bad_words <- as.factor(df$bad_words)
df_collapsed$bad_words <- as.factor(df_collapsed$bad_words)
df_hybrid$bad_words <- as.factor(df_hybrid$bad_words)

################################################################################
#####                              PoS Tagging                             #####

# https://github.com/UniversalDependencies/UD_Italian-VIT/tree/master
italian.vit <- udpipe::udpipe_download_model(language = "italian-vit",
                                             model_dir = getwd(),
                                             udpipe_model_repo = "jwijffels/udpipe.models.ud.2.5",
                                             overwrite = F)

italian.vit <- udpipe::udpipe_load_model(italian.vit$file_model)

ifelse(parallel::detectCores() <= 12, cores<-(as.numeric(parallel::detectCores()-1)), cores<-12)


## ABILITARE SE SI VUOLE IL POST TAGGING groupby character (IN ONE-HOT-ENCODE)
#df <- data.frame(doc_id=df$character, text=df$script_line, stringsAsFactors = F)
#df_collapsed <- data.frame(doc_id=df$character, text=df$script_line, stringsAsFactors = F)
#df_hybrid <- data.frame(doc_id=df$character, text=df$script_line, stringsAsFactors = F)


df.pos <- udpipe::udpipe(df$script_line, object = italian.vit, parallel.cores = cores)
df.pos$doc_id <- as.integer(df.pos$doc_id)
df.pos.counts <- df.pos %>%
  group_by(doc_id, upos) %>%
  dplyr::summarize(count = n())
df.pos.matrix <- df.pos.counts %>%
  tidyr::pivot_wider(names_from = upos, values_from = count, values_fill = 0)

df_collapsed.pos <- udpipe::udpipe(df_collapsed$script_line, object = italian.vit, parallel.cores = cores)
df_collapsed.pos$doc_id <- as.integer(df_collapsed.pos$doc_id)
df_collapsed.pos.counts <- df_collapsed.pos %>%
  group_by(doc_id, upos) %>%
  dplyr::summarize(count = n())
df_collapsed.pos.matrix <- df_collapsed.pos.counts %>%
  tidyr::pivot_wider(names_from = upos, values_from = count, values_fill = 0)

df_hybrid.pos <- udpipe::udpipe(df_hybrid$script_line, object = italian.vit, parallel.cores = cores)
df_hybrid.pos$doc_id <- as.integer(df_hybrid.pos$doc_id)
df_hybrid.pos.counts <- df_hybrid.pos %>%
  group_by(doc_id, upos) %>%
  dplyr::summarize(count = n())
df_hybrid.pos.matrix <- df_hybrid.pos.counts %>%
  tidyr::pivot_wider(names_from = upos, values_from = count, values_fill = 0)

rm(df_collapsed.pos, df_collapsed.pos.counts, df_hybrid.pos, df_hybrid.pos.counts, df.pos, df.pos.counts)


df.final           <- left_join(df, df.pos.matrix, by = "doc_id")
df_collapsed.final <- left_join(df_collapsed, df_collapsed.pos.matrix, by = "doc_id")
df_hybrid.final    <- left_join(df_hybrid, df_hybrid.pos.matrix, by = "doc_id")
rm(df, df_collapsed, df_hybrid)

df.final           <- df.final[,-9]
df_collapsed.final <- df_collapsed.final[,-9]
df_hybrid.final    <- df_hybrid.final[,-9]
write_feather(df.final, "df.final.feather")
write_feather(df_collapsed.final, "df_collapsed.final.feather")
write_feather(df_hybrid.final, "df_hybrid.final.feather")
################################################################################
# Creazione di un Corpus a partire dal dataset
corpus <- Corpus(VectorSource(df.final$script_line))
corpus_collapsed <- Corpus(VectorSource(df_collapsed.final$script_line))
corpus_hybrid <- Corpus(VectorSource(df_hybrid.final$script_line))

corpus           <- tm_map(corpus, content_transformer(tolower))
corpus_collapsed <- tm_map(corpus_collapsed, content_transformer(tolower))
corpus_hybrid    <- tm_map(corpus_hybrid, content_transformer(tolower))

# Rimozione della punteggiatura FATTA IN PYTHON PERCHé LE PAROLE ERANO CONCATENATE SENZA SPAZI
#corpus           <- tm_map(corpus, removePunctuation)
#corpus_collapsed <- tm_map(corpus_collapsed, removePunctuation)
#corpus_hybrid    <- tm_map(corpus_hybrid, removePunctuation)

corpus           <- tm_map(corpus, content_transformer(function(x) gsub("[^[:alpha:]]", " ", x)))
corpus_collapsed <- tm_map(corpus_collapsed, content_transformer(function(x) gsub("[^[:alpha:]]", " ", x)))
corpus_hybrid    <- tm_map(corpus_hybrid, content_transformer(function(x) gsub("[^[:alpha:]]", " ", x)))

corpus           <- tm_map(corpus, removeNumbers)
corpus_collapsed <- tm_map(corpus_collapsed, removeNumbers)
corpus_hybrid    <- tm_map(corpus_hybrid, removeNumbers)

# Rimozione delle spaziature extra
corpus           <- tm_map(corpus, stripWhitespace)
corpus_collapsed <- tm_map(corpus_collapsed, stripWhitespace)
corpus_hybrid    <- tm_map(corpus_hybrid, stripWhitespace)


######################### Rimozione delle stop words ###########################
corpus_clean           <- tm_map(corpus, content_transformer(removeWords), stopwords::stopwords(language = "it"))
corpus_collapsed_clean <- tm_map(corpus_collapsed, content_transformer(removeWords), stopwords::stopwords(language = "it"))
corpus_hybrid_clean    <- tm_map(corpus_hybrid, content_transformer(removeWords), stopwords::stopwords(language = "it"))

trim_spaces <- function(x) {
  stringr::str_trim(x, side = c("both", "left", "right"))
}

corpus_clean           <- tm_map(corpus_clean, content_transformer(function(x) trim_spaces(x)))
corpus_collapsed_clean <- tm_map(corpus_collapsed_clean, content_transformer(function(x) trim_spaces(x)))
corpus_hybrid_clean    <- tm_map(corpus_hybrid_clean, content_transformer(function(x) trim_spaces(x)))

corpus_clean           <- tm_map(corpus_clean, stripWhitespace)
corpus_collapsed_clean <- tm_map(corpus_collapsed_clean, stripWhitespace)
corpus_hybrid_clean    <- tm_map(corpus_hybrid_clean, stripWhitespace)

################################################################################
#####                                Stemming                              #####
whitelist <- c("alberto","anacleti","spadino","amedeo","cinaglia","aureliano","adami","gabriele","marchilli","samurai","valerio","sara","monaschi", "livia", "sveva", "angelica","sale", "manfredi")

convert_counts <- function(x){
  x <- ifelse(x>0, 1, 0)
}
################################################################################
#####                             Bag of Words                             #####
#custom_stemming <- function(corpus, whitelist) {
#  stemmed_corpus <- lapply(corpus, function(doc) {
#    words <- unlist(strsplit(as.character(doc), " "))
#    stemmed_words <- ifelse(words %in% whitelist, words, wordStem(words, language = "italian"))
#    PlainTextDocument(paste(stemmed_words, collapse = " "))
#  })
#  # Extract the content from each document in the stemmed_corpus list
#  texts <- sapply(stemmed_corpus, function(doc) doc$content)
#  corpus <- Corpus(VectorSource(texts))
#  dtm <- DocumentTermMatrix(corpus) # bag of words
#  dtm_2 <- apply(dtm, MARGIN = 2, convert_counts)
#  sparse <- slam::as.simple_triplet_matrix(dtm)
#  df <- as.data.frame(as.matrix(sparse))
#  return(list("dtm" = dtm,
#              "dtm_2" = dtm_2,
#              "df" = df))
#}
custom_stemming <- function(corpus, whitelist, df) {
  # Create a frequency table of all the words in the corpus
  word_freq <- table(unlist(strsplit(as.character(corpus), " ")))
  
  # Get the words that appear at least 5 times and are not in the whitelist
  words_to_include <- names(word_freq[word_freq >= 5 & !(names(word_freq) %in% whitelist)])
  
  # Define a function to stem words
  stem_word <- function(word) {
    ifelse(word %in% whitelist, word, wordStem(word, language = "italian"))
  }
  words_to_include <- sapply(words_to_include, stem_word)
  
  # Apply stemming to each document in the corpus
  stemmed_corpus <- lapply(corpus, function(doc) {
    words <- unlist(strsplit(as.character(doc), " "))
    stemmed_words <- sapply(words, stem_word)
    PlainTextDocument(paste(stemmed_words, collapse = " "))
  })
  
  # Extract the content from each document in the stemmed_corpus list
  texts  <- sapply(stemmed_corpus, function(doc) doc$content)
  corpus <- Corpus(VectorSource(texts))
  corpus <- tm_map(corpus, content_transformer(function(x) trim_spaces(x)))
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Create the dtm with only the included words
  dtm <- DocumentTermMatrix(corpus, control = list(dictionary = words_to_include))
  sparse <- slam::as.simple_triplet_matrix(dtm)
  new_df <- as.data.frame(as.matrix(sparse))
  columns_to_add <- c("episode", "duration", "bad_words", "is_male", "ADJ", "ADP",
                "AUX", "CCONJ", "DET", "NOUN", "PRON", "INTJ", "VERB",
                "NUM", "ADV", "SCONJ", "X", "NA", "PROPN", "PUNCT")
  for (col in columns_to_add) {
    new_df[col] <- df[col]
  }
  dtm <- apply(new_df, MARGIN = 2, convert_counts)
  
  return(list("dtm" = dtm,
              "df" = new_df,
              "character" = df$character))
}

dtm.With_stopW           <- custom_stemming(corpus, whitelist, df.final)
dtm_collapsed.With_stopW <- custom_stemming(corpus_collapsed, whitelist, df_collapsed.final)
dtm_hybrid.With_stopW    <- custom_stemming(corpus_hybrid, whitelist, df_hybrid.final)

dtm             <- custom_stemming(corpus_clean, whitelist, df.final)
dtm_collapsed   <- custom_stemming(corpus_collapsed_clean, whitelist, df_collapsed.final)
dtm_hybrid      <- custom_stemming(corpus_hybrid_clean, whitelist, df_hybrid.final)

rm(italian.vit, allowed_characters)



################################################################################
#####                                 Word2Vec                             #####
#####                              Word Embedding                          #####

my_stemming <- function(corpus, whitelist) {
  stemmed_corpus <- lapply(corpus, function(doc) {
    words <- unlist(strsplit(as.character(doc), " "))
    stemmed_words <- ifelse(words %in% whitelist, words, wordStem(words, language = "italian"))
    PlainTextDocument(paste(stemmed_words, collapse = " "))
  })
  # Extract the content from each document in the stemmed_corpus list
  texts <- sapply(stemmed_corpus, function(doc) doc$content)
  corpus <- Corpus(VectorSource(texts))
 
  return(corpus) 
}


convert_word2vec <- function(corpus, cores){
  if (!is.character(corpus))
    corpus <- as.character(corpus)
  # Splits it into individual words (tokens)
  tokens <- text2vec::space_tokenizer(corpus)

  # Create vocabulary
  # Creates a vocabulary from tokens, which is a set of all unique words in this corpus
  it = text2vec::itoken(tokens, progressbar = FALSE)
  vocab <- text2vec::create_vocabulary(it)

  # Prune the vocabulary
  # This filters the vocabulary to only include words that appear at least 5 times in the corpus.
  # This is done because words that appear very infrequently can't be accurately represented as vectors.
  vocab <- text2vec::prune_vocabulary(vocab, term_count_min = 5L)

  # Create the term-co-occurrence matrix (TCM)
  # This creates a matrix where each element represents how often a pair of words appear together in the corpus
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  tcm <- text2vec::create_tcm(it, vectorizer, skip_grams_window = 5L)

  # Factorize the TCM matrix via the GloVe algorithm
  # This is the main step where the GloVe algorithm is used to generate the word vectors.
  glove = GlobalVectors$new(rank = 50, x_max = 10)
  wv_main = glove$fit_transform(tcm, n_iter = 100, convergence_tol = 0.01, n_threads = cores)

  # Get the word vectors
  # Here, we add the main word vectors to the context word vectors to get the final word vectors
  wv_context = glove$components
  word_vectors = wv_main + t(wv_context)
  word_vectors = as.matrix(word_vectors)

  return(list(
    "vocabulary" = vocab,
    "glove" = word_vectors
  ))
}


convert_document_to_vector <- function(document, word_vectors) {
  tokens <- text2vec::space_tokenizer(document)
  tokens <- unlist(tokens)  # Convert tokens to a character vector
  tokens_n <- length(tokens)
  tokens <- tokens[tokens %in% rownames(word_vectors)]  # Filter out tokens not in the vocabulary
  #print(tokens)
  
  if (length(tokens) == 0) {
    return(rep(0, ncol(word_vectors)))
  }
  
  token_vectors <- word_vectors[tokens, ]
  
  if (length(token_vectors) == 0) {
    return(rep(0, ncol(word_vectors)))
  }
  
  if (length(token_vectors) == 1) {
    return(token_vectors)
  }
  token_vectors = t(token_vectors)
  return(rowMeans(token_vectors, na.rm = TRUE))
}


wv           <- convert_word2vec(corpus_clean, cores)
wv_collapsed <- convert_word2vec(corpus_collapsed_clean, cores)
wv_hybrid    <- convert_word2vec(corpus_hybrid_clean, cores)

corpus_clean           <- my_stemming(corpus_clean, whitelist)
corpus_collapsed_clean <- my_stemming(corpus_collapsed_clean, whitelist)
corpus_hybrid_clean    <- my_stemming(corpus_hybrid_clean, whitelist)

wv.stem           <- convert_word2vec(corpus_clean, cores)
wv_collapsed.stem <- convert_word2vec(corpus_collapsed_clean, cores)
wv_hybrid.stem    <- convert_word2vec(corpus_hybrid_clean, cores)

# Apply the function to each document in your corpus
document_vectors <- sapply(corpus_clean, convert_document_to_vector, word_vectors = wv$glove)
document_vectors <- sapply(df.final$script_line, convert_document_to_vector, word_vectors = wv$glove)

#Transpose the matrix so that documents are rows and dimensions are columns
document_vectors <- t(document_vectors)


rm(whitelist, corpus, corpus_collapsed, corpus_hybrid, corpus_clean, corpus_collapsed_clean, corpus_hybrid_clean)

# Rimozione della punteggiatura
#corpus_clean           <- tm_map(corpus_clean, removePunctuation)
#corpus_collapsed_clean <- tm_map(corpus_collapsed_clean, removePunctuation)
#corpus_hybrid_clean    <- tm_map(corpus_hybrid_clean, removePunctuation)
#
#corpus_clean           <- tm_map(corpus_clean, removeNumbers)
#corpus_collapsed_clean <- tm_map(corpus_collapsed_clean, removeNumbers)
#corpus_hybrid_clean    <- tm_map(corpus_hybrid_clean, removeNumbers)
#
## Rimozione delle spaziature extra
#corpus_clean           <- tm_map(corpus_clean, stripWhitespace)
#corpus_collapsed_clean <- tm_map(corpus_collapsed_clean, stripWhitespace)
#corpus_hybrid_clean    <- tm_map(corpus_hybrid_clean, stripWhitespace)

#inspect(corpus_collapsed)
#inspect(corpus_collapsed_clean)

#dtm           <- DocumentTermMatrix(corpus_clean)
#dtm_collapsed <- DocumentTermMatrix(corpus_collapsed_clean)
#dtm_hybrid    <- DocumentTermMatrix(corpus_hybrid_clean)
#
#
## DTM a matrice sparsa
## simplify_simple_sparse_array tries to reduce v
#sparse           <- slam::as.simple_triplet_matrix(dtm)
#sparse_collapsed <- slam::as.simple_triplet_matrix(dtm_collapsed)
#sparse_hybrid    <- slam::as.simple_triplet_matrix(dtm_hybrid)
#rm(corpus_clean, corpus_collapsed, corpus_hybrid, dtm, dtm_collapsed, dtm_hybrid)
#
#labels           <- suburra$character
#labels_collapsed <- suburra_collapsed$character
#labels_hybrid    <- suburra_hybrid$character
#
#V3_normal    <- data.frame(sparse, stringsAsFactors = F)
#V3_collapsed <- data.frame(sparse_collapsed, labels_collapsed, stringsAsFactors = F)
#v3_hybrid    <- data.frame(sparse_hybrid, labels_hybrid, stringsAsFactors = F)

save.image(file = "P02.RData", compress=T)
