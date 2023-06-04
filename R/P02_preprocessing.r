################################################################################
#####                                                                      #####
#####                             PRE-PROCESSING                           #####
#####                                                                      #####
################################################################################



################################################################################
#####                        Functions for packages                        #####
installAndLoadPackages <- function(packageNames) {
  # Convert packageNames to a character vector if it's a single package name
  if (is.character(packageNames)) {
    packageNames <- as.character(packageNames)
  }
  
  # Check if each package is already installed
  notInstalledPackages <- packageNames[!sapply(packageNames, requireNamespace, quietly = TRUE)]
  
  # Install any packages that are not already installed
  if (length(notInstalledPackages) > 0) {
    install.packages(notInstalledPackages, dependencies = TRUE)
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

installAndLoadPackages(c("reshape2", "magrittr", "dplyr", "tidyr", "ggplot2", "tm", "SnowballC",
                         "slam", "e1071", "stopwords", "koRpus", "koRpus.lang.it", "udpipe",
                         "parallel", "httr", "stringr", "purrr"))

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
  summarise(
    original = n()
  ) %>%
  arrange(desc(original)) %>%
  ungroup()


collapsed_df <- suburra_collapsed %>%
  group_by(character) %>%
  summarise(
    collapsed = n()
  ) %>%
  arrange(desc(collapsed)) %>%
  ungroup()

hybrid_df <- suburra_hybrid %>%
  group_by(character, is_male) %>%
  summarise(
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
  summarise(
    speak_time = sum(duration),
    time = convert_ticks_to_time(speak_time)
    ) %>%
  arrange(desc(speak_time)) %>%
  ungroup() %>%
  top_n(10, speak_time)

character_speak_time


################################################################################
#####                          Dataframe Settings                          #####
setwd("/home/smoxy/AIMLEA4Suburra/R/")
suburra <- read.csv("../DATA/01_Suburra_data.csv", stringsAsFactors = FALSE)
suburra_collapsed <- read.csv("../DATA/02_Suburra_data_collapsed.csv", stringsAsFactors = FALSE)
suburra_hybrid <- read.csv("../DATA/03_Suburra_data_hybrid.csv", stringsAsFactors = FALSE)

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
  summarise(count = n())
df.pos.matrix <- df.pos.counts %>%
  tidyr::pivot_wider(names_from = upos, values_from = count, values_fill = 0)

df_collapsed.pos <- udpipe::udpipe(df_collapsed$script_line, object = italian.vit, parallel.cores = cores)
df_collapsed.pos$doc_id <- as.integer(df_collapsed.pos$doc_id)
df_collapsed.pos.counts <- df_collapsed.pos %>%
  group_by(doc_id, upos) %>%
  summarise(count = n())
df_collapsed.pos.matrix <- df_collapsed.pos.counts %>%
  tidyr::pivot_wider(names_from = upos, values_from = count, values_fill = 0)

df_hybrid.pos <- udpipe::udpipe(df_hybrid$script_line, object = italian.vit, parallel.cores = cores)
df_hybrid.pos$doc_id <- as.integer(df_hybrid.pos$doc_id)
df_hybrid.pos.counts <- df_hybrid.pos %>%
  group_by(doc_id, upos) %>%
  summarise(count = n())
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

################################################################################
# Creazione di un Corpus a partire dal dataset
corpus <- Corpus(VectorSource(df.final$script_line))
corpus_collapsed <- Corpus(VectorSource(df_collapsed.final$script_line))
corpus_hybrid <- Corpus(VectorSource(df_hybrid.final$script_line))

corpus           <- tm_map(corpus, content_transformer(tolower))
corpus_collapsed <- tm_map(corpus_collapsed, content_transformer(tolower))
corpus_hybrid    <- tm_map(corpus_hybrid, content_transformer(tolower))

# Rimozione della punteggiatura
corpus           <- tm_map(corpus, removePunctuation)
corpus_collapsed <- tm_map(corpus_collapsed, removePunctuation)
corpus_hybrid <- tm_map(corpus_hybrid, removePunctuation)

corpus           <- tm_map(corpus, removeNumbers)
corpus_collapsed <- tm_map(corpus_collapsed, removeNumbers)
corpus_hybrid <- tm_map(corpus_hybrid, removeNumbers)

# Rimozione delle spaziature extra
corpus           <- tm_map(corpus, stripWhitespace)
corpus_collapsed <- tm_map(corpus_collapsed, stripWhitespace)
corpus_hybrid <- tm_map(corpus_hybrid, stripWhitespace)


######################### Rimozione delle stop words ###########################
corpus_clean           <- tm_map(corpus, content_transformer(removeWords), stopwords::stopwords(language = "it"))
corpus_collapsed_clean <- tm_map(corpus_collapsed, content_transformer(removeWords), stopwords::stopwords(language = "it"))
corpus_hybrid_clean    <- tm_map(corpus_hybrid, content_transformer(removeWords), stopwords::stopwords(language = "it"))

################################################################################
#####                                Stemming                              #####
whitelist <- c("alberto","anacleti","spadino","amedeo","cinaglia","aureliano","adami","gabriele","marchilli","samurai","valerio","sara","monaschi", "livia", "sveva", "angelica","sale", "manfredi")

custom_stemming <- function(corpus, whitelist) {
  stemmed_corpus <- lapply(corpus, function(doc) {
    words <- unlist(strsplit(as.character(doc), " "))
    stemmed_words <- ifelse(words %in% whitelist, words, wordStem(words, language = "italian"))
    PlainTextDocument(paste(stemmed_words, collapse = " "))
  })
  # Extract the content from each document in the stemmed_corpus list
  texts <- sapply(stemmed_corpus, function(doc) doc$content)
  corpus <- Corpus(VectorSource(texts))
  dtm <- DocumentTermMatrix(corpus) # bag of words
  sparse <- slam::as.simple_triplet_matrix(dtm)
  df <- as.data.frame(as.matrix(sparse))
  return(df)
}

dtm.With_stopW           <- custom_stemming(corpus, whitelist)
dtm_collapsed.With_stopW <- custom_stemming(corpus_collapsed, whitelist)
dtm_hybrid.With_stopW    <- custom_stemming(corpus_hybrid, whitelist)

dtm.No_stopW             <- custom_stemming(corpus_clean, whitelist)
dtm_collapsed.No_stopW   <- custom_stemming(corpus_collapsed_clean, whitelist)
dtm_hybrid.No_stopW      <- custom_stemming(corpus_hybrid_clean, whitelist)

rm(corpus, corpus_collapsed, corpus_hybrid, corpus_clean, corpus_collapsed_clean, corpus_hybrid_clean)


dtm.With_stopW$character           <- df.final$character
dtm_collapsed.With_stopW$character <- df_collapsed.final$character
dtm_hybrid.With_stopW$character    <- df_hybrid.final$character

dtm.No_stopW$character             <- df.final$character
dtm_collapsed.No_stopW$character   <- df_collapsed.final$character
dtm_hybrid.No_stopW$character      <- df_hybrid.final$character

rm(italian.vit, whitelist, allowed_characters)

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
