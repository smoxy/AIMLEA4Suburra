installAndLoadPackages(c("caret", "smotefamily", "parallel", "doParallel", "bnclassify", "C50", "plyr"))
library(languageserver)
load("P02.RData")
load("/home/smoxy/AIMLEA4Suburra/R/server_computation.RData")
setwd("/home/smoxy/AIMLEA4Suburra/R/")

# START CLUSTERS
ifelse(detectCores() <= 12, cores<-(as.numeric(detectCores()-1)), cores<-12)
cl <- parallel::makePSOCKcluster(cores)
doParallel::registerDoParallel(cl)


################################################################################
#####                  SUBSETTING IN TRAIN AND TEST  ->  DTM               #####
set.seed(1234)  # For reproducibility
Index <- caret::createDataPartition(dtm.With_stopW$character, p = .8, list = FALSE, times = 1)
dtm.With_stopW_TRAIN            <- dtm.With_stopW[Index,]
dtm.With_stopW_TEST             <- dtm.With_stopW[-Index,]
Index <- caret::createDataPartition(dtm_collapsed.With_stopW$character, p = .8, list = FALSE, times = 1)
dtm_collapsed.With_stopW_TRAIN  <- dtm_collapsed.With_stopW[Index,]
dtm_collapsed.With_stopW_TEST   <- dtm_collapsed.With_stopW[-Index,]
Index <- caret::createDataPartition(dtm_hybrid.With_stopW$character, p = .8, list = FALSE, times = 1)
dtm_hybrid.With_stopW_TRAIN     <- dtm_hybrid.With_stopW[Index,]
dtm_hybrid.With_stopW_TEST      <- dtm_hybrid.With_stopW[-Index,]
Index <- caret::createDataPartition(dtm.No_stopW$character, p = .8, list = FALSE, times = 1)
dtm.No_stopW_TRAIN              <- dtm.No_stopW[Index,]
dtm.No_stopW_TEST               <- dtm.No_stopW[-Index,]
Index <- caret::createDataPartition(dtm_collapsed.No_stopW$character, p = .8, list = FALSE, times = 1)
dtm_collapsed.No_stopW_TRAIN    <- dtm_collapsed.No_stopW[Index,]
dtm_collapsed.No_stopW_TEST     <- dtm_collapsed.No_stopW[-Index,]
Index <- caret::createDataPartition(dtm_hybrid.No_stopW$character, p = .8, list = FALSE, times = 1)
dtm_hybrid.No_stopW_TRAIN       <- dtm_hybrid.No_stopW[Index,]
dtm_hybrid.No_stopW_TEST        <- dtm_hybrid.No_stopW[-Index,]

Index <- caret::createDataPartition(df.final$character, p = .75, list = FALSE, times = 1)
df.final_TRAIN           <- df.final[Index,]
df.final_TEST            <- df.final[-Index,]
Index <- caret::createDataPartition(df_collapsed.final$character, p = .75, list = FALSE, times = 1)
df_collapsed.final_TRAIN <- df_collapsed.final[Index,]
df_collapsed.final_TEST  <- df_collapsed.final[-Index,]
Index <- caret::createDataPartition(df_hybrid.final$character, p = .75, list = FALSE, times = 1)
df_hybrid.final_TRAIN    <- df_hybrid.final[Index,]
df_hybrid.final_TEST     <- df_hybrid.final[-Index,]


################################################################################
#####                                  CARET                               #####
#####                                 Tuning                               #####
#####                                                                      #####
################################################################################
# crea un oggetto di controllo per l'addestramento del modello.
ctrl_smote_repeatedCrossValidation <- caret::trainControl(method = "repeatedcv", # metodo di validazione incrociata da utilizzare
                            number = 10,         # il numero di fold nella validazione incrociata (10)
                            repeats = 10,        # il numero di ripetizioni per la validazione incrociata (10)
                            verboseIter = TRUE, # stampare o meno i messaggi iterativi durante l'addestramento
                            sampling = "smote")  # metodo di campionamento da utilizzare ("smote")


ctrl <- caret::trainControl(method = "repeatedcv",
                            number = 100,
                            repeats = 5,
                            search = "grid",
                            verboseIter = F)


################################################################################
#####                  Random Forest with SMOTE Oversampling               #####
dtm_collapsed.With_stopW_model <- caret::train(character ~ .,
                                               data = dtm_collapsed.With_stopW_TRAIN,
                                               method = "rf",
                                               preProcess = c("scale", "center"),
                                               trControl = ctrl,
                                               allowParallel=TRUE)

dtm_collapsed.No_stopW_model <- caret::train(character ~ .,
                                             data = dtm_collapsed.No_stopW_TRAIN,
                                             method = "rf",
                                             preProcess = c("scale", "center"),
                                             trControl = ctrl,
                                             allowParallel=TRUE)

dtm_hybrid.With_stopW_model <- caret::train(character ~ .,
                                            data = dtm_hybrid.With_stopW_TRAIN,
                                            method = "rf",
                                            preProcess = c("scale", "center"),
                                            trControl = ctrl,
                                            allowParallel=TRUE)

dtm_hybrid.No_stopW_model <- caret::train(character ~ .,
                                          data = dtm_hybrid.No_stopW_TRAIN,
                                          method = "rf",
                                          preProcess = c("scale", "center"),
                                          trControl = ctrl,
                                          allowParallel=TRUE)

dtm.No_stopW_model <- caret::train(character ~ .,
                                   data = dtm.No_stopW_TRAIN,
                                   method = "rf",
                                   preProcess = c("scale", "center"),
                                   trControl = ctrl,
                                   allowParallel=TRUE)

save(dtm_collapsed.With_stopW_model, dtm_collapsed.No_stopW_model,
     dtm_hybrid.With_stopW_model,
     file = "/home/smoxy/AIMLEA4Suburra/R/server_computation.RData")

dtm_collapsed.With_stopW_PRED <- predict(dtm_collapsed.With_stopW_model, newdata = dtm_collapsed.With_stopW_TEST[, -which(names(dtm_collapsed.With_stopW_TEST) == "character")])
dtm_collapsed.With_stopW_CONF <- caret::confusionMatrix(data = dtm_collapsed.With_stopW_PRED, reference = dtm_collapsed.With_stopW_TEST$character)

save(dtm_collapsed.With_stopW_model, dtm_collapsed.No_stopW_model,
     dtm_hybrid.With_stopW_model, dtm_hybrid.No_stopW_model, dtm.No_stopW_model,
     file = "/home/smoxy/AIMLEA4Suburra/R/server_computation.RData")


dtm.With_stopW_TRAIN %>%
    group_by(character) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)*100)

dtm.With_stopW_TEST %>%
    group_by(character) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)*100)


dtm_hybrid.No_stopW_TEST %>%
    group_by(character) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)*100)

dtm_hybrid.No_stopW_TEST %>%
    group_by(character) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n)*100)

##STOP CLUSTER
stopCluster(cl)
rm(cl)
registerDoSEQ()



################################################################################
#####                                                                      #####
#####                                  C5.0                                #####
#####                                                                      #####
################################################################################
#####                             Decision tree                            #####
installAndLoadPackages(c("C50", "xtable"))
m <- C50::C5.0(dtm_collapsed.No_stopW_TRAIN[,-which(names(dtm_collapsed.No_stopW_TRAIN) == "character")], dtm_collapsed.No_stopW_TRAIN$character, costs=NULL)
p <- C50::predict.C5.0(m, dtm_collapsed.No_stopW_TEST, type = "class")
summary(m)

########## TEST with caret ############
tuneGrid <- expand.grid(trials = c(10, 20, 30),
                        model = c("tree", "rules"),
                        winnow = c(TRUE, FALSE))

df.final_TRAIN$script_line <- as.character(df.final_TRAIN$script_line)

m <-caret::train(x = df.final_TRAIN[, -c(which(names(df.final_TRAIN) == "character"),which(names(df.final_TRAIN) == "script_line"))], 
                 y = df.final_TRAIN$character,
                 method = 'C5.0',
                 trControl = ctrl,
                 tuneGrid = tuneGrid)
mean(m$resample$Accuracy) #model accuracy
summary(m)
p.final <- predict(m, newdata = df.final_TEST[, -which(names(df.final_TEST) == "character")])
confusion_matrix <- caret::confusionMatrix(data = p.final, reference = df.final_TEST$character)
round(confusion_matrix$byClass * 100, 2)
latex_table <- xtable(as.data.frame(round(confusion_matrix$byClass * 100, 2)), caption = "Metrics Table") # Sensitivity, Specificity, Precision+
print(latex_table, include.rownames = F)

#######################################

m <- C50::C5.0(df.final_TRAIN[,-c(which(names(df.final_TRAIN) == "character"), which(names(df.final_TRAIN) == "script_line"))], df.final_TRAIN$character, trials = 10)
summary(m)
p.final <- C50::predict.C5.0(m, df.final_TEST, type = "class")
# Create a confusion matrix
confusion_matrix <- confusionMatrix(data = p.final, reference = df.final_TEST$character)
# Extract the desired metrics
accuracy <- confusion_matrix$overall['Accuracy']
latex_table <- xtable(as.data.frame(round(confusion_matrix$byClass * 100, 2)), caption = "Metrics Table") # Sensitivity, Specificity, Precision+
# Print the LaTeX code
print(latex_table, include.rownames = F)


m <- C50::C5.0(df_collapsed.final_TRAIN[,-c(which(names(df_collapsed.final_TRAIN) == "character"), which(names(df_collapsed.final_TRAIN) == "script_line"))], df_collapsed.final_TRAIN$character, df.final_TRAIN$character, trials = 10)
summary(m)
p_collapsed.final <- C50::predict.C5.0(m, df_collapsed.final_TEST, type = "class")
m <- C50::C5.0(df_hybrid.final_TRAIN[,-c(which(names(df_hybrid.final_TRAIN) == "character"), which(names(df_hybrid.final_TRAIN) == "script_line"))], df_hybrid.final_TRAIN$character, df.final_TRAIN$character, trials = 10)
summary(m)
p_hybrid.final <- C50::predict.C5.0(m, df_hybrid.final_TEST, type = "class")


################################################################################
#####                                                                      #####
#####                            Bayesian Model                            #####
#####                                                                      #####
################################################################################
#####                   Model Averaged Naive Bayes Classifier              #####

# <- 

save.image(file = "P03.RData", compress=T)