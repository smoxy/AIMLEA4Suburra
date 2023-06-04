installAndLoadPackages(c("caret", "smotefamily", "parallel", "doParallel"))
library(languageserver)
load("/home/smoxy/AIMLEA4Suburra/R/workspace.RData")
#load("/home/smoxy/AIMLEA4Suburra/R/server_computation.RData")
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


################################################################################
#####                                                                      #####
#####                                 Tuning                               #####
#####                                                                      #####
################################################################################
                # crea un oggetto di controllo per l'addestramento del modello.
ctrl <- caret::trainControl(method = "repeatedcv", # metodo di validazione incrociata da utilizzare
                     number = 10,         # il numero di fold nella validazione incrociata (10)
                     repeats = 5,        # il numero di ripetizioni per la validazione incrociata (10)
                     verboseIter = TRUE, # stampare o meno i messaggi iterativi durante l'addestramento
                     sampling = "smote")  # metodo di campionamento da utilizzare ("smote")


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
save(dtm_collapsed.With_stopW_model, dtm_collapsed.No_stopW_model,
    dtm_hybrid.With_stopW_model, dtm_hybrid.No_stopW_model,
    file = "/home/smoxy/AIMLEA4Suburra/R/server_computation.RData")

##STOP CLUSTER
stopCluster(cl)
rm(cl)
registerDoSEQ()


df_smote <- SMOTE(X = df[ , colnames(dtm.With_stopW) != "character"],
                  K = 5,     # Number of nearest neighbors to consider, change accordingly
                  dup_size = 0.5)

model_rf <- caret::train(character ~ .,
                         data = dtm.With_stopW,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 10, 
                                                  verboseIter = FALSE))









################################################################################
#####                                                                      #####
#####                                 TESTING                              #####
#####                                                                      #####
################################################################################
#####                             Decision tree                            #####






















