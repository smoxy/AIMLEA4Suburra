################################################################################
#####                                                                      #####
#####                                 Tuning                               #####
#####                                                                      #####
################################################################################
installAndLoadPackages(c("caret", "smotefamily", "parallel", "doParallel"))
library(languageserver)


# START CLUSTERS
ifelse(detectCores() <= 12, cores<-(as.numeric(detectCores()-1)), cores<-12)
cl <- parallel::makePSOCKcluster(cores)
doParallel::registerDoParallel(cl)


##STOP CLUSTER
#stopCluster(cl)
#registerDoSEQ()
################################################################################
#####                  Random Forest with SMOTE Oversampling               #####
set.seed(1234)
# crea un oggetto di controllo per l'addestramento del modello.
ctrl <- caret::trainControl(method = "repeatedcv", # metodo di validazione incrociata da utilizzare
                     number = 3,         # il numero di fold nella validazione incrociata (10)
                     repeats = 10,        # il numero di ripetizioni per la validazione incrociata (10)
                     verboseIter = FALSE, # stampare o meno i messaggi iterativi durante l'addestramento
                     sampling = "smote")  # metodo di campionamento da utilizzare ("smote")

model_rf_smote <- caret::train(character ~ .,
                               data = dtm.With_stopW,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl,
                               allowParallel=TRUE)

model_rf_smote <- randomForest.dtm.With_stopW


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






















