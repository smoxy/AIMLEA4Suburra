installAndLoadPackages(c("parallel", "caret", "smotefamily", "parallel", "doParallel", "bnclassify", "C50"))
setwd("/home/smoxy/AIMLEA4Suburra/R/")
library(languageserver)
load("P02.RData")
load("server_computation.DTM_RF.RData")

# START CLUSTERS
ifelse(parallel::detectCores() <= 12, cores<-(as.numeric(parallel::detectCores()-1)), cores<-12)
cl <- parallel::makePSOCKcluster(cores)
doParallel::registerDoParallel(cl)


################################################################################
#####                  SUBSETTING IN TRAIN AND TEST  ->  DTM               #####
set.seed(1234)  # For reproducibility
dtm.Index <- caret::createDataPartition(dtm$character, p = .75, list = FALSE, times = 1)
dtm_TRAIN              <- dtm$dtm_2[dtm.Index,]
dtm_TEST               <- dtm$dtm_2[-dtm.Index,]
dtm_collapsed.Index <- caret::createDataPartition(dtm_collapsed$character, p = .75, list = FALSE, times = 1)
dtm_collapsed_TRAIN    <- dtm_collapsed$dtm_2[dtm_collapsed.Index,]
dtm_collapsed_TEST     <- dtm_collapsed$dtm_2[-dtm_collapsed.Index,]
dtm_hybrid.Index <- caret::createDataPartition(dtm_hybrid$character, p = .75, list = FALSE, times = 1)
dtm_hybrid_TRAIN       <- dtm_hybrid$dtm_2[dtm_hybrid.Index,]
dtm_hybrid_TEST        <- dtm_hybrid$dtm_2[-dtm_hybrid.Index,]
dtm.With_stopW.Index <- caret::createDataPartition(dtm.With_stopW$character, p = .75, list = FALSE, times = 1)
dtm.With_stopW_TRAIN            <- dtm.With_stopW$dtm_2[dtm.With_stopW.Index,]
dtm.With_stopW_TEST             <- dtm.With_stopW$dtm_2[-dtm.With_stopW.Index,]
dtm_collapsed.With_stopW.Index <- caret::createDataPartition(dtm_collapsed.With_stopW$character, p = .75, list = FALSE, times = 1)
dtm_collapsed.With_stopW_TRAIN  <- dtm_collapsed.With_stopW$dtm_2[dtm_collapsed.With_stopW.Index,]
dtm_collapsed.With_stopW_TEST   <- dtm_collapsed.With_stopW$dtm_2[-dtm_collapsed.With_stopW.Index,]
dtm_hybrid.With_stopW.Index <- caret::createDataPartition(dtm_hybrid.With_stopW$character, p = .75, list = FALSE, times = 1)
dtm_hybrid.With_stopW_TRAIN     <- dtm_hybrid.With_stopW$dtm_2[dtm_hybrid.With_stopW.Index,]
dtm_hybrid.With_stopW_TEST      <- dtm_hybrid.With_stopW$dtm_2[-dtm_hybrid.With_stopW.Index,]


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
#####                                                                      #####
#####                                 Tuning                               #####
#####                                                                      #####
################################################################################
# crea un oggetto di controllo per l'addestramento del modello
ctrl <- caret::trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 5,
                            verboseIter = F)


################################################################################
#####                  Random Forest with SMOTE Oversampling               #####
dtm_collapsed.stopW_model <- caret::train(x = dtm_collapsed.With_stopW_TRAIN,
                                               y = dtm_collapsed.With_stopW$character[dtm_collapsed.With_stopW.Index],
                                               method = "rf",
                                               tuneGrid = expand.grid(mtry = seq(1, ncol(dtm_collapsed.With_stopW_TRAIN), by = 1)),
                                               trControl = ctrl,
                                               allowParallel=TRUE)

dtm_collapsed.model         <- caret::train(x = dtm_collapsed_TRAIN,
                                            y = dtm_collapsed$character[dtm_collapsed.Index],
                                            method = "rf",
                                            tuneGrid = expand.grid(mtry = seq(1, ncol(dtm_collapsed_TRAIN), by = 1)),
                                            trControl = ctrl,
                                            allowParallel=TRUE)

dtm_hybrid.stopW_model <- caret::train(x = dtm_hybrid.With_stopW_TRAIN,
                                            y = dtm_hybrid.With_stopW$character[dtm_hybrid.With_stopW.Index],
                                            method = "rf",
                                            tuneGrid = expand.grid(mtry = seq(1, ncol(dtm_hybrid.With_stopW_TRAIN), by = 1)),
                                            trControl = ctrl,
                                            allowParallel=TRUE)

dtm_hybrid.model            <- caret::train(x = dtm_hybrid_TRAIN,
                                            y = dtm_collapsed$character[dtm_collapsed.Index],
                                            method = "rf",
                                            tuneGrid = expand.grid(mtry = seq(1, ncol(dtm_hybrid_TRAIN), by = 1)),
                                            trControl = ctrl,
                                            allowParallel=TRUE)

dtm.model                   <- caret::train(x = dtm_TRAIN,
                                            y = dtm$character[dtm.Index],
                                            method = "rf",
                                            tuneGrid = expand.grid(mtry = seq(1, ncol(dtm_TRAIN), by = 1)),
                                            trControl = ctrl,
                                            allowParallel=TRUE)

#dtm_collapsed.With_stopW_PRED <- predict(dtm_collapsed.With_stopW_model, newdata = dtm_collapsed.With_stopW_TEST[, -which(names(dtm_collapsed.With_stopW_TEST) == "character")])
#dtm_collapsed.With_stopW_CONF <- caret::confusionMatrix(data = dtm_collapsed.With_stopW_PRED, reference = dtm_collapsed.With_stopW_TEST$character)

save(dtm_collapsed.stopW_model, dtm_collapsed.model,
     dtm_hybrid.stopW_model, dtm_hybrid.model, dtm.model,
     file = "server_computation.DTM_RF2.RData")


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

########## TEST with caret ############
tuneGrid <- expand.grid(trials = c(5, 10, 20, 30),
                        model = c("tree", "rules"),
                        winnow = c(TRUE, FALSE))

ctrl <- caret::trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 5,
                            verboseIter = FALSE)

caretC50 <- function(X_train, y_train, X_test, y_test, tuneControl, tuneGrid){
    model <- caret::train(x = X_train,
                          y = y_train,
                          method = 'C5.0',
                          trControl = tuneControl,
                          tuneGrid = tuneGrid,
                          allowParallel = T)

    all_models_accuracy <- mean(model$resample$Accuracy)
    prediction <- predict(model, newdata = X_test)
    confusion_matrix <- caret::confusionMatrix(data = prediction, reference = y_test)
    confusion_matrix <- round(confusion_matrix$byClass * 100, 2)
    latex_table <- xtable(as.data.frame(round(confusion_matrix$byClass * 100, 2)), caption = "Metrics Table") # Sensitivity, Specificity, Precision
    
    return(list(
        model <- model,
        average.accuracy.across.models <- all_models_accuracy,
        prediction <- prediction,
        confusion_matrix <- confusion_matrix,
        latex_table <- latex_table))
}

c50.dtm              <- caretC50(X_train = dtm_TRAIN[, -which(names(dtm_TRAIN) == 'character')], y_train = dtm_TRAIN['character'], X_test=dtm_TEST[, -which(names(dtm_TEST) == 'character')], y_test=dtm_TEST['character'], tuneControl = ctrl, tuneGrid)

c50.dtm_collapsed    <- caretC50(dtm_collapsed_TRAIN[, -which(names(dtm_collapsed_TRAIN) == 'character')], dtm_collapsed_TRAIN['character'], dtm_collapsed_TEST[, -which(names(dtm_collapsed_TEST) == 'character')], dtm_collapsed_TEST['character'], ctrl, tuneGrid)

c50.dtm_hybrid       <- caretC50(dtm_hybrid_TRAIN[, -which(names(dtm_hybrid_TRAIN) == 'character')], dtm_hybrid_TRAIN['character'], dtm_hybrid_TEST[, -which(names(dtm_hybrid_TEST) == 'character')], dtm_hybrid_TEST['character'], ctrl, tuneGrid)

c50.dtm.With_stopW            <- caretC50(dtm.With_stopW_TRAIN[, -which(names(dtm.With_stopW_TRAIN) == 'character')], dtm.With_stopW_TRAIN['character'], dtm.With_stopW_TEST[, -which(names(dtm.With_stopW_TEST) == 'character')], dtm.With_stopW_TEST['character'], ctrl, tuneGrid)

c50.dtm_collapsed.With_stopW  <- caretC50(dtm_collapsed.With_stopW_TRAIN[, -which(names(dtm_collapsed.With_stopW_TRAIN) == 'character')], dtm_collapsed.With_stopW_TRAIN['character'], dtm_collapsed.With_stopW_TEST[, -which(names(dtm_collapsed.With_stopW_TEST) == 'character')], dtm_collapsed.With_stopW_TEST['character'], ctrl, tuneGrid)

c50.dtm_hybrid.With_stopW     <- caretC50(dtm_hybrid.With_stopW_TRAIN[, -which(names(dtm_hybrid.With_stopW_TRAIN) == 'character')], dtm_hybrid.With_stopW_TRAIN['character'], dtm_hybrid.With_stopW_TEST[, -which(names(dtm_hybrid.With_stopW_TEST) == 'character')], dtm_hybrid.With_stopW_TEST['character'], ctrl, tuneGrid)



#model accuracy
#p.final <- predict(m, newdata = df.final_TEST[, -which(names(df.final_TEST) == "character")])
#confusion_matrix <- caret::confusionMatrix(data = p.final, reference = df.final_TEST$character)
#round(confusion_matrix$byClass * 100, 2)

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
