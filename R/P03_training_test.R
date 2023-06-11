#install.packages("parallel",Ncpus = 4)
library(parallel)
cores<-(as.numeric(parallel::detectCores()-2))
installAndLoadPackages(c("caret", "smotefamily", "parallel", "doParallel", "bnclassify", "C50", "xtable"), cores = cores)
HOME <- "/mnt/volume_fra1_01/AIMLEA4Suburra/R"
#HOME <- "/home/smoxy/AIMLEA4Suburra/R/"
#HOME <- "C:/Users/smoxy/Documents/UNI/AIMLEA/AIMLEA4Suburra/R"
setwd(HOME)
load("P02.RData")
load("server_computation.DTM_RF.RData")
load("server_computation.DTM_C50.RData")
load("server_computation.DF_C50.RData")

# START CLUSTERS
#ifelse(parallel::detectCores() <= 32, cores<-(as.numeric(parallel::detectCores()-2)), cores<-32)
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



#df.final['character'] <- as.factor(df.final['character'])
#df_collapsed.final['character'] <- as.factor(df_collapsed.final['character'])
#df_hybrid.final['character'] <- as.factor(df_hybrid.final['character'])
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
                            repeats = 1,
                            verboseIter = F,
                            sampling = "smote")

caretRF <- function(X_train, y_train, X_test, y_test, tuneControl){
    tuneGrid <- expand.grid(mtry = seq(2, sqrt(ncol(X_train)), by = 4))
    
    model <- caret::train(x = X_train,
                          y = y_train,
                          method = "rf",
                          #tuneGrid = tuneGrid,
                          trControl = tuneControl,
                          allowParallel=TRUE)
    return(NULL)
    all_models_accuracy <- mean(model$resample$Accuracy)
    prediction <- predict(model, newdata = X_test)
    confusion_matrix <- caret::confusionMatrix(data = prediction, reference = y_test)
    confusion_matrix <- round(confusion_matrix$byClass * 100, 3)  #"positive" "table" "overall" "byClass" "mode" "dots" 
    confusion_matrix <- confusion_matrix[,-6]
    latex_table <- xtable(as.data.frame(confusion_matrix), caption = "Metrics Table") # Sensitivity, Specificity, Precision
    gc()
    return(list(
        "model" = model,
        "average.accuracy.across.models" = all_models_accuracy,
        "prediction" = prediction,
        "confusion_matrix" = confusion_matrix,
        "latex_table" = latex_table))
}

################################################################################
#####                  Random Forest with SMOTE Oversampling               #####
RF.dtm              <- caretRF(X_train = dtm_TRAIN, y_train = dtm$character[dtm.Index], X_test=dtm_TEST, y_test=dtm$character[-dtm.Index], tuneControl = ctrl)

RF.dtm_collapsed    <- caretRF(dtm_collapsed_TRAIN, dtm_collapsed$character[dtm_collapsed.Index], dtm_collapsed_TEST, dtm_collapsed$character[-dtm_collapsed.Index], ctrl)

RF.dtm_hybrid       <- caretRF0(dtm_hybrid_TRAIN, dtm_hybrid$character[dtm_hybrid.Index], dtm_hybrid_TEST, dtm_hybrid$character[-dtm_hybrid.Index], ctrl)
save(RF.dtm, RF.dtm_collapsed, RF.dtm_hybrid, file = "server_computation.DTM_RF.RData")

RF.dtm.With_stopW            <- caretRF(dtm.With_stopW_TRAIN, dtm.With_stopW$character[dtm.With_stopW.Index], dtm.With_stopW_TEST, dtm.With_stopW$character[-dtm.With_stopW.Index], ctrl)

RF.dtm_collapsed.With_stopW  <- caretRF(dtm_collapsed.With_stopW_TRAIN, dtm_collapsed.With_stopW$character[dtm_collapsed.With_stopW.Index], dtm_collapsed.With_stopW_TEST, dtm_collapsed.With_stopW$character[-dtm_collapsed.With_stopW.Index], ctrl)

RF.dtm_hybrid.With_stopW     <- caretRF(dtm_hybrid.With_stopW_TRAIN, dtm_hybrid.With_stopW$character[dtm_hybrid.With_stopW.Index], dtm_hybrid.With_stopW_TEST, dtm_hybrid.With_stopW$character[-dtm_hybrid.With_stopW.Index], ctrl)

save(RF.dtm, RF.dtm_collapsed, RF.dtm_hybrid, RF.dtm.With_stopW, RF.dtm_collapsed.With_stopW, RF.dtm_hybrid.With_stopW, file = "server_computation.DTM_C50.RData")

rm(RF.final, RF.collapsed.final, RF.hybrid.final, RF.dtm, RF.dtm_collapsed, RF.dtm_hybrid, RF.dtm.With_stopW, RF.dtm_collapsed.With_stopW, RF.dtm_hybrid.With_stopW)

# find the best model based
findBestModel <- function(models) {
    best_model <- NULL
    best_specificity_model <- NULL
    best_accuracy <- 0
    best_specificity <- 0
    best_model_name <- ""
  
    for (model_name in names(models)) {
        model <- models[[model_name]]
        mean(c50.collapsed.final$confusion_matrix[,1])
        avg_accuracy <- mean(model$confusion_matrix[,1])
        avg_specificity <- mean(model$confusion_matrix[,2])
    
        if (avg_accuracy > best_accuracy) {
            best_model <- model
            best_accuracy <- avg_accuracy
            best_model_name <- model_name
        }

        if (avg_specificity > best_specificity) {
            best_specificity_model <- model
            best_specificity <- avg_specificity
            best_model_name <- model_name
        }

    } 
    return(list(
        "best_model" = best_model,
        "best_accuracy" = best_accuracy,
        "best_specificity_model" = best_specificity_model,
        "best_specificity" = best_specificity
    ))
}

result <- findBestModel(list(
  RF.dtm = RF.dtm,
  RF.dtm_collapsed = RF.dtm_collapsed,
  RF.dtm_hybrid = RF.dtm_hybrid,
  RF.dtm.With_stopW = RF.dtm.With_stopW,
  RF.dtm_collapsed.With_stopW = RF.dtm_collapsed.With_stopW,
  RF.dtm_hybrid.With_stopW = RF.dtm_hybrid.With_stopW
))

# Print the name and accuracy of the best model
print(result$best_model_name)
print(result$best_accuracy)



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


################################################################################
#####                                  C5.0                                #####
tuneGrid <- expand.grid(trials = c(5, 10, 20, 30),
                        model = c("tree", "rules"),
                        winnow = c(TRUE, FALSE))

ctrl <- caret::trainControl(method = "repeatedcv",
                            number = 10,
                            repeats = 5,
                            verboseIter = TRUE)

caretC50 <- function(X_train, y_train, X_test, y_test, tuneControl, tuneGrid){
    model <- caret::train(x = X_train,
                          y = y_train,
                          method = 'C5.0',
                          trControl = tuneControl,
                          tuneGrid = tuneGrid,
                          allowParallel = TRUE)

    all_models_accuracy <- mean(model$resample$Accuracy)
    prediction <- predict(model, newdata = X_test)
    confusion_matrix <- caret::confusionMatrix(data = prediction, reference = y_test)
    confusion_matrix <- round(confusion_matrix$byClass * 100, 3)  #"positive" "table" "overall" "byClass" "mode" "dots" 
    confusion_matrix <- confusion_matrix[,-6] #ELIMINO RECALL (è uguale alla sensitivity)
    latex_table <- xtable(as.data.frame(confusion_matrix), caption = "Metrics Table") # Sensitivity, Specificity, Precision
    gc()
    return(list(
        "model" = model,
        "average.accuracy.across.models" = all_models_accuracy,
        "prediction" = prediction,
        "confusion_matrix" = confusion_matrix,
        "latex_table" = latex_table))
}

c50.final           <- caretC50(X_train = df.final_TRAIN[, -c(which(names(df.final_TRAIN) == 'character'),which(names(df.final_TRAIN) == 'script_line'))], y_train = df.final_TRAIN$character, X_test=df.final_TEST[, -c(which(names(df.final_TEST) == 'character'),which(names(df.final_TEST) == 'script_line'))], y_test=df.final_TEST$character, tuneControl = ctrl, tuneGrid=tuneGrid)

c50.collapsed.final <- caretC50(X_train = df_collapsed.final_TRAIN[, -c(which(names(df_collapsed.final_TRAIN) == 'character'),which(names(df_collapsed.final_TRAIN) == 'script_line'))], y_train = df_collapsed.final_TRAIN$character, X_test=df_collapsed.final_TEST[, -c(which(names(df_collapsed.final_TEST) == 'character'),which(names(df_collapsed.final_TEST) == 'script_line'))], y_test=df_collapsed.final_TEST$character, tuneControl = ctrl, tuneGrid=tuneGrid)

c50.hybrid.final    <- caretC50(X_train = df_hybrid.final_TRAIN[, -c(which(names(df_hybrid.final_TRAIN) == 'character'),which(names(df_hybrid.final_TRAIN) == 'script_line'))], y_train = df_hybrid.final_TRAIN$character, X_test=df_hybrid.final_TEST[, -c(which(names(df_hybrid.final_TEST) == 'character'),which(names(df_hybrid.final_TEST) == 'script_line'))], y_test=df_hybrid.final_TEST$character, tuneControl = ctrl, tuneGrid=tuneGrid)
save(c50.final, c50.collapsed.final, c50.hybrid.final, file = "server_computation.DF_C50.RData")

c50.dtm              <- caretC50(X_train = dtm_TRAIN, y_train = dtm$character[dtm.Index], X_test=dtm_TEST, y_test=dtm$character[-dtm.Index], tuneControl = ctrl, tuneGrid)

c50.dtm_collapsed    <- caretC50(dtm_collapsed_TRAIN, dtm_collapsed$character[dtm_collapsed.Index], dtm_collapsed_TEST, dtm_collapsed$character[-dtm_collapsed.Index], ctrl, tuneGrid)

c50.dtm_hybrid       <- caretC50(dtm_hybrid_TRAIN, dtm_hybrid$character[dtm_hybrid.Index], dtm_hybrid_TEST, dtm_hybrid$character[-dtm_hybrid.Index], ctrl, tuneGrid)
save(c50.dtm, c50.dtm_collapsed, c50.dtm_hybrid, file = "server_computation.DTM_C50.RData")

c50.dtm.With_stopW            <- caretC50(dtm.With_stopW_TRAIN, dtm.With_stopW$character[dtm.With_stopW.Index], dtm.With_stopW_TEST, dtm.With_stopW$character[-dtm.With_stopW.Index], ctrl, tuneGrid)

c50.dtm_collapsed.With_stopW  <- caretC50(dtm_collapsed.With_stopW_TRAIN, dtm_collapsed.With_stopW$character[dtm_collapsed.With_stopW.Index], dtm_collapsed.With_stopW_TEST, dtm_collapsed.With_stopW$character[-dtm_collapsed.With_stopW.Index], ctrl, tuneGrid)

c50.dtm_hybrid.With_stopW     <- caretC50(dtm_hybrid.With_stopW_TRAIN, dtm_hybrid.With_stopW$character[dtm_hybrid.With_stopW.Index], dtm_hybrid.With_stopW_TEST, dtm_hybrid.With_stopW$character[-dtm_hybrid.With_stopW.Index], ctrl, tuneGrid)
save(c50.dtm, c50.dtm_collapsed, c50.dtm_hybrid, c50.dtm.With_stopW, c50.dtm_collapsed.With_stopW, c50.dtm_hybrid.With_stopW, file = "server_computation.DTM_C50.RData")


result <- findBestModel(list(
  c50.final = c50.final,
  c50.collapsed.final = c50.collapsed.final,
  c50.hybrid.final = c50.hybrid.final,
  c50.dtm = c50.dtm,
  c50.dtm_collapsed = c50.dtm_collapsed,
  c50.dtm_hybrid = c50.dtm_hybrid,
  c50.dtm.With_stopW = c50.dtm.With_stopW,
  c50.dtm_collapsed.With_stopW = c50.dtm_collapsed.With_stopW,
  c50.dtm_hybrid.With_stopW = c50.dtm_hybrid.With_stopW
))

# Print the name and accuracy of the best model
print(result$best_model_name)
print(result$best_accuracy)

rm(c50.final, c50.collapsed.final, c50.hybrid.final, c50.dtm, c50.dtm_collapsed, c50.dtm_hybrid, c50.dtm.With_stopW, c50.dtm_collapsed.With_stopW, c50.dtm_hybrid.With_stopW)

################################################################################
#####                                                                      #####
#####                            Bayesian Model                            #####
#####                                                                      #####
################################################################################
#####                   Model Averaged Naive Bayes Classifier              #####


save.image(file = "P03.RData", compress=T)



##STOP CLUSTER
stopCluster(cl)
rm(cl)
registerDoSEQ()