tSparse <- readRDS("cache/tSparse_links.rds")
df <- readRDS("cache/dataframe_links.rds")

dt <- data.frame(tSparse, y = as.factor(df$y))
levels(dt$y) <- c("Fake", "Tr")

# Split dataset
library(caret)

set.seed(2108)
trainIndex <- createDataPartition(dt$y, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dtTrain <- dt[trainIndex, ]
dtTest <- dt[-trainIndex, ]

dim(dtTrain)
dim(dtTest)

dt_dl <- dtTrain[, -ncol(dtTrain)]
head(dt_dl)

comboInfo <- findLinearCombos(dt_dl)
comboInfo

# Train ----
set.seed(2108)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, classProbs = TRUE, 
                     savePredictions = TRUE, summaryFunction = twoClassSummary)

mtry_seq <- sqrt(ncol(dtTrain))
mtry_seq

links_rf_model <- train(y ~., data = dtTrain, method = "rf", 
                      trControl = ctrl, tuneGrid = expand.grid(.mtry = 16), metric = "ROC")

varImp(links_rf_model)

# Test performance ----
predictions <- predict(links_rf_model, dtTest, type = "raw")
cm <- confusionMatrix(predictions, dtTest$y, positive = "Tr")
cm

rf.predict <- predict(links_rf_model, dtTest, type = "prob")

predictions_prob <- as.vector(rf.predict[, 2])

library(pROC)
rocobj <- roc(dtTest$y, predictions_prob, ci=TRUE, of="auc", percent = FALSE)

rocobj_sen <- roc(dtTest$y, predictions, ci=TRUE, of="se", percent = FALSE)
{plot(rocobj, main="", percent = FALSE, ci=TRUE, print.auc=TRUE)
  plot(rocobj_sen, main="Confidence intervals", percent = FALSE, ci=TRUE, print.auc=TRUE)}

table(dt$wwwdiariodobrasilorg, dt$y)
table(dt$gglobocom, dt$y)


saveRDS(links_rf_model, file = "cache/links_rf_model.rds")
