###########################################
###########################################
#**********   Flu Outbreak   *************#
###########################################
###########################################


library(outbreaks)
library(regexr)
library(gridExtra)
library(grid)

# convert ? to NAs
fluH7N9.china.2013$age[which(fluH7N9.china.2013$age == "?")] <- NA

# create a new column with case ID
fluH7N9.china.2013$case.ID <- paste("case", fluH7N9.china.2013$case.ID, sep = "_")
head(fluH7N9.china.2013)

# gather for plotting with ggplot2
library(tidyr)
fluH7N9.china.2013_gather <- gather(fluH7N9.china.2013, Group, Date, date.of.onset:date.of.outcome)

# rearrange group order
fluH7N9.china.2013_gather$Group <- factor(fluH7N9.china.2013_gather$Group, levels = c("date.of.onset", "date.of.hospitalisation", "date.of.outcome"))

# rename groups
library(plyr)
fluH7N9.china.2013_gather$Group <- mapvalues(fluH7N9.china.2013_gather$Group, from = c("date.of.onset", "date.of.hospitalisation", "date.of.outcome"), 
                                             to = c("Date of onset", "Date of hospitalisation", "Date of outcome"))

# renaming provinces
fluH7N9.china.2013_gather$province <- mapvalues(fluH7N9.china.2013_gather$province, 
                                                from = c("Anhui", "Beijing", "Fujian", "Guangdong", "Hebei", "Henan", "Hunan", "Jiangxi", "Shandong", "Taiwan"), 
                                                to = rep("Other", 10))

# add a level for unknown gender
levels(fluH7N9.china.2013_gather$gender) <- c(levels(fluH7N9.china.2013_gather$gender), "unknown")
fluH7N9.china.2013_gather$gender[is.na(fluH7N9.china.2013_gather$gender)] <- "unknown"

# rearrange province order so that Other is the last
fluH7N9.china.2013_gather$province <- factor(fluH7N9.china.2013_gather$province, levels = c("Jiangsu",  "Shanghai", "Zhejiang", "Other"))

# convert age to numeric
fluH7N9.china.2013_gather$age <- as.numeric(as.character(fluH7N9.china.2013_gather$age))

# preparing my ggplot2 theme of choice

library(ggplot2)
my_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "black"),
      legend.position = "bottom",
      legend.justification = "top", 
      legend.box = "horizontal",
      legend.box.background = element_rect(colour = "grey50"),
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}

ggplot(data = fluH7N9.china.2013_gather, aes(x = Date, y = age, fill = outcome)) +
  stat_density2d(aes(alpha = ..level..), geom = "polygon") +
  geom_jitter(aes(color = outcome, shape = gender), size = 1.5) +
  geom_rug(aes(color = outcome)) +
  labs(
    fill = "Outcome",
    color = "Outcome",
    alpha = "Level",
    shape = "Gender",
    x = "Date in 2013",
    y = "Age",
    title = "2013 Influenza A H7N9 cases in China",
    caption = ""
  ) +
  facet_grid(Group ~ province) +
  my_theme() +
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_brewer(palette="Set1", na.value = "grey50") +
  scale_fill_brewer(palette="Set1")

fluH7N9.china.2013_gather_2 <- fluH7N9.china.2013_gather[, -4] %>%
  gather(group_2, value, gender:province)

fluH7N9.china.2013_gather_2$value <- mapvalues(fluH7N9.china.2013_gather_2$value, from = c("m", "f", "unknown", "Other"), 
                                               to = c("Male", "Female", "Unknown gender", "Other province"))

fluH7N9.china.2013_gather_2$value <- factor(fluH7N9.china.2013_gather_2$value, 
                                            levels = c("Female", "Male", "Unknown gender", "Jiangsu", "Shanghai", "Zhejiang", "Other province"))

p1 <- ggplot(data = fluH7N9.china.2013_gather_2, aes(x = value, fill = outcome, color = outcome)) +
  geom_bar(position = "dodge", alpha = 0.7, size = 1) +
  my_theme() +
  scale_fill_brewer(palette="Set1", na.value = "grey50") +
  scale_color_brewer(palette="Set1", na.value = "grey50") +
  labs(
    x = "",
    y = "Count",
    title = "Numbers of flu cases - gender and province",
    caption = ""
  )

p2 <- ggplot(data = fluH7N9.china.2013_gather, aes(x = age, fill = outcome, color = outcome)) +
  geom_density(alpha = 0.3, size = 1) +
  geom_rug() +
  scale_color_brewer(palette="Set1", na.value = "grey50") +
  scale_fill_brewer(palette="Set1", na.value = "grey50") +
  my_theme() +
  labs(
    x = "Age",
    y = "Density",
    title = "Age distribution of flu cases",
    caption = ""
  )

ggplot(data = fluH7N9.china.2013_gather, aes(x = Date, y = age, color = outcome)) +
  geom_point(aes(shape = gender), size = 1.5, alpha = 0.6) +
  geom_path(aes(group = case.ID)) +
  facet_wrap( ~ province, ncol = 2) +
  my_theme() +
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_brewer(palette="Set1", na.value = "grey50") +
  scale_fill_brewer(palette="Set1") +
  labs(
    color = "Outcome",
    shape = "Gender",
    x = "Date in 2013",
    y = "Age",
    title = "Time series: Onset to outcome of flu"
  )

# preparing the data frame for modeling

library(dplyr)

dataset <- fluH7N9.china.2013 %>%
  mutate(hospital = as.factor(ifelse(is.na(date.of.hospitalisation), 0, 1)),
         gender_f = as.factor(ifelse(gender == "f", 1, 0)),
         province_Jiangsu = as.factor(ifelse(province == "Jiangsu", 1, 0)),
         province_Shanghai = as.factor(ifelse(province == "Shanghai", 1, 0)),
         province_Zhejiang = as.factor(ifelse(province == "Zhejiang", 1, 0)),
         province_other = as.factor(ifelse(province == "Zhejiang" | province == "Jiangsu" | province == "Shanghai", 0, 1)),
         days_onset_to_outcome = as.numeric(as.character(gsub(" days", "",
                                                              as.Date(as.character(date.of.outcome), format = "%Y-%m-%d") - 
                                                                as.Date(as.character(date.of.onset), format = "%Y-%m-%d")))),
         days_onset_to_hospital = as.numeric(as.character(gsub(" days", "",
                                                               as.Date(as.character(date.of.hospitalisation), format = "%Y-%m-%d") - 
                                                                 as.Date(as.character(date.of.onset), format = "%Y-%m-%d")))),
         age = as.numeric(as.character(age)),
         early_onset = as.factor(ifelse(date.of.onset < summary(fluH7N9.china.2013$date.of.onset)[[3]], 1, 0)),
         early_outcome = as.factor(ifelse(date.of.outcome < summary(fluH7N9.china.2013$date.of.outcome)[[3]], 1, 0))) %>%
  subset(select = -c(2:4, 6, 8))
rownames(dataset) <- dataset$case.ID
dataset <- dataset[, -1]
head(dataset)

# impute missing data

library(mice)
dataset_impute <- mice(dataset[, -1],  print = FALSE)
dataset_impute

dataset_complete <- merge(dataset[, 1, drop = FALSE], mice::complete(dataset_impute, 1), by = "row.names", all = TRUE)
rownames(dataset_complete) <- dataset_complete$Row.names
dataset_complete <- dataset_complete[, -1]

summary(dataset$outcome)

train_index <- which(is.na(dataset_complete$outcome))
train_data <- dataset_complete[-train_index, ]
test_data  <- dataset_complete[train_index, -1] # Removing the outcome column 
library(caret)
set.seed(27)
val_index <- createDataPartition(train_data$outcome, p = 0.7, list=FALSE)
val_train_data <- train_data[val_index, ]
val_test_data  <- train_data[-val_index, ]
val_train_X <- val_train_data[,-1]
val_test_X <- val_test_data[,-1]

# Decision tree

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

set.seed(27)
fit <- rpart(outcome ~ .,
             data = train_data,
             method = "class",
             control = rpart.control(xval = 10, minbucket = 2, cp = 0), parms = list(split = "information"))

fancyRpartPlot(fit)

# Predict which features are important

# prepare training scheme
library(randomForest)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# train the model
set.seed(27)
model <- train(outcome ~ ., data = train_data, method = "rf", preProcess = NULL, trControl = control)

# estimate variable importance
importance <- varImp(model, scale=TRUE)

# prepare for plotting
importance_df_1 <- importance$importance
importance_df_1$group <- rownames(importance_df_1)

importance_df_1$group <- mapvalues(importance_df_1$group, 
                                   from = c("age", "hospital2", "gender_f2", "province_Jiangsu2", "province_Shanghai2", "province_Zhejiang2",
                                            "province_other2", "days_onset_to_outcome", "days_onset_to_hospital", "early_onset2", "early_outcome2" ), 
                                   to = c("Age", "Hospital", "Female", "Jiangsu", "Shanghai", "Zhejiang",
                                          "Other province", "Days onset to outcome", "Days onset to hospital", "Early onset", "Early outcome" ))
f = importance_df_1[order(importance_df_1$Overall, decreasing = FALSE), "group"]

importance_df_2 <- importance_df_1
importance_df_2$Overall <- 0

importance_df <- rbind(importance_df_1, importance_df_2)

# setting factor levels
importance_df <- within(importance_df, group <- factor(group, levels = f))
importance_df_1 <- within(importance_df_1, group <- factor(group, levels = f))

ggplot() +
  geom_point(data = importance_df_1, aes(x = Overall, y = group, color = group), size = 2) +
  geom_path(data = importance_df, aes(x = Overall, y = group, color = group, group = group), size = 1) +
  scale_color_manual(values = rep(brewer.pal(1, "Set1")[1], 11)) +
  my_theme() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  labs(
    x = "Importance",
    y = "",
    title = "Determined with Random Forest and repeated cross validation (10 repeats, 10 times)"
  )

# Feature plot

dataset_complete_gather <- dataset_complete %>%
  mutate(set = ifelse(rownames(dataset_complete) %in% rownames(test_data), "Test Data", 
                      ifelse(rownames(dataset_complete) %in% rownames(val_train_data), "Validation Train Data",
                             ifelse(rownames(dataset_complete) %in% rownames(val_test_data), "Validation Test Data", "NA"))),
         case_ID = rownames(.)) %>%
  gather(group, value, age:early_outcome)

dataset_complete_gather$group <- mapvalues(dataset_complete_gather$group, 
                                           from = c("age", "hospital", "gender_f", "province_Jiangsu", "province_Shanghai", "province_Zhejiang",
                                                    "province_other", "days_onset_to_outcome", "days_onset_to_hospital", "early_onset", "early_outcome" ), 
                                           to = c("Age", "Hospital", "Female", "Jiangsu", "Shanghai", "Zhejiang",
                                                  "Other province", "Days onset to outcome", "Days onset to hospital", "Early onset", "Early outcome" ))

ggplot(data = dataset_complete_gather, aes(x = as.numeric(value), fill = outcome, color = outcome)) +
  geom_density(alpha = 0.2) +
  geom_rug() +
  scale_color_brewer(palette="Set1", na.value = "grey50") +
  scale_fill_brewer(palette="Set1", na.value = "grey50") +
  my_theme() +
  facet_wrap(set ~ group, ncol = 11, scales = "free") +
  labs(
    x = "Value",
    y = "Density",
    title = "Density distribution of train and test data"
  )

# ML algorithms to find the best model

## Random Forest:

set.seed(27)
model_rf <- caret::train(outcome ~ .,
                         data = val_train_data,
                         method = "rf",
                         preProcess = NULL,
                         trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE))
model_rf
confusionMatrix(predict(model_rf, val_test_data[, -1]), val_test_data$outcome)

## Elastic net:

set.seed(27)
model_glmnet <- caret::train(outcome ~ .,
                             data = val_train_data,
                             method = "glmnet",
                             preProcess = NULL,
                             trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE))
model_glmnet
confusionMatrix(predict(model_glmnet, val_test_data[, -1]), val_test_data$outcome)

## KNN:

set.seed(27)
model_kknn <- caret::train(outcome ~ .,
                           data = val_train_data,
                           method = "kknn",
                           preProcess = NULL,
                           trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE))
model_kknn
confusionMatrix(predict(model_kknn, val_test_data[, -1]), val_test_data$outcome)

# Comparing the accuracy of three models

# Create a list of models
models <- list(rf = model_rf, glmnet = model_glmnet, kknn = model_kknn)

# Resample the models
resample_results <- resamples(models)

# Generate a summary
summary(resample_results, metric = c("Kappa", "Accuracy"))
bwplot(resample_results , metric = c("Kappa","Accuracy"))

# Predicting results of validation test samples

results <- data.frame(randomForest = predict(model_rf, newdata = val_test_data[, -1], type="prob"),
                      glmnet = predict(model_glmnet, newdata = val_test_data[, -1], type="prob"),
                      kknn = predict(model_kknn, newdata = val_test_data[, -1], type="prob"))

results$sum_Death <- rowSums(results[, grep("Death", colnames(results))])
results$sum_Recover <- rowSums(results[, grep("Recover", colnames(results))])
results$log2_ratio <- log2(results$sum_Recover/results$sum_Death)
results$true_outcome <- val_test_data$outcome
results$pred_outcome <- ifelse(results$log2_ratio > 1.5, "Recover", ifelse(results$log2_ratio < -1.5, "Death", "uncertain"))
results$prediction <- ifelse(results$pred_outcome == results$true_outcome, "CORRECT", 
                             ifelse(results$pred_outcome == "uncertain", "uncertain", "wrong"))
results[, -c(1:6)] # just displaying the scores (death/recover)

# Predicting all the unknown outcomes (NA's - )

set.seed(27)
model_rf <- caret::train(outcome ~ .,
                         data = train_data,
                         method = "rf",
                         preProcess = NULL,
                         trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE))
model_glmnet <- caret::train(outcome ~ .,
                             data = train_data,
                             method = "glmnet",
                             preProcess = NULL,
                             trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE))
model_kknn <- caret::train(outcome ~ .,
                           data = train_data,
                           method = "kknn",
                           preProcess = NULL,
                           trControl = trainControl(method = "repeatedcv", number = 10, repeats = 10, verboseIter = FALSE))

models <- list(rf = model_rf, glmnet = model_glmnet, kknn = model_kknn)

# Resample the models
resample_results <- resamples(models)

bwplot(resample_results , metric = c("Kappa","Accuracy"))

results <- data.frame(randomForest = predict(model_rf, newdata = test_data, type="prob"),
                      glmnet = predict(model_glmnet, newdata = test_data, type="prob"),
                      kknn = predict(model_kknn, newdata = test_data, type="prob"))
results$sum_Death <- rowSums(results[, grep("Death", colnames(results))])
results$sum_Recover <- rowSums(results[, grep("Recover", colnames(results))])
results$log2_ratio <- log2(results$sum_Recover/results$sum_Death)
results$predicted_outcome <- ifelse(results$log2_ratio > 1.5, "Recover", ifelse(results$log2_ratio < -1.5, "Death", "uncertain"))
results[, -c(1:6)]


