# Check if required packages are installed, if not install them
if (!require("glmnet")) {
  install.packages("glmnet")
}

if (!require("tibble")) {
  install.packages("tibble")
}

if (!require("caret")) {
  install.packages("caret")
}

if (!require("corrplot")) {
  install.packages("corrplot")
}

# Load required libraries
library(tibble)
library(glmnet)
library(caret)
library(corrplot)

# Load data
dab_data <- read.csv("data/coventure_dab_julien.csv")

# List of columns to subset
columns_to_subset <- c("TLFBCEQ_Dab", "NewPLE_recode2", "anymonthlycannabisCV", "anymonthlycannabisPV", "DEM_01", "SES", "AvggramsyearPV", "AgeCannabisOnset", "anycaarmst1tot3")

# Subset the dataset with the list of columns and change 9999 to NA
subset_dataset <- dab_data[, columns_to_subset, drop = FALSE]
subset_dataset[subset_dataset == 9999] <- NA
subset_dataset <- na.omit(subset_dataset)
subset_dataset$anycaarmst1tot3 <- factor(subset_dataset$anycaarmst1tot3)

# View dataset 
tibble::as_tibble(subset_dataset)

# Adjusting DEM_01 values
subset_dataset$DEM_01 <- subset_dataset$DEM_01 - 1
subset_dataset$DEM_01 <- factor(subset_dataset$DEM_01)

# Check the distribution of the outcome variable
histogram(subset_dataset$anycaarmst1tot3, xlab = "my outcome", main = "subset_dataset")

# Compute correlation matrix
subset_dataset2 <- subset_dataset
subset_dataset2$anycaarmst1tot3 <- as.numeric(subset_dataset2$anycaarmst1tot3)
# Subset numeric columns only
numeric_subset <- subset_dataset2[, sapply(subset_dataset2, is.numeric)]
# Compute correlation matrix
correlation_matrix <- cor(numeric_subset, use = "pairwise.complete.obs")

# Create correlation plot
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, addrect = 2, diag = FALSE)

# Create training model
set.seed(42)
cv_10 <- trainControl(method = "cv", number = 10)

# Function to calculate accuracy
calc_acc <- function(actual, predicted) {
  mean(actual == predicted)
}

# Unweighted model
hit_elnet <- train(
  anycaarmst1tot3 ~ TLFBCEQ_Dab * NewPLE_recode2 * anymonthlycannabisCV * anymonthlycannabisPV * DEM_01 * SES * AvggramsyearPV * AgeCannabisOnset, 
  data = subset_dataset,
  method = "glmnet",
  trControl = cv_10
) 

print(hit_elnet)
opt_reg_params <- hit_elnet$bestTune
coefficients <- coef(hit_elnet$finalModel) 
calc_acc(actual = subset_dataset$anycaarmst1tot3, predicted = predict(hit_elnet, newdata = subset_dataset))
table(subset_dataset$anycaarmst1tot3, predict(hit_elnet, newdata = subset_dataset))
print(coefficients)

# Function to get the best alpha and lambda values
get_best_result <- function(caret_fit) {
  best <- which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result <- caret_fit$results[best, ]
  rownames(best_result) <- NULL
  best_result
}

# Weighted Model
Y <- subset_dataset2$anycaarmst1tot3
Y <- Y - 1
fraction_0 <- rep(1 - sum(Y == 0) / length(Y), sum(Y == 0))
fraction_1 <- rep(1 - sum(Y == 1) / length(Y), sum(Y == 1))
weights <- numeric(length(Y))
weighted <- TRUE
if (weighted == TRUE) {
  weights[Y == 0] <- fraction_0
  weights[Y == 1] <- fraction_1
} else {
  weights <- rep(1, length(Y))
}

hit_elnet <- train(
  anycaarmst1tot3 ~ TLFBCEQ_Dab + NewPLE_recode2 + anymonthlycannabisCV + anymonthlycannabisPV + DEM_01 + SES + AvggramsyearPV + AgeCannabisOnset, 
  data = subset_dataset,
  method = "glmnet",
  trControl = cv_10,
  weights = weights
)

print(hit_elnet)
coefficients <- coef(hit_elnet$finalModel) 
print(coefficients)
calc_acc(actual = subset_dataset$anycaarmst1tot3, predicted = predict(hit_elnet, newdata = subset_dataset))
table(subset_dataset$anycaarmst1tot3, predict(hit_elnet, newdata = subset_dataset))

#coefficients <- coef(hit_elnet$finalModel)
#print(coefficients)
opt_lambda <- hit_elnet$bestTune$lambda
opt_alpha <- hit_elnet$bestTune$alpha

# Fitting the model with only relevant predictors
# fit <- glmnet(as.matrix(subset_dataset[, -which(names(subset_dataset) == "anycaarmst1tot3")]), as.numeric(subset_dataset[, "anycaarmst1tot3"]), 
#               weights=weights,
#               lambda = opt_lambda,
#               alpha = opt_alpha,
#               family=binomial)
# coef(fit)
# newx=as.matrix(subset_dataset[, -which(names(subset_dataset) == "anycaarmst1tot3")])
# table(subset_dataset$anycaarmst1tot3, predict(fit,newx=newx))
# Interaction only model

# model1 <- lm(as.numeric(anycaarmst1tot3) ~ TLFBCEQ_Dab * anymonthlycannabisCV * DEM_01, data = subset_dataset)
# summary(model1)
# 
# # Model specifying which effects we want to look at
# model2 <- lm(as.numeric(anycaarmst1tot3) ~ TLFBCEQ_Dab + anymonthlycannabisCV + DEM_01 + 
#                TLFBCEQ_Dab:anymonthlycannabisCV + TLFBCEQ_Dab:DEM_01 + anymonthlycannabisCV:DEM_01, 
#              data = subset_dataset)
# 
# table(subset_dataset$anycaarmst1tot3, predict(model2, newdata = subset_dataset))
# summary(model2)

########################################################## Subset model

subset_dataset$anycaarmst1tot3 <- as.factor(subset_dataset$anycaarmst1tot3)#-1
model3 <- glm(anycaarmst1tot3 ~ TLFBCEQ_Dab + anymonthlycannabisCV + DEM_01 + SES +
               TLFBCEQ_Dab:anymonthlycannabisCV + TLFBCEQ_Dab:DEM_01 + anymonthlycannabisCV:DEM_01, 
             data = subset_dataset,
             family = binomial,
             weights=weights,
             )

summary(model3)

predicted_prob<- predict(model3, newdata = subset_dataset, type="response")
binary_output <- ifelse(predicted_prob >= 0.5, 1, 0)


table(subset_dataset$anycaarmst1tot3, binary_output)


####################################################### Full model 

model3 <- glm(anycaarmst1tot3 ~  TLFBCEQ_Dab * NewPLE_recode2 * anymonthlycannabisCV * anymonthlycannabisPV * DEM_01 * SES * AvggramsyearPV * AgeCannabisOnset,  
              data = subset_dataset,
#              weights=weights,
              family = binomial)

summary(model3)

predicted_prob<- predict(model3, newdata = subset_dataset, type="response")
binary_output <- ifelse(predicted_prob >= 0.5, 1, 0)


table(subset_dataset$anycaarmst1tot3, binary_output)
