install.packages("glmnet")
install.packages("tibble")
install.packages("caret")



# Load data
dab_data<-read.csv("C:\\Users\\maria\\Documents\\UdeM\\Conrod Lab\\Proventure\\Dab\\Dab\\Logreg_Test_Dab+Julien++.csv")

#Variables included in the previous models: 
#TLFBCEQ_Dab            -> cateogircal (Y/N)  -> have they ever used dab
#NewPLE_recode2         -> categorical (0-3)  -> PLEs recoded to include T-CTRL and T-cannabis
#anymonthlycannabisCV   -> categorical (Y/N)  -> did they ever use cannabis at a frequency of once a month or higher
#anymonthlycannabisPV   -> categorical (Y/N)  -> did they ever use cannabis at a frequency of once a month or higher
#DEM_01                 -> categorical (M/F)
#SES                    -> ordinal
#AvggramsyearPV         -> continuous
#AgeCannabisOnset       -> contiunous
#anycaarmst1tot3 OR pleaveraget1t3  


#Other variables that may be of interest:
#anyweeklycannabisPV (cat, Y/N)
#anyweeklycannabisCV (cat, Y/N)
#cannabisquantPV (ordinal)
#AvgCannabisCV (ordinal)
#AvgCannabisPV (ordinal)
#DEPAPO_ALC_CV (cat, Y/N)
#DEPADO_12M_ALC_PV (cat, Y/N)
#anyHScannabisy1toy5 (cat, Y/N)
#missing is 9999


# List of columns to subset
columns_to_subset <- c("TLFBCEQ_Dab", "NewPLE_recode2", "anymonthlycannabisCV", "anymonthlycannabisPV", "DEM_01", "SES", "AvggramsyearPV", "AgeCannabisOnset", "anycaarmst1tot3")

# Subset the dataset with the list of columns and change 9999 to NA
subset_dataset <- dab_data[, columns_to_subset, drop = FALSE]

subset_dataset[subset_dataset == 9999] <- NA
subset_dataset = na.omit(subset_dataset)
subset_dataset$anycaarmst1tot3=factor(subset_dataset$anycaarmst1tot3)

library(tibble)
tibble::as_tibble(subset_dataset)

#check the distribution of our outcome variable
library(caret)
histogram(subset_dataset$anycaarmst1tot3, xlab = "my outcome",
          main = "subset_dataset")


# Load the corrplot package
install.packages("corrplot")
library(corrplot)

# Assuming your dataset is named dab_data
# Compute correlation matrix
subset_dataset2<-subset_dataset
subset_dataset2$anycaarmst1tot3=as.numeric(subset_dataset2$anycaarmst1tot3)
correlation_matrix <- cor(subset_dataset2, use = "pairwise.complete.obs")

# Create correlation plot
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addrect = 2, diag = FALSE)


#Create our training model
library(caret)
set.seed(42)
cv_5 = trainControl(method = "cv", number = 5)

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

############################################ Unweighted model ########################################
library(glmnet)
hit_elnet = train(
  anycaarmst1tot3 ~ TLFBCEQ_Dab* NewPLE_recode2*anymonthlycannabisCV* anymonthlycannabisPV* DEM_01* SES*AvggramsyearPV * AgeCannabisOnset, data = subset_dataset,
  method = "glmnet",
  trControl = cv_5
)

print(hit_elnet)

#Function to calculate the model accuracy
calc_acc(actual = subset_dataset$anycaarmst1tot3,
         predicted = predict(hit_elnet, newdata = subset_dataset))
table(subset_dataset$anycaarmst1tot3, predict(hit_elnet, newdata = subset_dataset))

coefficients <- coef(hit_elnet)
# Print the coefficients
print(coefficients)
coef(hit_elnet, "lambda.min")

#Function to get the best alpha and lambda values
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}



################################## Weighted Model ##################################################

#Here we are giving more weight to our CAARMS positive people since there are so few in the dataset
Y<-subset_dataset2$anycaarmst1tot3
Y<-Y-1
fraction_0 <- rep(1 - sum(Y == 0) / length(Y), sum(Y == 0))
fraction_1 <- rep(1 - sum(Y == 1) / length(Y), sum(Y == 1))
# assign that value to a "weights" vector
weights <- numeric(length(Y))
weighted=TRUE
if (weighted == TRUE) {
  weights[Y == 0] <- fraction_0
  weights[Y == 1] <- fraction_1
} else {
  weights <- rep(1, length(Y))
}

# create an initial model that includes the weights
hit_elnet = train(
  anycaarmst1tot3 ~ TLFBCEQ_Dab* NewPLE_recode2*anymonthlycannabisCV* anymonthlycannabisPV* DEM_01* SES*AvggramsyearPV * AgeCannabisOnset, data = subset_dataset,
  method = "glmnet",
  trControl = cv_5,
  weights=weights
)
print(hit_elnet)

#Calculate accuracy 
calc_acc(actual = subset_dataset$anycaarmst1tot3,
         predicted = predict(hit_elnet, newdata = subset_dataset))
table(subset_dataset$anycaarmst1tot3, predict(hit_elnet, newdata = subset_dataset))

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result}

coefficients <- coef(hit_elnet)
# Print the coefficients
print(coefficients)
coef(hit_elnet, "lambda.min")

########################### Fitting the model with only relevant predictors #################################
#Here, we use the best fit lambda value we printed earlier
fit = glmnet(as.matrix(subset_dataset[, -which(names(subset_dataset) == "anycaarmst1tot3")]), as.numeric(subset_dataset[,"anycaarmst1tot3"]), 
             lambda=0.02346958)
coef(fit)

#Interaction only model
model1<-lm(as.numeric(anycaarmst1tot3) ~ TLFBCEQ_Dab*anymonthlycannabisCV*DEM_01, data = subset_dataset)
summary(model1)

#Model specifying which effects we want to look at (because TLFBCEQ_Dab:anymonthlycannabisCV:DEM_01 in the previous model is NA)
model2<-lm(as.numeric(anycaarmst1tot3) ~ TLFBCEQ_Dab+anymonthlycannabisCV+DEM_01+TLFBCEQ_Dab:anymonthlycannabisCV+TLFBCEQ_Dab:DEM_01 +anymonthlycannabisCV:DEM_01, data = subset_dataset)
summary(model2)



