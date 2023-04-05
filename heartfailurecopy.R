# Installing Packages and loading them
install.packages("tidyverse")
install.packages("corrplot")
install.packages("olsrr")

library("tidyverse")
library("corrplot")
library("olsrr")

# Set my working directory

setwd("C:/Users/ual-laptop/OneDrive - University of Arizona/Desktop/ECON Project")

# Read my csv file
heartFailureC <- read_csv(file = "heartfailure.csv",col_types = "nlnlnlnnnllnl",col_names = TRUE)

#Summaries
summary(heartFailureC)
summary(heartFailureC$anaemia)
summary(heartFailureC$diabetes)
summary(heartFailureC$high_blood_pressure)
summary(heartFailureC$sex)
summary(heartFailureC$smoking)
summary(heartFailureC$DEATH_EVENT)

# Removing Outliers
heartFailureCO <- heartFailureC %>%
  mutate(max1 = quantile(creatinine_phosphokinase, .75) + (1.5 *IQR(creatinine_phosphokinase))) %>%
  mutate(max2 = quantile(ejection_fraction, .75) + (1.5 *IQR(ejection_fraction))) %>%
  mutate(max3 = quantile(platelets, .75) + (1.5 * IQR(platelets))) %>%
  mutate(max4 = quantile(serum_creatinine, .75) + (1.5 *IQR(serum_creatinine))) %>%
  mutate(max5 = quantile(serum_sodium, .75) + (1.5 *IQR(serum_sodium))) %>%
  filter(creatinine_phosphokinase <= max1) %>%
  filter(ejection_fraction <= max2) %>%
  filter(platelets <= max3) %>%
  filter(serum_creatinine <= max4) %>%
  filter(serum_sodium <= max5) %>%
  select(-max1,-max2,-max3,-max4,-max5)
#Running the linear regression model on all variables

heartFailureLinearModel <- lm(DEATH_EVENT ~., data = heartFailureCO)
summary(heartFailureLinearModel)

ols_plot_resid_fit(heartFailureLinearModel)

# all variables excluding time
hFlM_ex_time <- lm(DEATH_EVENT ~.-(time), data = heartFailureCO)
summary(hFlM_ex_time)

# all variables excluding time and sex
hFlM_ex_time_sex <- lm(DEATH_EVENT ~.-(time + sex), data = heartFailureC)
summary(hFlM_ex_time_sex)
# #age
# heartFailureLinearModelAGE <- lm(DEATH_EVENT ~ age, data = heartFailureC)
# summary(heartFailureLinearModelAGE)
# 
# #anaemia
# heartFailureLinearModelanaemia <- lm(DEATH_EVENT ~ anaemia, data = heartFailureC)
# summary(heartFailureLinearModelanaemia)
# 
# #CPK
heartFailureLinearModelCPK <- lm(DEATH_EVENT ~ creatinine_phosphokinase
                                      , data = heartFailureC)
summary(heartFailureLinearModelCPK)
# 
# #diabeties
# heartFailureLinearModeldiabetes <- lm(DEATH_EVENT ~ diabetes
#                                  , data = heartFailureC)
# summary(heartFailureLinearModeldiabetes)
# 
# # Ejection Fraction
# heartFailureLinearModelejection_fraction <- lm(DEATH_EVENT ~ ejection_fraction
#                                       , data = heartFailureC)
# summary(heartFailureLinearModelejection_fraction)
# 
# # high_blood_pressure
# heartFailureLinearModelBP <- lm(DEATH_EVENT ~ high_blood_pressure
#                                                , data = heartFailureC)
# summary(heartFailureLinearModelBP)
# 
# # platelets
# heartFailureLinearModelplatelets <- lm(DEATH_EVENT ~ platelets
#                                 , data = heartFailureC)
# summary(heartFailureLinearModelplatelets)
# 
# # serum_creatinine
# heartFailureLinearModelSC <- lm(DEATH_EVENT ~ serum_creatinine
#                                        , data = heartFailureC)
# summary(heartFailureLinearModelSC)
# 
# # serum_sodium
# heartFailureLinearModelSs <- lm(DEATH_EVENT ~ serum_sodium
#                                 , data = heartFailureC)
# summary(heartFailureLinearModelSs)
# 
# # sex
# heartFailureLinearModelsex <- lm(DEATH_EVENT ~ sex
#                                 , data = heartFailureC)
# summary(heartFailureLinearModelsex)
# 
 # smoking
heartFailureLinearModelsmoking <- lm(DEATH_EVENT ~ smoking
                                , data = heartFailureC)
summary(heartFailureLinearModelsmoking)
# 
# # time
 heartFailureLinearModeltime <- lm(DEATH_EVENT ~ time
                                      , data = heartFailureCO)
 summary(heartFailureLinearModeltime)

# only adding significant variables
hFlM_sig_variable <- lm(DEATH_EVENT ~.-(anaemia 
                                        + diabetes 
                                        + high_blood_pressure
                                        + platelets
                                        + smoking), data = heartFailureC)
summary(hFlM_sig_variable)

hFlM_sig_variable1 <- lm(DEATH_EVENT ~.-(anaemia 
                                        + diabetes 
                                        + high_blood_pressure
                                        + platelets
                                        + smoking
                                        + time), data = heartFailureC)
summary(hFlM_sig_variable1)

# CPK and ejection
heartFailureLinearModelCPKEJ <- lm(DEATH_EVENT ~ creatinine_phosphokinase +
                                   ejection_fraction
                                 , data = heartFailureC)
summary(heartFailureLinearModelCPKEJ)

# Smoking along with other
Smoking_Dia_Bp<- lm(DEATH_EVENT ~ smokingTrue)

# Check for multi collinearity
ols_vif_tol(heartFailureLinearModel)

# Histograms function
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value,fill=key),
                              color = "black")+
    facet_wrap( ~ key,scales= "free")+
    theme_minimal()
}

# Display histograms
displayAllHistograms(heartFailureC)
str(heartFailureC)

displayAllHistograms(heartFailureCO)
str(heartFailureCO)
# Corelation values
cor(heartFailureC$DEATH_EVENT ,heartFailureC$anaemia)

    
round(cor(heartFailureC),digits = 2)

corrplot(cor(heartFailureC),
         method = "number",
         type = "lower",
         tl.cex = 0.5)

# For Logistic Regression
heartFailureCLog1 <- glm(data=heartFailureC, family=binomial,
                         formula=DEATH_EVENT ~ .)
summary(heartFailureCLog1)
# Removing Outliers
heartFailureClog <- heartFailureC %>%
  mutate(max1 = quantile(creatinine_phosphokinase, .75) + (1.5 *IQR(creatinine_phosphokinase))) %>%
  mutate(max2 = quantile(ejection_fraction, .75) + (1.5 *IQR(ejection_fraction))) %>%
  mutate(max3 = quantile(platelets, .75) + (1.5 * IQR(platelets))) %>%
  mutate(max4 = quantile(serum_creatinine, .75) + (1.5 *IQR(serum_creatinine))) %>%
  mutate(max5 = quantile(serum_sodium, .75) + (1.5 *IQR(serum_sodium))) %>%
  filter(creatinine_phosphokinase <= max1) %>%
  filter(ejection_fraction <= max2) %>%
  filter(platelets <= max3) %>%
  filter(serum_creatinine <= max4) %>%
  filter(serum_sodium <= max5) %>%
  select(-max1,-max2,-max3,-max4,-max5)

# Dividing into training and testing data set
set.seed(252)
sampleSet <- sample(nrow(heartFailureClog),
                    round(nrow(heartFailureClog)*.75),
                    replace = FALSE)

heartFailureCTraining <- heartFailureClog[sampleSet, ]
heartFailureCTest <- heartFailureClog[-sampleSet, ]

summary(heartFailureCTraining$DEATH_EVENT)

#the code below is needed if we want to tackle class imbalance and
#balance our dependent variable
# Installing smote packages
install.packages("smotefamily")
library(smotefamily)

# Smooting the training dataset
heartFailureCTrainingSmoted <- tibble(SMOTE(
  X=data.frame(heartFailureCTraining),
  target = heartFailureCTraining$DEATH_EVENT,
  dup_size = 1)$data)

summary(heartFailureCTrainingSmoted)

# converting back into logical data types
heartFailureCTrainingSmoted <- heartFailureCTrainingSmoted %>%
  mutate(DEATH_EVENT = as.logical(DEATH_EVENT),
         anaemia = as.logical(anaemia),
         diabetes= as.logical(diabetes),
         high_blood_pressure = as.logical(high_blood_pressure),
         sex = as.logical(sex),
         smoking = as.logical(smoking))

summary(heartFailureCTrainingSmoted)

# removing class variablke which is added by smooting
heartFailureCTrainingSmoted <- heartFailureCTrainingSmoted %>%
  select(-class)

summary(heartFailureCTrainingSmoted)



# Runnning logistic regression
heartFailureCLogistic <- glm(data=heartFailureCTraining, family=binomial,
                       formula=DEATH_EVENT ~ .)
summary(heartFailureCLogistic)



# Installing stargazer for quality regression table
install.packages("stargazer")
library(stargazer)
help("stargazer-package")
stargazer(heartFailureLinearModel)

# converting into odds ratio
exp(coef(heartFailureCLog1)["age"])
exp(coef(heartFailureCLog1)["anaemiaTRUE"])
exp(coef(heartFailureCLog1)["creatinine_phosphokinase"])
exp(coef(heartFailureCLog1)["diabetesTRUE"])
exp(coef(heartFailureCLog1)["ejection_fraction"])
exp(coef(heartFailureCLog1)["high_blood_pressureTRUE"])
exp(coef(heartFailureCLog1)["platelets"])
exp(coef(heartFailureCLog1)["serum_creatinine"])
exp(coef(heartFailureCLog1)["serum_sodium"])
exp(coef(heartFailureCLog1)["sexTRUE"])
exp(coef(heartFailureCLog1)["smokingTRUE"])
exp(coef(heartFailureCLog1)["time"])

# using logistic tibble to  predict out our test dataset 
heartFailurePrediction <- predict(heartFailureCLogistic,
                                 heartFailureCTest,
                                 type='response')

print(heartFailurePrediction)

# converting the prediction into 0,1
heartFailurePrediction <- 
  ifelse(heartFailurePrediction >= 0.5,1,0)

# creating a confusion matrix
heartFailureConfusionMatrix <- table(heartFailureCTest$DEATH_EVENT,
                                     heartFailurePrediction)

print(heartFailureConfusionMatrix)

# Calculating false positive
heartFailureConfusionMatrix[1,2]/
  (heartFailureConfusionMatrix[1,2]+heartFailureConfusionMatrix[1,1])

# Calculating false negative
heartFailureConfusionMatrix[2,1]/
  (heartFailureConfusionMatrix[2,1]+heartFailureConfusionMatrix[2,2])

# Calculating Model Prediction Accuracy
sum(diag(heartFailureConfusionMatrix))/ nrow(heartFailureCTest)
