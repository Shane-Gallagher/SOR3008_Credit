obesity <- read.csv('obesity.csv',header=TRUE)

str(obesity)




obesity$Gender<-as.factor(obesity$Gender)
obesity$family_history_with_overweight<-as.factor(obesity$family_history_with_overweight)
obesity$FAVC<-as.factor(obesity$FAVC)
obesity$CAEC<-as.factor(obesity$CAEC)
obesity$SMOKE<-as.factor(obesity$SMOKE)
obesity$SCC<-as.factor(obesity$SCC)
obesity$CALC<-as.factor(obesity$CALC)
obesity$MTRANS<-as.factor(obesity$MTRANS)
obesity$NObeyesdad<-as.factor(obesity$NObeyesdad)
obesity$Is_Obese<-as.factor(obesity$Is_Obese)

str(obesity)

set.seed(3008)  # for reproducibility
n <- nrow(obesity)

# Create training indices (70% of data)
train_index <- sample(1:n, size = round(0.7 * n))
obesity_training <- obesity[train_index, ]

# The remaining indices (30% of data)
remaining <- obesity[-train_index, ]
n_remaining <- nrow(remaining)

# From the remaining, assign validation set (one-third of remaining ≈10% overall)
valid_index <- sample(1:n_remaining, size = round(0.3333 * n_remaining))
obesity_validation <- remaining[valid_index, ]

# The test set is the rest of the remaining data (≈20% overall)
obesity_test <- remaining[-valid_index, ]

# Check sizes
cat("Training set size:", nrow(obesity_training), "\n")
cat("Validation set size:", nrow(obesity_validation), "\n")
cat("Test set size:", nrow(obesity_test), "\n")


res_logistic <- glm(relevel(Is_Obese, "yes")~ relevel(family_history_with_overweight, "yes") + Age
                    + relevel(Gender, "Male") + Height + Weight + relevel(FAVC, "yes") + FCVC + NCP + relevel(CAEC, "Always") 
                    + relevel(SMOKE, "yes") + CH2O + relevel(SCC, "yes") + FAF + TUE + relevel(CALC, "Always")
                    + MTRANS ,data=obesity,family=binomial())

summary(res_logistic)

confint.default(res_logistic)
exp(cbind(OR=coef(res_logistic),
          confint.default(res_logistic)))

obesity$predprobobese <- predict(res_logistic, newdata = obesity,
                                type = "response")

summary(obesity)
obesity$predprobhealthy<-1-obesity$predprobobese
#If 50% probability cut-off
obesity$fitted.results <- ifelse(obesity$predprobobese >
                                 0.5,"yes","no")

obesity

mean(obesity$fitted.results != obesity$Is_Obese)

table(obesity$Is_Obese, obesity$fitted.results)

obesity_test$predprobobese <- predict(res_logistic, newdata = obesity_test
                                       , type = "response")
obesity_test$predprobhealthy<-1-obesity_test$predprobobese
obesity_test$fitted.results <- ifelse(obesity_test$predprobobese >
                                      0.5,"yes","no")
mean(obesity_test$fitted.results != obesity_test$Is_Obese)
table(obesity_test$Is_Obese, obesity_test$fitted.results)

install.packages("ROCR")
library(ROCR)

pr <- prediction(obesity$predprobobese, obesity$Is_Obese)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc






