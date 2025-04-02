obesity <- read.csv('obesity.csv',header=TRUE)

obesity$obs_num <- 1:nrow(obesity)

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

# Export datasets
write.csv(obesity_test, file = "obesity_test.csv", row.names = TRUE)
write.csv(obesity_training, file = "obesity_training.csv", row.names = TRUE)
write.csv(obesity_validation, file = "obesity_validation.csv", row.names = TRUE)


res_logistic <- glm(relevel(Is_Obese, "no")~ relevel(family_history_with_overweight, "no") + Age
                    + relevel(Gender, "Female") + Height + Weight + relevel(FAVC, "no") + FCVC + NCP + relevel(CAEC, "no") 
                    + relevel(SMOKE, "no") + CH2O + relevel(SCC, "yes") + FAF + TUE + relevel(CALC, "no")
                    + MTRANS ,data=obesity_training,family=binomial())

summary(res_logistic)


res_logistic_step <- step(res_logistic,direction="backward",trace=TRUE)

summary(res_logistic_step)

#Testing the logistic regression without stepwise

confint.default(res_logistic)
exp(cbind(OR=coef(res_logistic),
          confint.default(res_logistic)))


obesity_test$predprobobese <- predict(res_logistic, newdata = obesity_test
                                       , type = "response")
obesity_test$predprobhealthy<-1-obesity_test$predprobobese
obesity_test$fitted.results <- ifelse(obesity_test$predprobobese >
                                      0.72,"yes","no")
mean(obesity_test$fitted.results != obesity_test$Is_Obese)
table(obesity_test$Is_Obese, obesity_test$fitted.results)

library(ROCR)

pr <- prediction(obesity_test$predprobobese, obesity_test$Is_Obese)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Testing the logistic regression with stepwise

confint.default(res_logistic_step)
exp(cbind(OR=coef(res_logistic_step),
          confint.default(res_logistic_step)))


obesity

mean(obesity_test$fitted.results != obesity_test$Is_Obese)

table(obesity_test$Is_Obese, obesity_test$fitted.results)

obesity_test$predprobobese <- predict(res_logistic_step, newdata = obesity_test
                                      , type = "response")
obesity_test$predprobhealthy<-1-obesity_test$predprobobese
obesity_test$fitted.results <- ifelse(obesity_test$predprobobese >
                                        0.72,"yes","no")
mean(obesity_test$fitted.results != obesity_test$Is_Obese)
table(obesity_test$Is_Obese, obesity_test$fitted.results)

library(ROCR)

pr_step <- prediction(obesity_test$predprobobese, obesity_test$Is_Obese)
prf_step <- performance(pr_step, measure = "tpr", x.measure = "fpr")
plot(prf_step)
auc_step <- performance(pr_step, measure = "auc")
auc_step <- auc_step@y.values[[1]]
auc_step






