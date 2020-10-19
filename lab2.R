library(tidyverse)
library(caret)
library(arsenal)
library(e1071)

grad <- read_csv("gradAdmit.csv")


# 1 a ---------------------------------------------------------------------


set.seed(258)
train = grad %>% sample_frac(0.8)
test = grad %>% setdiff(train)

train$gre = scale(train$gre)
train$gpa = scale(train$gpa)
train$rank = scale(train$rank)

train = train[sample(nrow(train)),]
fold1 = train[1:64,]
fold2 = train[65:128,]
fold3 = train[129:192,]
fold4 = train[193:256,]
fold5 = train[257:320,] 
#train1 = train1[order(train1$gpa),]

training1 = rbind(fold5, fold2, fold3, fold4)
validation1 = fold1
validation1$admit = as.factor(validation1$admit)
training1$admit = as.factor(training1$admit)

training2 = rbind(fold1, fold3, fold4, fold5)
validation2 = fold2
validation2$admit = as.factor(validation2$admit)
training2$admit = as.factor(training2$admit)
 
training3 = rbind(fold1, fold2, fold4, fold5)
validation3 = fold3
validation3$admit = as.factor(validation3$admit)
training3$admit = as.factor(training3$admit)

training4 = rbind(fold1, fold2, fold3, fold5)
validation4 = fold4
validation4$admit = as.factor(validation4$admit)
training4$admit = as.factor(training4$admit)

training5 = rbind(fold1, fold2, fold3, fold4)
validation5 = fold5
validation5$admit = as.factor(validation5$admit)
training5$admit = as.factor(training5$admit)

# build a basic model to see how it looks like
comparison = NULL
for (kernel in c('polynomial', 'radial')){
    for (degree in c(1,3,5)){
        for (gamma in exp(seq(-5,5, by = 5))){
            for (coefficient in 0:3){
                for (cost in exp(seq(1,9, by = 4))){
                    
                    model1 = svm(admit ~., data = training1, kernel = kernel, degree = degree, gamma = gamma, coef0 = coefficient, cost = cost)
                    prediction1 = predict(model1, validation1)
                    result1 = confusionMatrix(prediction1, validation1$admit)
                    final1 = result1$overall[1]
                    
                    model2 = svm(admit ~., data = training2, kernel = kernel, degree = degree, gamma = gamma, coef0 = coefficient, cost = cost)
                    prediction2 = predict(model2, validation2)
                    result2 = confusionMatrix(prediction2, validation2$admit)
                    final2 = result2$overall[1]
                    
                    model3 = svm(admit ~., data = training3, kernel = kernel, degree = degree, gamma = gamma, coef0 = coefficient, cost = cost)
                    prediction3 = predict(model3, validation3)
                    result3 = confusionMatrix(prediction3, validation3$admit)
                    final3 = result3$overall[1]
                    
                    model4 = svm(admit ~., data = training4, kernel = kernel, degree = degree, gamma = gamma, coef0 = coefficient, cost = cost)
                    prediction4 = predict(model4, validation4)
                    result4 = confusionMatrix(prediction4, validation4$admit)
                    final4 = result4$overall[1]
                    
                    model5 = svm(admit ~., data = training5, kernel = kernel, degree = degree, gamma = gamma, coef0 = coefficient, cost = cost)
                    prediction5 = predict(model5, validation5)
                    result5 = confusionMatrix(prediction5, validation5$admit)
                    final5 = result5$overall[1]
                    
                    precision = mean(final1, final2, final3, final4, final5)
                    result = c(kernel, degree, gamma, coefficient, cost, precision)
                    comparison = rbind(comparison, result)
                }
            }
        }
    }
}
# after one long loop, i realize linear kernal literally did not change no matter how i tune parameters
comparison = data.frame(comparison)

colnames(comparison) <- c('kernel', 'degree', 'gamma', 'coefficient', 'cost', 'precision')

write.csv(comparison, 'grid search svm result.csv')



# 1 c ---------------------------------------------------------------------
comparison$precision = as.numeric(as.character(comparison$precision))

best = comparison %>%
    arrange(desc(precision)) 
best = best[1,]

train$admit = as.factor(train$admit)
model = svm(admit ~., data = train, kernel = 'polynomial', degree = 5, gamma = 1, coef0 = 1, cost = 148.413)
test$admit = as.factor(test$admit)
prediction = predict(model, test)
result = confusionMatrix(prediction, test$admit)
final = result$overall[1]

