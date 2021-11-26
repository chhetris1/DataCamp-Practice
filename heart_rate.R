#PROJECT 2: HAERT DISEASE
library(readxl)
hd <- read_excel("Cleveland_hdxl .xlsx")
View(hd)
head(hd)
library(tidyverse)
hd %>% mutate(hd = factor(ifelse(class>0, 1, 0))) %>%
                          
  mutate(sex = factor(sex, 
                      levels = c(0,1), 
                      labels = c("female","male" ))) -> hd
head(hd)

#Now let's see if heart disease prevalence (hd$hd) varies by sex, age, and thalach
chisq.test(hd$hd, hd$sex) #both factors
#looks like hd prevelance varies by sex [X^2 = 22.043, df=1, p-value <0.001]
t.test(age~hd, data = hd) #age continuous, hd factor)
#t(300)=4.03, p<0.001, i.e. hd prevalence varies by age
t.test(thalach~hd, data = hd) #thalach continuous, hd factor
#t(272) = -7.85, p <0.0001 i.e. maximum heart hare has an effect on hd prevalence

hd %>% mutate(hd_labelled = ifelse(hd == 0, "No Disease", "Disease")) -> hd
ggplot(hd, aes(hd_labelled, age))+geom_boxplot()

ggplot(hd, aes(x=hd_labelled, fill = sex ))+
  geom_bar(position = "fill")+
  ylab("sex proportion")

ggplot(hd, aes(hd_labelled, thalach))+geom_boxplot()

#Now let's run a logistic regression 
model <- glm(hd~age+sex+thalach, data=hd, family = "binomial")
summary(model)
library(broom)
tidy_m <- tidy(model)
tidy_m
#calculating Odds Ratio
tidy_m$OR <- exp(tidy_m$estimate)
#calculating 95% CI
tidy_m$lower_CI <- exp(tidy_m$estimate - 1.96*tidy_m$std.error)
tidy_m$upper_CI <- exp(tidy_m$estimate + 1.96*tidy_m$std.error)
tidy_m
#get the predicted prob. in our dataset
pred_prob <- predict(model, hd, type = "response")
pred_prob
#with 50% probability as cut-off, label hd risk or not
hd$pred_hd <- ifelse(pred_prob >=0.5, 1,0)
mean(hd$pred_hd)
#create a newdata for a new case
newdata <- data.frame(age=53, sex="male", thalach=150)
newdata
p_new <- predict(model, newdata, type = "response")
p_new

#let's test some model performance metrics
library(Metrics)
#calculate auc, accuracy, classification error 
auc <- Metrics::auc(hd$hd, hd$pred_hd)
accuracy <- Metrics::accuracy(hd$hd, hd$pred_hd)
classification_error <- Metrics::ce(hd$hd, hd$pred_hd)
#one way to print it out
print(paste("AUC=", auc))
print(paste("Accuracy=", accuracy))
print(paste("classification Error", classification_error))
#confusion matrix
table(hd$hd, hd$pred_hd, dnn = c("True Status", "Predicted Status"))
