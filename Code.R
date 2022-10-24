setwd("D:/College/Big Data/Project")
df<-read.csv("heart_failure_clinical_records_dataset.csv")
head(df)
df$anaemia[df$anaemia==1]<-"Yes"
df$anaemia[df$anaemia==0]<-"No"
df$diabetes[df$diabetes==0]<-"No"
df$diabetes[df$diabetes==1]<-"Yes"
df$high_blood_pressure[df$high_blood_pressure==1]<-"Yes"
df$high_blood_pressure[df$high_blood_pressure==0]<-"No"
df$smoking[df$smoking==0]<-"No"
df$smoking[df$smoking==1]<-"Yes"
df$DEATH_EVENT[df$DEATH_EVENT==1]<-"Dead"
df$DEATH_EVENT[df$DEATH_EVENT==0]<-"Alive"
df$sex[df$sex==1]<-"Male"
df$sex[df$sex==0]<-"Female"
head(df)
df$anaemia<-as.character(df$anaemia)
df$diabetes<-as.character(df$diabetes)
df$high_blood_pressure<-as.character(df$high_blood_pressure)
df$sex<-as.character(df$sex)
df$smoking<-as.character(df$smoking)
df$DEATH_EVENT<-as.character(df$DEATH_EVENT)
DeathCounts <- table(df$DEATH_EVENT)
barplot(DeathCounts,main="Death Counts")
library(dplyr)
df2<-rename(count(df, sex, DEATH_EVENT),Freq = n)
df2
barplot(df2$Freq,names.arg=paste(df2$DEATH_EVENT, df2$sex),main="Gender Death Counts")
barplot(df2$Freq,names.arg=paste(df2$DEATH_EVENT, df2$sex),main="Gender Counts")
barplot(DeathCounts,main="Counts")
df$anaemia[df$anaemia=="Yes"]<-"Anaemia"
df$anaemia[df$anaemia=="No"]<-"No Anaemia"
df$diabetes[df$diabetes=="No"]<-"No Diabetes"
df$diabetes[df$diabetes=="Yes"]<-"Diabetes"
df$high_blood_pressure[df$high_blood_pressure=="Yes"]<-"High Blood Pressure"
df$high_blood_pressure[df$high_blood_pressure=="No"]<-"No High Blood Pressure"
df$smoking[df$smoking=="No"]<-"Not Smoker"
df$smoking[df$smoking=="Yes"]<-"Smoker"
head(df)
df3<-rename(count(df, high_blood_pressure, DEATH_EVENT),Freq = n)
df3
barplot(df3$Freq,names.arg=paste(df3$DEATH_EVENT, "with", df3$high_blood_pressure),main="Blood Pressure Counts")
df4<-rename(count(df, diabetes, DEATH_EVENT),Freq = n)
df4
barplot(df4$Freq,names.arg=paste(df4$DEATH_EVENT, "with", df4$diabetes),main="Diabetes Counts")
df5<-rename(count(df, anaemia, DEATH_EVENT),Freq = n)
df5
barplot(df5$Freq,names.arg=paste(df5$DEATH_EVENT, "with", df5$anaemia),main="Anaemia Counts")
create_train_test <- function(data, size = 0.8, train = TRUE) {
n_row = nrow(data)
total_row <- size * n_row
train_sample <- 1: total_row
if (train == TRUE) {
return (data[train_sample, ])
} else {
return (data[-train_sample, ])
}
}
shuffle_index <- sample(0:nrow(df))
head(shuffle_index)
df <- df[shuffle_index, ]
head(df)
data_train <- create_train_test(df, 0.8, train = TRUE)
data_test <- create_train_test(df, 0.8, train = FALSE)
dim(data_train)
dim(data_test)
prop.table(table(data_train$DEATH_EVENT))
prop.table(table(data_test$DEATH_EVENT))
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit <- rpart(DEATH_EVENT~., data = data_train, method = 'class')
rpart.plot(fit)
predict_unseen <-predict(fit, data_test, type = 'class')
table_mat <- table(data_test$DEATH_EVENT, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
install.packages("ggplot2")
library(ggplot2)
ggplot(df, aes(x=age, fill=DEATH_EVENT)) +
geom_histogram()
save.image("D:/College/Big Data/Project/.RData")
savehistory("D:/College/Big Data/Project/.Rhistory")
df <- subset (df, select = -time)
shuffle_index <- sample(0:nrow(df))
head(shuffle_index)
df <- df[shuffle_index, ]
head(df)
data_train <- create_train_test(df, 0.8, train = TRUE)
data_test <- create_train_test(df, 0.8, train = FALSE)
dim(data_train)
dim(data_test)
prop.table(table(data_train$DEATH_EVENT))
prop.table(table(data_test$DEATH_EVENT))
library(rpart)
library(rpart.plot)
fit <- rpart(DEATH_EVENT~., data = data_train, method = 'class')
rpart.plot(fit)
predict_unseen <-predict(fit, data_test, type = 'class')
table_mat <- table(data_test$DEATH_EVENT, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
save.image("D:/College/Big Data/Project/.RData")
savehistory("D:/College/Big Data/Project/.Rhistory")
load("H:/Omar's College/Big Data/Project/.RData")
