x <- read.csv("test_Y3wMUE5_7gLdaTN.csv")
head(x)

library(readr)
test_Y3wMUE5_7gLdaTN <- read_csv("test_Y3wMUE5_7gLdaTN.csv")
View(test_Y3wMUE5_7gLdaTN)

library(readr)
train_u6lujuX_CVtuZ9i <- read_csv("train_u6lujuX_CVtuZ9i.csv")
View(train_u6lujuX_CVtuZ9i)

library(dplyr)
library(ggplot2)
library(rpart)
loan <- read.csv("train_u6lujuX_CVtuZ9i.csv",na.strings = c("","NaN"," "))
prediction <- read.csv("test_Y3wMUE5_7gLdaTN.csv",na.strings = c("","NaN"," "))
prediction$Loan_Status <- as.factor("NA")

df.loan <- rbind(loan[,2:13],prediction[,2:13])


#Missing values Summary
Variable <- colnames(df.loan)
NA_count <- sapply(df.loan, function(x) sum(is.na(x)))
miss_summ <- data.frame(Variable,NA_count,row.names = NULL)
View(miss_summ)
miss_summ %>%
        arrange(desc(NA_count))
sum(is.na.data.frame(df.loan))

#Visualizing the data
library(ggplot2)
ggplot(df.loan, aes(x=Loan_Status, y=ApplicantIncome)) +
        geom_point() +
        geom_smooth()
ggplot(completerecords, aes(x=LoanAmount, y=ApplicantIncome)) + 
        geom_point() + 
        geom_smooth()

cat("There are total", sum(is.na(df.loan)), "missing values in the dataset")
mean(df.loan$LoanAmount, na.rm = TRUE)


write.csv(df.loan, "dfloan.csv")

summary(df.loan)

completerecords <- na.omit(df.loan)
View(completerecords)

#Linear Regression
#Linear Regression with only complete records
regressionCC <- lm(ApplicantIncome ~ LoanAmount + Loan_Amount_Term + Credit_History, data = completerecords)
print(regressionCC)
summary(regressionCC)
#Linear Regression with all data including NA values
regressionNA <- lm(ApplicantIncome ~ LoanAmount + Loan_Amount_Term + Credit_History, data = df.loan)
print(regressionNA)
summary(regressionNA)

regressionNA <- lm(ApplicantIncome ~ LoanAmount + Loan_Amount_Term, data = df.loan)
print(regressionNA)
summary(regressionNA)
