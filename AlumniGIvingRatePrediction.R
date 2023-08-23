#Including the required libraries
library(tidyverse)
library(moderndive)
library(skimr)
library(ISLR)
library(car)

#Loading data
url<-'/Users/shirinakbar/Your team Dropbox/Shirin Akbar/Mac/Documents/Fall 22/AlumniGiving.csv'
alumni <- read.csv(url)

#Looking raw data
glimpse(alumni)
#selecting the interested independent variables
alumni_reg <-alumni %>%
  select(Graduation_rate=Graduation.Rate,
         percent_of_classes_under_20=X..of.Classes.Under.20,
         student_faculty_ratio=Student.Faculty.Ratio,
         alumni_giving_rate=Alumni.Giving.Rate)
#Exploring data
glimpse(alumni_reg)
alumni_reg %>% sample_n(size=5)

#We have data for 48 universities
#Computing summary statistics & co-relation

summary(alumni_reg)
alumni_reg %>% skim()

alumni_reg %>%
  select(alumni_giving_rate,Graduation_rate, percent_of_classes_under_20,student_faculty_ratio)%>%
  cor()

##Data visualisation
boxplot(alumni_reg, use.cols = TRUE)
#Median > Mean for percent_of_classes_under_20 meaning percent_of_classes_under_20 has a left skewed distribution
#Mean > Median for student_faculty_ratio meaning student_faculty_ratio has a right skewed distribution
#Mean ~ Median for alumni_giving_rate meaning alumni_giving_rate has a symmetric distribution 
ggplot(alumni_reg, aes(x=percent_of_classes_under_20)) + geom_histogram(binwidth = 4,colour="black", fill="light blue")
ggplot(alumni_reg, aes(x=student_faculty_ratio)) + geom_histogram(binwidth = 4,colour="black", fill="light blue")
ggplot(alumni_reg, aes(x=alumni_giving_rate)) + geom_histogram(binwidth = 4,colour="black", fill="light blue")


#We observe that student_faculty_ratio has one outlier.
ggplot(alumni_reg, aes(x="", y=percent_of_classes_under_20)) + geom_boxplot()
ggplot(alumni_reg, aes(x="", y=student_faculty_ratio)) + geom_boxplot()

#####actual linear smooth

ggplot(alumni_reg, aes(x=student_faculty_ratio,y=alumni_giving_rate))+
  geom_point()+
  labs(x="Student and faculty Ratio", y="Alumni Giving Rate", title="Alumni giving rate and Student Faculty ratio")+
  geom_smooth(method="lm",se=FALSE)


ggplot(alumni_reg, aes(x=Graduation_rate,y=alumni_giving_rate))+
  geom_point()+
  labs(x="Graduation Rate", y="Alumni Giving Rate", title="Alumni giving rate and  Graduation Rate")+
  geom_smooth(method="lm",se=FALSE)
#one outliner for Graduation_rate
#positive relationship between graduatiuon rate and alumni giving rate. This is consistent with the coefficient of 0.756 we commuted earlier.


ggplot(alumni_reg, aes(x=percent_of_classes_under_20,y=alumni_giving_rate))+
  geom_point()+
  labs(x="Percent of classes under 20", y="Alumni Giving Rate", title="Alumni giving rate and  and Percent of classes under 20")+
  geom_smooth(method="lm",se=FALSE)
#positive relationship between Percent of classes under 20 and Alumni giving rate



#Regression
#Fit regression model
alumni_model_fit0 <-lm(alumni_giving_rate~Graduation_rate+percent_of_classes_under_20+student_faculty_ratio, data=alumni_reg)
summary(alumni_model_fit0)
#Get regression table
get_regression_table(alumni_model)

# The graduation rate value is 0.748. Taking into account of all the other explanatory variables in our model, for every increase
# of 1 percent in the graduation rate there is an associated increase of on average 0.748 in alumni giving.
# For every increase of one unit of student faculty ratio , there is an associated decrease of, on average , 1.19 in the alumni giving.
# Taking alpha as 5%(0.05), p value of percent_of_classes_under_20 is not significant( pvalue>alpha)). SO no
# linear relationship between alumni_giving_rate and percent_of_classes_under_20). So we can remove them
# Another reason we can reject percent_of_classes_under_20 is because the confidence interval
# (-0.0252,0.32) contains 0 in them.

alumni_model_fit1<-lm(alumni_giving_rate~.-percent_of_classes_under_20,data=alumni_reg)
summary(alumni_model_fit1)

#Both the models are comparable with large R-square value  (0.6999 and 0.6996 respectively). 
#Next step: To compare both the models
#compare both model with test data

#Model1
set.seed(123)
n = nrow(alumni_reg)
p = ncol(alumni_reg)
train.index <- sample(row.names(alumni_reg), floor(0.8*n))
test.index <- setdiff(row.names(alumni_reg), train.index)
train.df <- alumni_reg[train.index,]
test.df <- alumni_reg[test.index,]
#Fitting linear model for model 1 (including all the 3 predictors)
mod1<-lm(alumni_giving_rate~.,data=train.df)
summary(mod1)
preds.mod1 <- predict(mod1, newdata = test.df)
MSE1 <- mean((preds.mod1 - test.df$alumni_giving_rate)^2)
RMSE1 <- sqrt(MSE1)
print(RMSE1) 
#RMSE1=5.510

#Model2
#Fitting linear model for model 2 (excluding the predictor percent_of_classes_under_20)
mod2<-lm(alumni_giving_rate~Graduation_rate+student_faculty_ratio,data=train.df)
summary(mod2)
preds.mod2 <- predict(mod2, newdata = test.df[,c("Graduation_rate","student_faculty_ratio")])
MSE2 <- mean((preds.mod2 - test.df$alumni_giving_rate)^2)
RMSE2 <- sqrt(MSE2)
print(RMSE2)
#RMSE2=5.463
#The mean sqaured error of model 2 is smaller than that of model 1, which means that model 2 is better. 

#Exploratory data analysis for deciding the transformation

#Distribution of alumni giving rate
hist(alumni_reg$alumni_giving_rate, freq = FALSE, xlab="Alumni Giving rate", main = "Histogram of median values of alumni giving rates")
lines(density(alumni_reg$alumni_giving_rate), lwd = 2, col = 'red')
boxplot(alumni_reg$alumni_giving_rate)

#Side by side plot
pairs(alumni_reg[,c(1,2,3,4)])

#Alumni giving rate and graduation rate
ggplot(alumni_reg, aes(x=Graduation_rate,y=alumni_giving_rate))+
  geom_point()+
  labs(x="Graduation Rate", y="Alumni Giving Rate", title="Alumni giving rate and Graduation rate")+
  geom_smooth(method="lm",se=FALSE)
#one outliner for Graduation_rate, 
#positive relationship between graduation rate and alumni giving rate. This is consistent with the coefficient of 0.756 we commuted earlier.


#Alumni giving rate and student faculty ratio
ggplot(alumni_reg, aes(x=student_faculty_ratio,y=alumni_giving_rate))+
  geom_point()+
  labs(x="Student and faculty Ratio", y="Alumni Giving Rate", title="Alumni giving rate and  and Student Faculty ratio")+
  geom_smooth(method="lm",se=FALSE)
#One outliner 

#Distribution of graduation rate
ggplot(alumni_reg, aes(Graduation_rate)) + geom_histogram() + 
  ggtitle("Histogram of graduation rate") + 
  theme(plot.title = element_text(size = 8), axis.title = element_text(size = 8))

#Histogram of stdent faculty ratio
ggplot(alumni_reg, aes(student_faculty_ratio)) + geom_histogram() + 
  ggtitle("Histogram of student vs faculty ratio") + 
  theme(plot.title = element_text(size = 8), axis.title = element_text(size = 8))

#Model3
 alumni_reg$student_faculty_ratio_new=(alumni_reg$student_faculty_ratio)^2
 n = nrow(alumni_reg)
 p = ncol(alumni_reg)
 set.seed(123)
 train.index <- sample(row.names(alumni_reg), floor(0.8*n))
 test.index <- setdiff(row.names(alumni_reg), train.index)
 train.df <- alumni_reg[train.index,]
 test.df <- alumni_reg[test.index,]
 mod3<-lm(alumni_giving_rate~Graduation_rate+student_faculty_ratio_new, data=train.df)
 preds.mod3 <- predict(mod3, newdata = test.df[,c("Graduation_rate","student_faculty_ratio_new")])
 MSE3 <- mean((preds.mod3 - test.df$alumni_giving_rate)^2)
 RMSE3 <- sqrt(MSE3)
 print(RMSE3)
#RMSE3=5.713
 
 #Model4
 alumni_reg$Graduation_rate_new=(alumni_reg$Graduation_rate)^2
 n = nrow(alumni_reg)
 p = ncol(alumni_reg)
 set.seed(123)
 train.index <- sample(row.names(alumni_reg), floor(0.8*n))
 test.index <- setdiff(row.names(alumni_reg), train.index)
 train.df <- alumni_reg[train.index,]
 test.df <- alumni_reg[test.index,]
 mod4<-lm(alumni_giving_rate~Graduation_rate_new+student_faculty_ratio, data=train.df)
 preds.mod4 <- predict(mod4, newdata = test.df[, -c(2)])
 MSE4 <- mean((preds.mod4 - test.df$alumni_giving_rate)^2)
 RMSE4 <- sqrt(MSE4)
 print(RMSE4) 
 ##RMSE4=5.409
 
 #Applying further transformations
 alumni_reg$student_faculty_ratio_new=(alumni_reg$student_faculty_ratio)^2 
 alumni_reg$Graduation_rate_new=(alumni_reg$Graduation_rate)^2
 
 #Model 5
 n = nrow(alumni_reg)
 p = ncol(alumni_reg)
 set.seed(123)
 train.index <- sample(row.names(alumni_reg), floor(0.8*n))
 test.index <- setdiff(row.names(alumni_reg), train.index)
 train.df <- alumni_reg[train.index,]
 test.df <- alumni_reg[test.index,]
 mod5<-lm(alumni_giving_rate~ Graduation_rate+ student_faculty_ratio+student_faculty_ratio_new,      
          data=train.df)
 preds.mod5 <- predict(mod5, newdata = test.df[,c("Graduation_rate", 
                                                  "student_faculty_ratio","student_faculty_ratio_new")])
 MSE5 <- mean((preds.mod5 - test.df$alumni_giving_rate)^2)
 RMSE5 <- sqrt(MSE5)
 print(RMSE5) 
 #RMSE5=6.16
 
 #Model 6
 n = nrow(alumni_reg)
 p = ncol(alumni_reg)
 set.seed(123)
 train.index <- sample(row.names(alumni_reg), floor(0.8*n))
 test.index <- setdiff(row.names(alumni_reg), train.index)
 train.df <- alumni_reg[train.index,]
 test.df <- alumni_reg[test.index,]
 mod6<-lm(alumni_giving_rate~ Graduation_rate+student_faculty_ratio+Graduation_rate_new,data=train.df)
 preds.mod6 <- predict(mod6, newdata = test.df[,c("Graduation_rate", 
                                                  "student_faculty_ratio","Graduation_rate_new")])
 MSE6 <- mean((preds.mod6 - test.df$alumni_giving_rate)^2)
 RMSE6 <- sqrt(MSE6)
 print(RMSE6) 
 #RMSE6=5.228
 
 print(c(MSE1, MSE2, MSE3, MSE4, MSE5, MSE6))
 print(c(RMSE1, RMSE2, RMSE3, RMSE4, RMSE5, RMSE6))
 #Fixing final model as Model 6 due to the least RMSE value.
 
 #Fitting the model 6 using the combined training and test dataset
 full.df = rbind(train.df, test.df)
 mod.final <- lm(alumni_giving_rate~Graduation_rate_new +Graduation_rate+ student_faculty_ratio, data = full.df)
 summary(mod.final)
 
 #Generating regression equation
 alumni_giving_rate_cap = 294.331 + 0.047* Graduation_rate^2 – 6.920 * Graduation_rate - 1.348 *  student_faculty_ratio
 
