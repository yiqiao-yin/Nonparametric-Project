#STAT 4222/5222
#Nonparametric Statistics
#Parametric & Nonparametric Statistical Methods in Classification using Categorical Data
#Group 2

library(dplyr)
library(mltools)
setwd("/Users/tong/Desktop/NP/Project")
adult <- read.csv("/Users/tong/Desktop/NP/Project/adult.csv")

###########clean data#############
data1 <- select(adult, income, age, educational.num,workclass, race,gender,hours.per.week, native.country, occupation)
data1 = data1[-which(data1$workclass=="?"),]
data1 = data1[-which(data1$occupation=="?"),]

#income: <=50K=0, >50K=1
data1$income <- as.factor(data1$income)
data1$income <- as.numeric(data1$income)
data1$income <- data1$income-1

#age: category it by 10 year period
data1$age[which(data1$age >=90)] <- 9
data1$age[which(data1$age >=80)] <- 8
data1$age[which(data1$age >=70)] <- 7
data1$age[which(data1$age >=60)] <- 6
data1$age[which(data1$age >=50)] <- 5
data1$age[which(data1$age >=40)] <- 4
data1$age[which(data1$age >=30)] <- 3
data1$age[which(data1$age >=20)] <- 2
data1$age[which(data1$age >=10)] <- 1 

#edu num
#hours per week
data1$hours.per.week[which(data1$hours.per.week <10)] <- 0
data1$hours.per.week[which(data1$hours.per.week >=90)] <- 9
data1$hours.per.week[which(data1$hours.per.week >=80)] <- 8
data1$hours.per.week[which(data1$hours.per.week >=70)] <- 7
data1$hours.per.week[which(data1$hours.per.week >=60)] <- 6
data1$hours.per.week[which(data1$hours.per.week >=50)] <- 5
data1$hours.per.week[which(data1$hours.per.week >=40)] <- 4
data1$hours.per.week[which(data1$hours.per.week >=30)] <- 3
data1$hours.per.week[which(data1$hours.per.week >=20)] <- 2
data1$hours.per.week[which(data1$hours.per.week >=10)] <- 1 

#workclass:
data1$workclass <- as.numeric(data1$workclass)

#race: white=0, black=1, other=2
data1$race <- as.numeric(data1$race)
data1$race[-c(which(data1$race == 3), which(data1$race == 5))] <- 2
data1$race[which(data1$race ==3)] <- 1 
data1$race[which(data1$race ==5)] <- 0


#gender: male=1, female=0
data1$gender <- as.numeric(data1$gender)
data1$gender <- data1$gender-1

#native country: US=0, other=1
data1$native.country <- as.numeric(data1$native.country)
data1$native.country[which(data1$native.country ==40)] <- 0
data1$native.country[which(data1$native.country !=0)] <- 1

#occupation
data1$occupation <- as.numeric(data1$occupation)

###########scatter plot#############
# pairs(data1)
# library("PerformanceAnalytics")
# chart.Correlation(data1, histogram=TRUE, pch=19)


###########Method Comparison##############
#1#KNN
library(DMwR)
AR<-NULL
all <- 1:46033
for (i in 1:5){
  temp=sample(all, 9206)
  all<- all[-temp]
  test <- as.data.frame(data1[temp,])
  train <- as.data.frame(data1[-temp,])
  knn <- kNN(income ~ .,train,test,norm=FALSE,k=2)
  AR <- c(AR,  ( sum(knn==test$income)/nrow(test) )  )
}
mean(AR) #0.7908538


#2#CART
library(rpart)
AR<-NULL
all <- 1:46033
for (i in 1:5){
  temp=sample(all, 9206)
  all<- all[-temp]
  test <- as.data.frame(data1[temp,])
  train <- as.data.frame(data1[-temp,])
  fit <- rpart(income ~., method="class", data=train)
  cart <- predict(fit, test, type = c("class"))
  AR <- c(AR,  ( sum(cart==test$income)/nrow(test)   )  )
}
mean(AR) #0.7957636

#3#SVM
library(e1071)
AR<-NULL
all <- 1:46033
for (i in 1:5){
  temp=sample(all, 9206)
  all<- all[-temp]
  test <- as.data.frame(data1[temp,])
  train <- as.data.frame(data1[-temp,])
  fit <- svm(income ~., data=train, type = 'C-classification')
  svm <- predict(fit, test)
  AR <- c(AR,  ( sum(svm==test$income)/nrow(test)   )  )
}
mean(AR) #0.8031718


#4#Random Forest
library(randomForest)
AR<-NULL
all <- 1:46033
for (i in 1:5){
  temp=sample(all, 9206)
  all<- all[-temp]
  test <- as.data.frame(data1[temp,])
  train <- as.data.frame(data1[-temp,])
  fit <- randomForest(income ~ ., data=train)
  rf <- predict(fit, test[-1])
  rf[which(rf>0.5)] <-1
  rf[which(rf<=0.5)] <-0
  AR <- c(AR,  ( sum(rf==test$income)/nrow(test)   )  )
}
mean(AR) #0.8107756


#################Parametric Method####################
#1#Logistic Regression
AR<-NULL
all <- 1:46033
for (i in 1:5){
  temp=sample(all, 9206)
  all<- all[-temp]
  test <- as.data.frame(data1[temp,])
  train <- as.data.frame(data1[-temp,])
  fit<- glm(income ~., data = train, family = "binomial")
  lg <- predict(fit, test[-1])
  lg[which(lg>0.5)] <-1
  lg[which(lg<=0.5)] <-0
  AR <- c(AR,  ( sum(lg==test$income)/nrow(test)   )  )
}
mean(AR) #0.7879426


#2#LASSO
library(glmnet)
AR<-NULL
all <- 1:46033
for (i in 1:5){
  temp=sample(all, 9206)
  all<- all[-temp]
  test <- as.data.frame(data1[temp,])
  train <- as.data.frame(data1[-temp,])
  fit<- glmnet(as.matrix(train[,-1]),as.matrix(train[,1]), family = "binomial")
  lasso <- predict(fit,as.matrix(test[,-1]),s=0.001,type="response")
  lasso[which(lasso>0.5)] <-1
  lasso[which(lasso<=0.5)] <-0
  AR <- c(AR,  ( sum(lasso==test$income)/nrow(test)   )  )
}
mean(AR) #0.7951336


#3#Naive Bayes
AR<-NULL
all <- 1:46033
for (i in 1:5){
  temp=sample(all, 9206)
  all<- all[-temp]
  test <- as.data.frame(data1[temp,])
  train <- as.data.frame(data1[-temp,])
  fit <- naiveBayes(as.factor(income) ~ ., data = train)
  nb <- predict(fit, test[,-1])
  AR <- c(AR,  ( sum(nb==test$income)/nrow(test)   )  )
}
mean(AR) #0.7866391


#4#Neural Network
library(nnet)
AR<-NULL
all <- 1:46033
for (i in 1:5){
  temp=sample(all, 9206)
  all<- all[-temp]
  test <- as.data.frame(data1[temp,])
  train <- as.data.frame(data1[-temp,])
  fit = nnet(train[,-1],class.ind(train$income), size=2,softmax=TRUE)
  nn <- predict(fit, test[,-1], type="class")
  AR <- c(AR,  ( sum(nn==test$income)/nrow(test)   )  )
}
mean(AR) #0.7912666

#####linear regression
AR<-NULL
all <- 1:46033
for (i in 1:5){
  temp=sample(all, 9206)
  all<- all[-temp]
  test <- as.data.frame(data1[temp,])
  train <- as.data.frame(data1[-temp,])
  fit<- lm(income ~., data = train)
  lm <- predict(fit, test[-1])
  lm[which(lm>0.5)] <-1
  lm[which(lm<=0.5)] <-0
  AR <- c(AR,  ( sum(lm==test$income)/nrow(test)   )  )
}
mean(AR) #0.7913317

##############Two Sample Test#################
agex <- data1[data1$income == 0,]$age
agey <- data1[data1$income == 1,]$age

edux <- data1[data1$income == 0,]$educational.num
eduy <- data1[data1$income == 1,]$educational.num

wcx <- data1[data1$income == 0,]$workclass
wcy <- data1[data1$income == 1,]$workclass

racex <- data1[data1$income == 0,]$race
racey <- data1[data1$income == 1,]$race

genderx <- data1[data1$income == 0,]$gender
gendery <- data1[data1$income == 1,]$gender

hourx <- data1[data1$income == 0,]$hours.per.week
houry <- data1[data1$income == 1,]$hours.per.week

natx <- data1[data1$income == 0,]$native.country
naty <- data1[data1$income == 1,]$native.country

ocpx <- data1[data1$income == 0,]$occupation
ocpy <- data1[data1$income == 1,]$occupation

# 2-sample t-test (parametric)
t.test(agex, agey, alternative = "less")
t.test(edux, eduy, alternative = "less")
t.test(hourx, houry, alternative = "less")
t.test(genderx, gendery, alternative = "less")
t.test(racex, racey, alternative = "greater")  
t.test(natx, naty, alternative = "greater")
t.test(wcx, wcy, alternative = "less") #no affect
t.test(ocpx, ocpy, alternative = "less")

#Wilcoxon test
wilcox.test(agex, agey, alternative = "less")
wilcox.test(edux, eduy, alternative = "less")
wilcox.test(hourx, houry, alternative = "less")
wilcox.test(genderx, gendery, alternative = "less")
wilcox.test(racex, racey, alternative = "greater")
wilcox.test(natx, naty, alternative = "greater")
wilcox.test(wcx, wcy, alternative = "less")
wilcox.test(ocpx, ocpy, alternative = "less")


#Kruskal-Wallis Test
kruskal.test(income ~ age, data = data1) 
kruskal.test(income ~ educational.num, data = data1) 
kruskal.test(income ~ workclass, data = data1) 
kruskal.test(income ~ gender, data = data1) 
kruskal.test(income ~ race, data = data1) 
kruskal.test(income ~ hours.per.week, data = data1) 
kruskal.test(income ~ native.country, data = data1) 
kruskal.test(income ~ occupation, data = data1) 


###########Correlation Matrix############# 
library(Hmisc)
library(corrplot)

data2 <- select(data1, income, age, educational.num,race,gender,hours.per.week, native.country)
res <- cor(data2, method = c("pearson"))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
p.mat <- rcorr(as.matrix(data2), type = c("pearson"))
p.mat$P[1,1] <- 0
p.mat$P[2,2] <- 0
p.mat$P[3,3] <- 0
p.mat$P[4,4] <- 0
p.mat$P[5,5] <- 0
p.mat$P[6,6] <- 0
p.mat$P[7,7] <- 0
corrplot(res, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat$P, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)


col<- colorRampPalette(c("blue", "white", "red"))(20)

#pearson
rcorr(as.matrix(data2), type = c("pearson"))
res <- cor(data1, method = c("pearson"))
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
heatmap(x = res, col = col, symm = TRUE)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(data1)
corrplot(res, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)


#spearman
heatmap(x = res, col = col, symm = TRUE)

#kendall
#rcorr(as.matrix(wage), type = c("kendall"))
res <- cor(data1, method = c("kendall"))
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
heatmap(x = res, col = col, symm = TRUE)

#Contingency Table
chisq.test(table(data1$income,data1$age) )
chisq.test(table(data1$income,data1$educational.num) )
chisq.test(table(data1$income,data1$race) )
chisq.test(table(data1$income,data1$gender) )
chisq.test(table(data1$income,data1$workclass) )
chisq.test(table(data1$income,data1$hours.per.week) )

chisq.test(table(data1$age,data1$educational.num) )
chisq.test(table(data1$age,data1$race) )
chisq.test(table(data1$age,data1$gender) )
chisq.test(table(data1$age,data1$workclass) )
chisq.test(table(data1$age,data1$hours.per.week) )

chisq.test(table(data1$educational.num,data1$race) )
chisq.test(table(data1$educational.num,data1$gender) )
chisq.test(table(data1$educational.num,data1$workclass) )
chisq.test(table(data1$educational.num,data1$hours.per.week) )

chisq.test(table(data1$race,data1$gender) )
chisq.test(table(data1$race,data1$workclass) )
chisq.test(table(data1$race,data1$hours.per.week) )

chisq.test(table(data1$gender,data1$workclass) )
chisq.test(table(data1$gender,data1$hours.per.week) )

chisq.test(table(data1$workclass,data1$hours.per.week) )



###########Anova##############
summary(aov(data1$income~data1$age*data1$educational.num*data1$hours.per.week*data1$gender*data1$race*data1$native.country*data1$workclass*data1$occupation))

#excluding native country/race
#adding data1$age:data1$educational.num   


#########2x2 Contingency Table############
boxplot(data1$income~data1$gender)
boxplot(data1$income~data1$race)

histogram(data1$income~data1$gender,col="black",breaks=c(-0.2, 0.2, 0.8, 1.2))
histogram(data1$income~data1$race,col="black",breaks=c(-0.2, 0.2, 0.8, 1.2, 1.8, 2.2))


#        income
#male

##Male  with same edu have higher salary
median(data1$educational.num) #10
low_edu <- data1[which(data1$educational.num<5),]
med_edu <- data1[which(data1$educational.num>=5),]
med_edu <- data1[which(med_edu$educational.num<10),]
high_edu <- data1[which(data1$educational.num>=10),]

low_edu_male <- data1[which(low_edu$gender==1),]
low_edu_wag <- data1[which(low_edu$income==1),]
low_edu_wag_male <- data1[which(low_edu_wag$gender==1),]

sex_wage_low_edu <- rbind(
  c(nrow(low_edu_wag_male ) ,  (nrow(low_edu_male)- nrow(low_edu_wag_male)  )   ),
  c((nrow(low_edu_wag) -nrow(low_edu_wag_male)  ),       (nrow(low_edu)-nrow(low_edu_wag_male )- (nrow(low_edu_male)- nrow(low_edu_wag_male)  )  - (nrow(low_edu_wag) -nrow(low_edu_wag_male)  )))
)
chisq.test(sex_wage_low_edu) # => fail rej =>  #male no more wage on low exp
fisher.test(sex_wage_low_edu)

low_edu_male <- data1[which(low_edu$gender==1),]
low_edu_wag <- data1[which(low_edu$income==1),]
low_edu_wag_male <- data1[which(low_edu_wag$gender==1),]


# med_exp_male <- wage[which(med_exp$SEX==0),]
# med_exp_wag <- wage[which(med_exp$WAGE.HR>=10),]
# med_exp_wag_male <- wage[which(med_exp_wag$SEX==0),]
# 
# sex_wage_med_exp <- rbind(
#   c(nrow(med_exp_wag_male ) ,  (nrow(med_exp_male)- nrow(med_exp_wag_male)  )   ),
#   c((nrow(med_exp_wag) -nrow(med_exp_wag_male)  ),       (nrow(med_exp)-nrow(med_exp_wag_male )- (nrow(med_exp_male)- nrow(med_exp_wag_male)  )  - (nrow(med_exp_wag) -nrow(med_exp_wag_male)  )))
# )
# chisq.test(sex_wage_med_exp) # => fail rej =>  #male have no more wage on med exp
# fisher.test(sex_wage_med_exp)


# high_exp_male <- wage[which(high_exp$SEX==0),]
# high_exp_wag <- wage[which(high_exp$WAGE.HR>=10),]
# high_exp_wag_male <- wage[which(high_exp_wag$SEX==0),]
# 
# sex_wage_high_exp <- rbind(
#   c(nrow(high_exp_wag_male ) ,  (nrow(high_exp_male)- nrow(high_exp_wag_male)  )   ),
#   c((nrow(high_exp_wag) -nrow(high_exp_wag_male)  ),       (nrow(high_exp)-nrow(high_exp_wag_male )- (nrow(high_exp_male)- nrow(high_exp_wag_male)  )  - (nrow(high_exp_wag) -nrow(high_exp_wag_male)  )))
# )
# chisq.test(sex_wage_high_exp) # => rej =>  #male have more wage on high exp
# fisher.test(sex_wage_high_exp)

edu <- data1$educational.num
for (i in 1:46033){
  if (edu[i] <5) {
    edu[i]=-1
  } else if (edu[i] >=10){
    edu[i]=1
  } else {
    edu[i]=0
  }
}


data1 <- cbind(data1, as.factor(edu))
mytable<-xtabs(~income+as.factor(edu)+gender,data=data1)
mantelhaen.test(mytable)
#p-value = 1.277e-06 =>  Male  with same exp have higher salary


#excluding native workclass
data3 <- select(adult, income, age, educational.num, race,gender,hours.per.week, occupation, native.country)

library(randomForest)
AR<-NULL
all <- 1:46033
for (i in 1:5){
  temp=sample(all, 9206)
  all<- all[-temp]
  test <- as.data.frame(data3[temp,])
  train <- as.data.frame(data3[-temp,])
  fit <- randomForest(income ~ ., data=train)
  rf <- predict(fit, test[-1])
  rf[which(rf>0.5)] <-1
  rf[which(rf<=0.5)] <-0
  AR <- c(AR,  ( sum(rf==test$income)/nrow(test)   )  )
}
mean(AR) #0.8178579







#excluding native country & workclass
data3 <- select(adult, income, age, educational.num, race,gender,hours.per.week, occupation)

library(randomForest)
AR<-NULL
all <- 1:46033
for (i in 1:5){
  temp=sample(all, 9206)
  all<- all[-temp]
  test <- as.data.frame(data3[temp,])
  train <- as.data.frame(data3[-temp,])
  fit <- randomForest(income ~ ., data=train)
  rf <- predict(fit, test[-1])
  rf[which(rf>0.5)] <-1
  rf[which(rf<=0.5)] <-0
  AR <- c(AR,  ( sum(rf==test$income)/nrow(test)   )  )
}
mean(AR) #0.8175538



#Excluding native country and adding age*edu, age*hour, edu*our, age*gender, hour*gender

data3 <- select(data1, income, age, educational.num, race,gender,hours.per.week, workclass, occupation)
data3 <- as.data.frame(cbind(data3, data3$age*data3$educational.num, data3$age*data3$hours.per.week, data3$educational.num*data3$hours.per.week, data3$age*data3$gender, data3$gender*data3$hours.per.week))
colnames(data3) <-c("v1","v2","v3","v4","v5","v6","v7","v8","v9","v10","v11","v12","v13")
AR<-NULL
all <- 1:46033
for (i in 1:5){
  temp=sample(all, 9206)
  all<- all[-temp]
  test <- as.data.frame(data3[temp,])
  train <- as.data.frame(data3[-temp,])
  fit <- randomForest(v1 ~., data=train)
  rf <- predict(fit, test[-1])
  AR <- c(AR,  ( sum(rf==test$v1)/nrow(test)   )  )
}
mean(AR) #0.808453





