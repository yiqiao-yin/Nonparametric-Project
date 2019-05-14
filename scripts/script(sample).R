################## NONPARAMETRIC STATSTICS ##################

##nonparametric statistical methods using R (chapter 3-5)
##Chapter3: two sample problem
##Chapter4: compute correlation & corresponding test, pearson, spearman, kendalls tau
##Chpater5: Anova test (one-way & two-way)

################## CHAPTER 3 ##################

library(npsm)
library(datasets)

#esophageal cancer
esoph

#create a vector standing for case group
x <- rep(esoph$alcgp, esoph$ncases)
y <- rep(esoph$alcgp, esoph$ncontrols)
z <- c(x,y)
w <- c(rep(1, length(x)), rep(0, length(y)))
table(z,w)
barplot(table(z,w))

x <- as.numeric(x)
y <- as.numeric(y)
wilcox.test(x,y)
#default is two sides

#test H0: x=y, H1:x>y
wilcox.test(x,y, alternative="greater")

#test H0: x=y, H1:x<y
wilcox.test(x,y, alternative="less")

#remark: wilcox.test one sample if you have pairwaise data, Wilcoxon sined rant test
#remark: wilcox.test two sample it performes Mann-whitney test
?wilcox.test
?rank.test

#rank.test: normal approximation of wilcoxon test
rank.test(x,y)

rank.test(x,y, scores=nscores)
#nscoresL you use normal score, phi^-1(rank/(N+1))
?rfit
?scores

#estimation of location parameter
# x~f(.), y~f(.~delta)
# estimation for delta
# call rfit
# rfit: robust fit
# in package Rfit
?Rfit

?rfit
quail2
head(quail2)

fit <- rfit(ldl~treat, data=quail2)
lmfit <- lm(ldl~treat, data=quail2)

summary(fit)
coef(summary(fit))
coef(summary(lmfit))

plot(quail2$treat, quail2$ldl)

#prefer
fit2 <- rfit(ldl~treat, data=quail2, scores=nscores)
coef(summary(fit2))
coef(summary(fit))





################## CHAPTER 4 ##################

library(npsm)
#dataset:bb2010
#baseball consist of 122 players in season 2010
#at least 450 times at bat
head(bb2010)
tail(bb2010)
#study the correlation between ave and hr
hist(bb2010$ave)
#hr: home runs

cor(bb2010$ave, bb2010$hr)
#guest there is positive cor

cor.test(bb2010$ave, bb2010$hr)

#what if there is a outlier
mybb <- bb2010
mybb[87,]
mybb[87,2] <-320 #become outlier

#there is an outlier now
cor.test(mybb$ave,mybb$hr)

#spearman or kendall's tau is good
cor.test(mybb$ave,mybb$hr, method="spearman")
cor.test(bb2010$ave,bb2010$hr, method="spearman")

cor.test(mybb$ave,mybb$hr, method="kendall")
cor.test(bb2010$ave,bb2010$hr, method="kendall")

#bootstrap
library(boot)
cor.boot.ci(bb2010$ave,bb2010$hr, method="spearman")
cor.boot.ci(bb2010$ave,bb2010$hr)

cor.boot.ci(bb2010$ave,bb2010$hr, method="pearson")
cor.boot.ci(bb2010$ave,bb2010$hr, method="kendall")

?cor.boot.ci
cor.boot.ci(mybb$ave,mybb$hr, method="kendall")
#we prefer to use spearman/kendall tao when data has outliers
#pearson is easy for computation




################## CHAPTER 5 ##################

quail
head(quail)

table(quail$treat)
table(quail2$treat)

#oneway.fit
robfit <- oneway.rfit(quail$ldl,quail$treat)
robfit
#by default gives the results without adjustment

summary(robfit, method ="tukey")
#tukey is good when you have balanced design
table(quail$treat)

summary(robfit, method ="bonferroni")


#how about two way?
#rfit, taov

serumLH
head(serumLH)

serumLH$light.regime

serumLH$LRF.dose

#2*5=10 conditions

#two factors impact on serum level
raov(serum ~ light.regime + LRF.dose, data=serumLH)
raov(serum ~ LRF.dose + light.regime, data=serumLH)

#balanced design
#balanced design / orthongonal design

rfit(serum ~ LRF.dose + light.regime, data=serumLH)

rfit <- rfit(serum ~ LRF.dose + light.regime, data=serumLH)
summary(rfit2)

rfit2 <- rfit(serum ~ LRF.dose + light.regime + light.regime*LRF.dose, data=serumLH)
summary(rfit2)

#waht is the estimal level for group with constant lgiht and dose=1250?
63.002+147


#what is the estimal level for group with intermitent lgiht and dose=1250?
63.002 +147+ 47 +153


tail(serumLH)
serumLH$serum[55:60]
mean(serumLH$serum[55:60])
median(serumLH$serum[55:60])
  
  

