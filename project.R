#parse in data for analysis
nyt4 <- read.csv("nyt4.csv", header=TRUE)
nyt8 <- read.csv("nyt8.csv", header=TRUE)
nyt15 <- read.csv("nyt15.csv", header=TRUE)
nyt25 <- read.csv("nyt25.csv", header=TRUE)
nyt30 <- read.csv("nyt30.csv", header=TRUE)

#create boxplot name
bxptNames = c("nyt4","nyt8","nyt15","nyt25","nyt30")
#create boxplot for part (a)
male <- c(sum(nyt4$Gender == 1), sum(nyt8$Gender == 1), sum(nyt15$Gender == 1), sum(nyt25$Gender == 1), sum(nyt30$Gender == 1))
female <- c(sum(nyt4$Gender == 0), sum(nyt8$Gender == 0), sum(nyt15$Gender == 0), sum(nyt25$Gender == 0), sum(nyt30$Gender == 0))
boxplot(male,female,names = c("Female-nyt(4,8,15,25,30)","Male-nyt(4,8,15,25,30)"))
boxplot(nyt4$Impressions,nyt8$Impressions,nyt15$Impressions,nyt25$Impressions,nyt30$Impressions,names=bxptNames)

#create histo for part (b)
hist(nyt4$Gender,breaks=2)
hist(nyt8$Gender,breaks=2)
hist(nyt15$Gender,breaks=2)
hist(nyt25$Gender,breaks=2)
hist(nyt30$Gender,breaks=2)
hist(nyt4$Impressions,breaks=15)
hist(nyt8$Impressions,breaks=15)
hist(nyt15$Impressions,breaks=15)
hist(nyt25$Impressions,breaks=15)
hist(nyt30$Impressions,breaks=15)

#create ECDF for part (c)
plot(ecdf(nyt4$Gender),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt8$Gender),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt15$Gender),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt25$Gender),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt30$Gender),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt4$Impressions),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt8$Impressions),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt15$Impressions),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt25$Impressions),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt30$Impressions),do.points=FALSE,verticals=TRUE)
#qqnorm(nyt4$Gender); qqline(nyt4$Gender)
#qqnorm(nyt8$Gender); qqline(nyt8$Gender)
#qqnorm(nyt15$Gender); qqline(nyt15$Gender)
#qqnorm(nyt25$Gender); qqline(nyt25$Gender)
qqnorm(nyt30$Gender); qqline(nyt30$Gender)
#qqnorm(nyt4$Impressions); qqline(nyt4$Impressions)
#qqnorm(nyt8$Impressions); qqline(nyt8$Impressions)
#qqnorm(nyt15$Impressions); qqline(nyt15$Impressions)
#qqnorm(nyt25$Impressions); qqline(nyt25$Impressions)
qqnorm(nyt30$Impressions); qqline(nyt30$Impressions)

#Significance test
cor.test(nyt4$Gender, nyt4$Impressions)
cor.test(nyt8$Gender, nyt8$Impressions)
cor.test(nyt15$Gender, nyt15$Impressions)
cor.test(nyt25$Gender, nyt25$Impressions)
cor.test(nyt30$Gender, nyt30$Impressions)

#6600-level
#unborn children can read nyt, filter out age zeros
nyt8 <- nyt8[nyt8$Age != 0,]
nyt30 <- nyt30[nyt30$Age != 0,]

hist(nyt8$Age,breaks=10)
hist(nyt8$Age,breaks=40)
hist(nyt30$Age,breaks=20)

plot(ecdf(nyt8$Age),do.points=FALSE,verticals=TRUE)
plot(ecdf(nyt30$Age),do.points=FALSE,verticals=TRUE)
qqnorm(nyt8$Age); qqline(nyt8$Age)
qqnorm(nyt30$Age); qqline(nyt30$Age)

cor.test(nyt8$Age, nyt8$Impressions)
cor.test(nyt30$Age, nyt30$Impressions)
