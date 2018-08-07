survival_sparrow <- read_excel("~/Desktop/davis/207/untitled folder/survival_sparrow.xls")
mydata = survival_sparrow

# data processing
mydata$STATUS[mydata$STATUS == 'Perished'] <- 0
mydata$STATUS[mydata$STATUS == 'Survived'] <- 1

# data type
sapply(mydata,class)

mydata$STATUS = as.factor(mydata$STATUS)
factor(mydata$AG)


# data summary
summary(mydata)
sapply(mydata, sd)

## 1.x variables
## ? histograms

# pie chart age
n = nrow(mydata)
lbls = c('adult', ' juvenile')
pct=round(100*table(mydata$AG)/n)
lab=paste(lbls, pct)
lab=paste(lab,'%',sep='')
lab
pie(table(mydata$AG),labels = lab,col = rainbow(2), main = 'pie chart of age')
piechart(table(mydata$AG))

# boxplot outliers
boxplot(mydata[3:11], title = 'Boxplot of Explanatory Variables')

## delete outlier
## no ouliers

# multicolinearity
## scattor plot
plot(mydata)
## corrlation matrix
cor(mydata[3:11])

## center to reduce the correlation
cor(mydata[3:11])

# data plot descriptions

# y variable description
## pie chart
lbls = c('Perished','Survived')
pct=round(100*table(mydata$STATUS)/n)
lab=paste(lbls, pct)
lab=paste(lab,'%',sep='')
lab
pie(table(mydata$STATUS),labels = lab,col = rainbow(2), main = 'pie chart of status')



##? histogram
mfrow
hist(mydata$TL)
hist(mydata$AE)
hist(mydata$WT)
hist(mydata$BH)
hist(mydata$HL)


## (2)State the model you want to use in details
## logistic model

## refinement strategy
## goodness of fit
## hypothesis test
## AIC

##

# s plot
newdata = mydata[order(mydata$TL), ]
newdata
flag = 0
m = 0
tot = 0
index = 1
mat <- matrix(ncol=2, nrow=10)
for (i in 1:80) {
  if (flag != 8) {
    tot = tot + newdata$TL[i];
    if (newdata$STATUS[i] == 0) {
      m = m+1
    }
    flag = flag + 1;
  }
  else {
    p = m/8;
    p;
    avg = tot/8;
    avg;
    mat[index,] <- c(p,avg);
    index = index + 1;
    flag = 0;
    # clear m and avg
    m = 0;
    tot = 0;
    
    # calc m and avg
    tot = tot + newdata$TL[i];
    if (newdata$STATUS[i] == 0) {
      m = m+1
      # calc m and avg
    }
    flag = flag + 1;
  }
}
p = m/8;
p;
avg = tot/8;
avg;
mat[index,] <- c(p,avg);
mat
plot(mat[,2],mat[,1])


## (3)Perform an initial study to validate your model choice.
# fit model
mylogit <- glm(STATUS~., data = mydata, family = binomial)
summary(mylogit)

##  linear model resonable test(goodness of fit)
qchisq(0.95,69)
### chi > resid dev, so it's reasonable

# model plots (fitted vs residuals/qqnorm/outliers)
plot(mylogit)
## fitted vs residuals
library('ggplot2')
qplot(y = mylogit$residuals, x = mylogit$fitted.values) +
  stat_smooth(method = "loess", span = 0.1)

# half normal plot
library(faraway)
halfnorm(mylogit$residuals)

# vif
library('car')
vif(mylogit)

# model hypothesis
confint.default(mylogit)

# Hypothesis test
# test if FL,TT can be dropped
# reduced model:delete FL,TT
redmylog = glm(STATUS~.-FL-TT, data = mydata, family = binomial)
summary(redmylog)

Gs2 = redmylog$deviance - mylogit$deviance; Gs2

## H0: βfl = βtt = 0;
## H1: not both βfl,βtt = 0;
qchisq(0.95,2) 
## we can reject h0 if Gs2 > chi square
## since Gs2 < 5.991465, we cannot reject H0 at 0.05 significant level.
### we can drop FL, HL

# test if AG can be dropped 
## H0: βag = 0;
## H1: βag ≠ 0;
## Z* = 0.154
Z = qnorm(0.975)
## we can reject H0 if Z* > Z
## Since Z* < Z, we cannot reject H0 at 0.05 significant level.
## P-value = 0.877529
### we can drop AG

# the model after AG is dropped
redmylog2 = glm(STATUS~.-FL-TT-AG, data = mydata, family = binomial)
summary(redmylog2)

# test if AE can be dropped 
## H0: βae = 0;
## H1: βae ≠ 0;
## Z* = 0.764
Z = qnorm(0.975)
## we can reject H0 if Z* > Z
## Since Z* < Z, we cannot reject H0 at 0.05 significant level.
## P-value = 0.44482 
### we can drop AE

# the model after AE is dropped
redmylog3 = glm(STATUS~.-FL-TT-AG-AE, data = mydata, family = binomial)
summary(redmylog3)

# test if SK can be dropped 
## H0: βsk = 0;
## H1: βsl ≠ 0;
## Z* = 0.964
Z = qnorm(0.975)
## we can reject H0 if Z* > Z
## Since Z* < Z, we cannot reject H0 at 0.05 significant level.
## P-value = 0.3351
### we can drop SK

# the model after SK is dropped
redmylog4 = glm(STATUS~.-FL-TT-AG-AE-SK, data = mydata, family = binomial)
summary(redmylog4)
vif(redmylog4)
redmylog5 = glm(STATUS~.-FL-TT-AG-AE-SK-BH, data = mydata, family = binomial)
summary(redmylog5)
vif(redmylog5)
step(mylogit)
plot(mylogit)
firstmymodel = glm(formula = STATUS ~ TL + WT + HL + KL, family = binomial, 
    data = mydata)

c = data.frame(mydata$STATUS,mydata$TL,mydata$WT,mydata$HL,mydata$KL,mydata$BH)
TL2 = (mydata$TL)^2
WT2 = (mydata$WT)^2
HL2 = (mydata$HL)^2
KL2 = (mydata$KL)^2
BH2 = (mydata$BH)^2
TLWT = (mydata$TL)*(mydata$WT)
TLHL = (mydata$TL)*(mydata$HL)
TLKL = (mydata$TL)*(mydata$KL)
WTHL = (mydata$WT)*(mydata$HL)
WTKL = (mydata$WT)*(mydata$KL)
HLKL = (mydata$HL)*(mydata$KL)
BHTL = (mydata$TL)*(mydata$BH)
BHWT = (mydata$BH)*(mydata$WT)
BHHL = (mydata$BH)*(mydata$HL)
BHKL = (mydata$BH)*(mydata$KL)
secondmymodel =  glm(formula = STATUS~TL+WT+HL+KL+BH+TLWT+TLHL+TLKL+WTHL+WTKL+HLKL+BHTL+BHWT+BHHL+BHKL+
                       TL2+WT2+HL2+KL2+BH2, family = binomial, 
                     data = mydata)
summary(secondmymodel)

# set this model as full model
model.full = secondmymodel
model.full
# step AIC forward
model.null = glm(STATUS ~ 1, 
                 data=mydata,
                 family = binomial(link="logit")
)
summary(model.null)

step(model.null,
     scope = list(upper=model.full),
     direction="both",
     data = mydata)
best.2 = step(model.null, scope = list(lower = model.null, upper = model.full), direction = "both")
summary(best.2)

# final model
final = glm(formula = STATUS ~ TL + HL + WT + KL, family = binomial(link = "logit"), 
            data = mydata)
summary(final)
plot(final)
qplot(y = final$residuals, x = final$fitted.values) +
  stat_smooth(method = "loess", span = 0.1)

# diagnostic
library(LogisticDx)
ss = dx(best.2)
sd.pear.res = ss$sPr #standardized pearson residual
hist(sd.pear.res, main = "Pearson standardized residuals")
cutoff.sd.pear = 3
sd.pear.res[sd.pear.res>3]
sd.pear.res


