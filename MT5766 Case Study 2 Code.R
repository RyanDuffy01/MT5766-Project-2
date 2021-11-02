#reads in sandstone data
sandstonedata <- read.csv("sandstone parameters.csv",header=T)

#plots all variables against each other
plot(sandstonedata)

#gives summary statistics for each variable
summary(sandstonedata)

#gives standard deviataion for each variable
sd(sandstonedata$perm)
sd(sandstonedata$por)
sd(sandstonedata$cond)
sd(sandstonedata$form)
sd(sandstonedata$polar)

#gives histograms for each variable
par(mfrow=c(1,5))
hist(sandstonedata$perm)
hist(sandstonedata$por)
hist(sandstonedata$cond)
hist(sandstonedata$form)
hist(sandstonedata$polar)

#fits GLMs for different combinations of variables
mixmodel1 <- glm(perm~por+cond+form+polar,family=Gamma,data=sandstonedata)
mixmodel2 <- glm(perm~por+cond,family=Gamma,data=sandstonedata,start = c(1,-1,1))
mixmodel3 <- glm(perm~por+cond+polar,family=Gamma,data=sandstonedata,start = c(1,-1,1,1))
mixmodel4 <- glm(perm~por+cond+form,family=Gamma,data=sandstonedata,start = c(1,-1,1,1))

#shows some diagnostic plots of models
par(mfrow=c(1,4))
plot(mixmodel1)
plot(mixmodel2)
plot(mixmodel3)
plot(mixmodel4)

#gives AIC of different models
AIC(mixmodel1)
AIC(mixmodel2)
AIC(mixmodel3)
AIC(mixmodel4)

#assigns variables to be loglikelihood of each model
llmodel1 <-logLik(mixmodel1) 
llmodel2 <-logLik(mixmodel2) 
llmodel3 <-logLik(mixmodel3) 
llmodel4 <-logLik(mixmodel4) 

 
modelcomparison1v2 <- 1-pchisq(2*(llmodel1-llmodel2),df=2)
modelcomparison1v3 <- 1-pchisq(2*(llmodel1-llmodel3),df=1)
modelcomparison1v4 <- 1-pchisq(2*(llmodel1-llmodel4),df=1)
modelcomparison3v2 <- 1-pchisq(2*(llmodel3-llmodel2),df=1)
modelcomparison4v2 <- 1-pchisq(2*(llmodel4-llmodel2),df=1)

#gives p value of model comparison tests
modelcomparisons <- data.frame(
  "Model 1" = c("N/A",modelcomparison1v2,modelcomparison1v3,modelcomparison1v4),
  "Model 2" = c("N/A","N/A","N/A","N/A"),
  "Model 3" = c("N/A",modelcomparison3v2,"N/A","N/A"),
  "Model 4"= c("N/A",modelcomparison4v2,"N/A","N/A")
    
)
rownames(modelcomparisons)<- c("Model 1","Model 2","Model 3","Model 4")

#Read as row 2 column 1 gives p value of comparison where model 1 
#is the larger model and model 2 is the smaller model

modelcomparisons

