

# Sheather Ch 6 #5

# Evaluate skills of pro golfers increasing winnings

# Data
golf<-read.csv("http://gattonweb.uky.edu/sheather/book/docs/datasets/pgatour2006.csv",
                    header=TRUE)

# EDA

# 5-number summary with mean
summary(golf)

# an Australian statistician thinks a log-transformation of the response is better
# compare
golf.out1<-lm(PrizeMoney ~ AveDrivingDistance + DrivingAccuracy + GIR + PuttingAverage
                           + BirdieConversion + SandSaves + Scrambling + BounceBack + PuttsPerRound,
              data=golf)
golf.out2<-lm(log(PrizeMoney) ~ AveDrivingDistance + DrivingAccuracy + GIR + PuttingAverage
                                + BirdieConversion + SandSaves + Scrambling + BounceBack + PuttsPerRound,
              data=golf)

# compare either the distribution of R-studentized residuals or component + residual plots

# R-studentized residuals
golf.R1<-rstudent(golf.out1)
golf.R2<-rstudent(golf.out2)
# compare distribution
par(mfrow=c(1,2))
plot(density(golf.R1))
# skewed, extreme outliers
plot(density(golf.R2))
# nearly symmetric, normal-looking, no obvious outliers
par(mfrow=c(1,1))

# component + residual plots (could also use added variable plots)
library(car)
crPlots(golf.out1)
# every plot shows four outliers
crPlots(golf.out2)
# improvement ... no outliers, constant variance, linear effects


# regression diagnostics for other data issues

# normality
shapiro.test(golf.R2)
# p-value=0.06525 (normal)

# outliers?
subset(golf,abs(golf.R2)>3)
# row 185: Tom Lehman
# don't see any reason to remove him (googling showed 2006 wasn't an unusual year for him)

# influential obs?
golf.leverage<-lm.influence(golf.out2)$hat
subset(golf,golf.leverage>2*10/196)
# 4 golfers

golf.cd<-cooks.distance(golf.out2)
subset(golf,golf.cd>4/(196-10))
# 10 golfers

# rows of suspicious cases
infl.ind1<-c(28,40,168,178,40,47,60,63,101,125,128,168,180,185)

# create plots to identify good or bad influential
par(mfrow=c(3,3))
for(i in 1:9){

    plot(log(golf$PrizeMoney)~golf[,i+3])
    points(log(golf$PrizeMoney[infl.ind1])~golf[infl.ind1,i+3],
           pch=19,col="red")

}
par(mfrow=c(1,1))
# understandable why these are flagged as influential since many are at extremes 
# of an explanatory variable but don't see any that are bad influential

# while several observations needed to be investigated, no cause for filtering
# (but I'd understand if people more familiar with golf saw something in a
#  few of these cases and removed a few)

# investigate collinearity
round(cor(golf[,4:12]),2)
# possible collinearity: PuttingAverage X (BirdieConversions, PuttsPerRound)

plot(~AveDrivingDistance + DrivingAccuracy + GIR + PuttingAverage + BirdieConversion
      + SandSaves + Scrambling + BounceBack + PuttsPerRound,
     data=golf)
# possible collinearity: AveDrivingDistance X DrivingAccuracy
#                        DrivingAccuracy X GIR
#                        PuttingAverage X (BirdieConversions, PuttsPerRound)
# there are areas where data is sparse, so expect future obs follow what 
# was seen in the past

# do coefficients have the expected sign?
# expected direction of the effect
cor(golf$PrizeMoney,golf[4:12])
# wrong sign: AveDrivingDistance, DrivingAccuracy, PuttingAverage
# also, surprised that more of these aren't significant
summary(golf.out2)

# notice that removing all explanatory variables that aren't significant
# would really be poor action because the collinearity problem has resulted
# in inflated standard errors and larger p-values so things that might
# actually have an effect appear non-significant

# filter columns to a model that has all the right coefficient signs
# and closer to anticipated significance 
#  remove PuttsPerRound, BounceBack, Scrambling, AveDrivingDistance 
golf.out3<-lm(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves,
              data=golf)
summary(golf.out3)


# Analysis

# model: log(PrizeMoney) = beta0 + beta1 DrivingAccuracy + beta2 GIR + beta3 PuttingAverage 
#                          + beta4 BirdieConversion + beta5 SandSaves + epsilon, epsilon~N(0,sigma2)
golf.out<-lm(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves,
              data=golf)
# table of estimates and std errors
summary(golf.out)

# 95% CI's on beta's
confint(golf.out)

# graphics to demonstrate effect of different skills on log(PrizeMoney)
crPlots(golf.out)
# could also have used added-variable plots, but the x-axis of those plots 
# might be too confusing to the golfer audience

# example of prediction
# Tiger Woods (row 178) Actual PrizeMoney = $662,771
exp(predict(golf.out,interval="prediction")[178,])
# prediction: $548,880.0  95% PI: $131,565.2 $2,289,885.5 
# notice that the actual was within the 95% PI

# Note: remember when we did the regression tree? that model provided a 
#       very different Tiger Woods prediction than actual!
#       why the difference? we did our EDA and found how to represent
#       the phenomenon

