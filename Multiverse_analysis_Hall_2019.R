
# Sample data and models provided by Jessica Hall
# Please cite Hall et al. 2019 Frontiers when referencing these data or models.
# Abbreviations are described in Table 6 of this article.


#call in data
verbbiasdata = read.table("DIRECTORY\\sample_Hall_multiverse.csv", header=TRUE, sep = ",")

#load packages
library(languageR)
library(rms)
library(lme4)
library(lmerTest)
library(nlme)
library(lattice)

#scale continous variables
d.10<-verbbiasdata
d.10$AGL1_c <- scale(d.10$AGL1) 
d.10$AGL2_c <- scale(d.10$AGL2)

#set factors
d.10$subject <- as.factor(d.10$subject)
d.10$bias <- as.factor(d.10$bias)

#check data 
summary(d.10)

## Note about data:
#
#  Data has been prescreened to remove trials with aberrant movements, that exceeded the time
#  requirments, or were not experimental trials.
# 
#  For VB1 models, consistency is the dependent variable. It is the measure of whether the participant's
#  choice matched the bias of the verb on that trial. It is coded as 1 if they match and 0 if they 
#  do not match.
#
#  For VB2, VB3, and VB4 models, MD is the dependent variable. This stands for the maximum
#  deviation of the mouse trajectory from a straight line from the trajectory's start to end
#  points for that trial. MDs that are negative mean the trajectory curved in a direction
#  away from the competitor picture, whereas MDs that are positive curved toward the competitor 
#  picture. MDs near 0 indicate a relatively straight trajectory.


# 48 models # 


# VB1 S1
VB1S1AGL1<-glmer(consistency~ 1 + AGL1_c + (1|subject), data=d.10, family=binomial)
summary(VB1S1AGL1)
VB1S1AGL2<-glmer(consistency~ 1 + AGL2_c + (1|subject), data=d.10, family=binomial)
summary(VB1S1AGL2)

## TD participants only##
d.11<- subset(d.10, dx=="TD")

VB1S1AGL1TD<-glmer(consistency~ 1 + AGL1_c + (1|subject), data=d.11, family=binomial)
summary(VB1S1AGL1TD)
VB1S1AGL2TD<-glmer(consistency~ 1 + AGL2_c + (1|subject), data=d.11, family=binomial)
summary(VB1S1AGL2TD)

# VB1 S2
VB1S2AGL1<-glmer(consistency~ 1 + AGL1_c*Ststrength + (1|subject), data=d.10, family=binomial)
summary(VB1S2AGL1)
VB1S2AGL2<-glmer(consistency~ 1 + AGL2_c*Ststrength + (1|subject), data=d.10, family=binomial)
summary(VB1S2AGL2)
VB1S2AGL1TD<-glmer(consistency~ 1 + AGL1_c*Ststrength + (1|subject), data=d.11, family=binomial)
summary(VB1S2AGL1TD)
VB1S2AGL2TD<-glmer(consistency~ 1 + AGL2_c*Ststrength + (1|subject), data=d.11, family=binomial)
summary(VB1S2AGL2TD)

# VB1 S3
d.10s<-subset(d.10, Ststrength >29)
VB1S3AGL1<-glmer(consistency~ 1 + AGL1_c + (1|subject), data=d.10s, family=binomial)
summary(VB1S3AGL1)
VB1S3AGL2<-glmer(consistency~ 1 + AGL2_c + (1|subject), data=d.10s, family=binomial)
summary(VB1S3AGL2)

## TD participants only##
d.11s<- subset(d.11, Ststrength >29)

VB1S3AGL1TD<-glmer(consistency~ 1 + AGL1_c + (1|subject), data=d.11s, family=binomial)
summary(VB1S3AGL1TD)
VB1S3AGL2TD<-glmer(consistency~ 1 + AGL2_c + (1|subject), data=d.11s, family=binomial)
summary(VB1S3AGL2TD)


## VB2 
#S1 
VB2 <- lmer(MD ~ 1+ consistency*AGL1_c+ (1|subject), data = d.10, REML = FALSE)
summary(VB2)
VB21 <- lmer(MD ~ 1+ consistency*AGL2_c+ (1|subject), data = d.10, REML = FALSE)
summary(VB21)
VB2TD <- lmer(MD ~ 1+ consistency*AGL1_c+ (1|subject), data = d.11, REML = FALSE)
summary(VB2TD)
VB21TD <- lmer(MD ~ 1+ consistency*AGL2_c+ (1|subject), data = d.11, REML = FALSE)
summary(VB21TD)
#S2
VB2S2 <- lmer(MD ~ 1+ consistency*AGL1_c*Ststrength + (1|subject), data = d.10, REML = FALSE)
summary(VB2S2)
VB21S2 <- lmer(MD ~ 1+ consistency*AGL2_c*Ststrength + (1|subject), data = d.10, REML = FALSE)
summary(VB21S2)
VB2S2TD <- lmer(MD ~ 1+ consistency*AGL1_c*Ststrength + (1|subject), data = d.11, REML = FALSE)
summary(VB2S2TD)
VB21S2TD <- lmer(MD ~ 1+ consistency*AGL2_c*Ststrength + (1|subject), data = d.11, REML = FALSE)
summary(VB21S2TD)
#S3
VB2S3 <- lmer(MD ~ 1+ consistency*AGL1_c+ (1|subject), data = d.10s, REML = FALSE)
summary(VB2S3)
## did not converge, run without random effects
VB2S3lm <- lm(MD ~ 1+ consistency*AGL1_c, data = d.10s)
summary(VB2S3lm)
VB21S3 <- lmer(MD ~ 1+ consistency*AGL2_c+ (1|subject), data = d.10s, REML = FALSE)
summary(VB21S3)
VB2S3TD <- lmer(MD ~ 1+ consistency*AGL1_c+ (1|subject), data = d.11s, REML = FALSE)
summary(VB2S3TD)
VB21S3TD <- lmer(MD ~ 1+ consistency*AGL2_c+ (1|subject), data = d.11s, REML = FALSE)
summary(VB21S3TD)


## VB3 ##
d.10i<-subset(d.10, bias=="instrument")

## VB3 S1
VB3S1<- lmer(MD ~ 1+ consistency*AGL1_c + (1|subject), data = d.10i, REML = FALSE)
summary(VB3S1)
VB3S1AGL2<- lmer(MD ~ 1+ consistency*AGL2_c + (1|subject), data = d.10i, REML = FALSE)
summary(VB3S1AGL2)
## TD only
d.11i<-subset(d.11, bias=="instrument")
VB3S1TD<- lmer(MD ~ 1+ consistency*AGL1_c + (1|subject), data = d.11i, REML = FALSE)
summary(VB3S1TD)
VB3S1AGL2TD<- lmer(MD ~ 1+ consistency*AGL2_c + (1|subject), data = d.11i, REML = FALSE)
summary(VB3S1AGL2TD)

## VB3 S2
VB3S2 <- lmer(MD ~ 1+ consistency*AGL1_c*Ststrength + (1|subject), data = d.10i, REML = FALSE)
summary(VB3S2)
VB3S2AGL2 <- lmer(MD ~ 1+ consistency*AGL2_c*Ststrength + (1|subject), data = d.10i, REML = FALSE)
summary(VB3S2AGL2)
VB3S2TD <- lmer(MD ~ 1+ consistency*AGL1_c*Ststrength + (1|subject), data = d.11i, REML = FALSE)
summary(VB3S2TD)
VB3S2AGL2TD <- lmer(MD ~ 1+ consistency*AGL2_c*Ststrength + (1|subject), data = d.11i, REML = FALSE)
summary(VB3S2AGL2TD)

## VB3 S3
d.10is<-subset(d.10i, Ststrength >29)
VB3S3 <- lmer(MD ~ 1+ consistency*AGL1_c + (1|subject), data = d.10is, REML = FALSE)
summary(VB3S3)
VB3S3AGL2 <- lmer(MD ~ 1+ consistency*AGL2_c + (1|subject), data = d.10is, REML = FALSE)
summary(VB3S3AGL2)
# TD only
d.11is<-subset(d.11i, Ststrength >29)
VB3S3TD <- lmer(MD ~ 1+ consistency*AGL1_c + (1|subject), data = d.11is, REML = FALSE)
summary(VB3S3TD)
VB3S3AGL2TD <- lmer(MD ~ 1+ consistency*AGL2_c + (1|subject), data = d.11is, REML = FALSE)
summary(VB3S3AGL2TD)

## VB4 ##
d.10im<-subset(d.10i, choice=="modifier")

## VB4 S1
VB4S1<- lmer(MD ~ 1+ consistency*AGL1_c + (1|subject), data = d.10im, REML = FALSE)
summary(VB4S1)
VB4S1AGL2<- lmer(MD ~ 1+ consistency*AGL2_c + (1|subject), data = d.10im, REML = FALSE)
summary(VB4S1AGL2)
## TD only
d.11im<-subset(d.11i, choice=="modifier")
VB4S1TD<- lmer(MD ~ 1+ consistency*AGL1_c + (1|subject), data = d.11im, REML = FALSE)
summary(VB4S1TD)
VB4S1AGL2TD<- lmer(MD ~ 1+ consistency*AGL2_c + (1|subject), data = d.11im, REML = FALSE)
summary(VB4S1AGL2TD)

## VB4 S2
VB4S2 <- lmer(MD ~ 1+ consistency*AGL1_c*Ststrength + (1|subject), data = d.10im, REML = FALSE)
summary(VB4S2)
VB4S2AGL2 <- lmer(MD ~ 1+ consistency*AGL2_c*Ststrength + (1|subject), data = d.10im, REML = FALSE)
summary(VB4S2AGL2)
VB4S2TD <- lmer(MD ~ 1+ consistency*AGL1_c*Ststrength + (1|subject), data = d.11im, REML = FALSE)
summary(VB4S2TD)
VB4S2AGL2TD <- lmer(MD ~ 1+ consistency*AGL2_c*Ststrength + (1|subject), data = d.11im, REML = FALSE)
summary(VB4S2AGL2TD)

## VB4 S4
d.10ims<-subset(d.10im, Ststrength >29)
VB4S4 <- lmer(MD ~ 1+ consistency*AGL1_c + (1|subject), data = d.10ims, REML = FALSE)
summary(VB4S4)
VB4S4AGL2 <- lmer(MD ~ 1+ consistency*AGL2_c + (1|subject), data = d.10ims, REML = FALSE)
summary(VB4S4AGL2)
# TD only
d.11ims<-subset(d.11im, Ststrength >29)
VB4S4TD <- lmer(MD ~ 1+ consistency*AGL1_c + (1|subject), data = d.11ims, REML = FALSE)
summary(VB4S4TD)
VB4S4AGL2TD <- lmer(MD ~ 1+ consistency*AGL2_c + (1|subject), data = d.11ims, REML = FALSE)
summary(VB4S4AGL2TD)
