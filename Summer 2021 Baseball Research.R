attach(Summer_2021_Baseball_Research_Data)



mod1.lm <- lm(wRCplus ~ G + PA + HR + R + RBI + BABIP + OBP + wOBA + WAR + Off)
summary(mod1.lm)
library(car)
crPlots(mod1.lm)
#Perhaps second order with OBP and WAR? Log for HR?
mod2.lm <- lm(wRCplus ~ G + PA + HR + R + RBI + BABIP + OBP + wOBA + WAR + Off + 
                I(OBP^2) + I(WAR^2))
summary(mod2.lm)
library(olsrr)
ols_step_forward_p(mod2.lm)
#selected variables: HR, R, RBI, OBP
ols_step_forward_aic(mod2.lm)
#selected variables: wOBA, BABIP, Off, I(WAR^2), WAR, I(OBP^2), OBP, RBI, PA, R, HR, G
ols_step_backward_p(mod2.lm)
#selected variables: G + PA + HR + R + RBI + OBP + wOBA + WAR + Off + I(OBP^2) +  I(WAR^2)
ols_step_backward_aic(mod2.lm)
#selected variables: G + PA + HR + R + RBI + BABIP + OBP + wOBA + WAR + Off + I(OBP^2) +  I(WAR^2)
ols_step_both_aic(mod2.lm)
#selected variables: wOBA, BABIP, Off, I(WAR^2), WAR, I(OBP^2), OBP, RBI, PA, R, HR, G
ols_step_both_p(mod2.lm)
#selected variables: HR, R, RBI, OBP
mod2.lm <- lm(wRCplus ~ G + PA + HR + R + RBI + BABIP + OBP + wOBA + WAR + Off + I(OBP^2) + I(WAR^2))
summary(mod2.lm)
k=ols_step_best_subset(mod2.lm)
plot(k)
k

mod3.lm <- lm(wRCplus ~ OBP + wOBA + WAR + Off + I(OBP^2) + I(WAR^2))
summary(mod3.lm)
ols_vif_tol(mod3.lm)
plot(mod3.lm)
#Too high of VIF

mod4.lm <- lm(wRCplus ~ wOBA + WAR + Off + I(OBP^2) + I(WAR^2))
summary(mod4.lm)
ols_vif_tol(mod4.lm)
plot(mod4.lm)

mod5.lm <- lm(wRCplus ~ OBP + wOBA + WAR + Off + OBP:wOBA + OBP:WAR + OBP:Off 
              + wOBA:WAR + wOBA:Off + WAR:Off+ I(OBP^2) + I(WAR^2))
summary(mod5.lm)
plot(mod5.lm)
k=ols_step_best_subset(mod5.lm)
plot(k)
k

mod6.lm <- lm(wRCplus ~ wOBA + WAR + OBP:wOBA + wOBA:WAR + wOBA:Off + I(OBP^2))
summary(mod6.lm)
plot(mod6.lm)
ols_vif_tol(mod6.lm)


print(predict(mod6.lm, Summer_2021_Baseball_Research_Data))
PREDwRCPLUS <- predict(mod6.lm, Summer_2021_Baseball_Research_Data)
Summer_2021_Baseball_Research_Data$PredwRCPlus <- PREDwRCPLUS
DIFF <- wRCplus - PREDwRCPLUS
Summer_2021_Baseball_Research_Data$Diff <- DIFF


plot(wRCplus,PREDwRCPLUS)
BaseCor <- Summer_2021_Baseball_Research_Data[,3:23]
cor(BaseCor)

library(ggplot2)
# Basic scatter plot

ggplot(Summer_2021_Baseball_Research_Data, aes(x=wRCplus, y=PREDwRCPLUS)) + 
  geom_point() + geom_smooth(method=lm) 
(cor(wRCplus,PREDwRCPLUS))^2




#calculating leverage of all points

influence(mod6.lm)$hat


Summer_2021_Baseball_Research_Data[which(influence(mod6.lm)$hat> 2*3/25),]

#plotting leverage

plot(influence(mod6.lm)$hat)

#getting all measures of influence together

print(influence.measures(mod6.lm))

#obtaining cooks distance only

cooks.distance(mod6.lm)

#plotting cook's distance

plot(cooks.distance(mod6.lm))

#only obtaining observations with high Cook's distance values

Summer_2021_Baseball_Research_Data[which(cooks.distance(mod6.lm) > 1),]

#obtaining DFFITS

dffits(mod6.lm)

#plotting DFFITS

plot(dffits(mod6.lm))


Summer_2021_Baseball_Research_Data[which(abs(dffits(mod6.lm)) > 2*sqrt(3/25)),]

covratio(mod6.lm)
plot(covratio(mod6.lm))
Summer_2021_Baseball_Research_Data[which(abs(covratio(mod6.lm)-1) > 3*3/25),]
