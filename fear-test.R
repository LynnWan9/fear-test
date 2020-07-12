library(tidyverse)
library(car)
library(emmeans)
library(ggpubr)
library(nnet)
library(car)

# read data
data <- read.csv("data.csv")
# glimpse at data
head(data)
str(data)
# convert to factor
data$Group <- as.factor(data$Group)
data$FeatherScore <- as.factor(data$FeatherScore)

## Aggregated Figures 
# only TI 
data %>%
  pivot_longer(Inductions:TIDuration, names_to="yname", values_to="Response") %>%
  ggplot(aes(x = Trt, y = Response, colour=Trt)) + geom_boxplot() +
  facet_wrap(~yname, scales = "free", ncol = 3) +
  xlab("Treatment")

# only NA
data %>%
  pivot_longer(Move:Defecate, names_to="yname", values_to="Response") %>%
  ggplot(aes(x = Trt, y = Response, colour=Trt)) + geom_boxplot() +
  facet_wrap(~yname, scales = "free", ncol = 3) +
  xlab("Treatment")

## Figures by Group
# only TI 
data %>%
  pivot_longer(Inductions:TIDuration, names_to="yname", values_to="Response") %>%
  ggplot(aes(x = Trt, y = Response, colour=Trt)) + geom_boxplot() +
  facet_wrap(~yname + Group, scales = "free", ncol = 4) +
  xlab("Treatment")

# only NA
data %>%
  pivot_longer(Move:Defecate, names_to="yname", values_to="Response") %>%
  ggplot(aes(x = Trt, y = Response, colour=Trt)) + geom_boxplot() +
  facet_wrap(~yname + Group, scales = "free", ncol = 4) +
  xlab("Treatment")

## Method 1: Continuous Varibles
### RCBD with interaction
# TI test
# TIDuration as response
fit1_1 <- lm(TIDuration ~ Trt * FeatherScore + Group, data = data)
Anova(fit1_1, type = 3, singular.ok = T)
# Inductions as response
fit1_2 <- lm(Inductions ~ Trt * FeatherScore + Group, data = data)
Anova(fit1_2, type = 3, singular.ok = T)
# HeadMove as response
fit1_3 <- lm(HeadMove ~ Trt * FeatherScore + Group, data = data)
Anova(fit1_3, type = 3, singular.ok = T)
# NA test
# Move as response
fit1_4 <- lm(Move ~ Trt * FeatherScore + Group, data = data)
Anova(fit1_4, type = 3, singular.ok = T)
# Vocal as response
fit1_5 <- lm(Vocal ~ Trt * FeatherScore + Group, data = data)
Anova(fit1_5, type = 3, singular.ok = T)
# Pecks as response
fit1_6 <- lm(Pecks ~ Trt * FeatherScore + Group, data = data)
Anova(fit1_6, type = 3, singular.ok = T)
# Jumps as response
fit1_7 <- lm(Jumps ~ Trt * FeatherScore + Group, data = data)
Anova(fit1_7, type = 3, singular.ok = T)
# Defecate as response
fit1_8 <- lm(Defecate ~ Trt * FeatherScore + Group, data = data)
Anova(fit1_8, type = 3, singular.ok = T)

### normality check
par(mfrow = c(2, 4))

qqnorm(resid(fit1_1))
qqline(resid(fit1_1))

qqnorm(resid(fit1_2))
qqline(resid(fit1_2))

qqnorm(resid(fit1_3))
qqline(resid(fit1_3))

qqnorm(resid(fit1_4))
qqline(resid(fit1_4))

qqnorm(resid(fit1_5))
qqline(resid(fit1_5))

qqnorm(resid(fit1_6))
qqline(resid(fit1_6))

qqnorm(resid(fit1_7))
qqline(resid(fit1_7))

qqnorm(resid(fit1_8))
qqline(resid(fit1_8))

plot(fitted(fit1_1), resid(fit1_1))
plot(fitted(fit1_2), resid(fit1_2))
plot(fitted(fit1_3), resid(fit1_3))
plot(fitted(fit1_4), resid(fit1_4))
plot(fitted(fit1_5), resid(fit1_5))
plot(fitted(fit1_6), resid(fit1_6))
plot(fitted(fit1_7), resid(fit1_7))
plot(fitted(fit1_8), resid(fit1_8))

### RCBD without interaction
# TI test
# TIDuration as response
fit2_1 <- lm(TIDuration ~ Trt + FeatherScore + Group, data = data)
Anova(fit2_1, type = 3)
# Inductions as response
fit2_2 <- lm(Inductions ~ Trt + FeatherScore + Group, data = data)
Anova(fit2_2, type = 3)
# HeadMove as response
fit2_3 <- lm(HeadMove ~ Trt + FeatherScore + Group, data = data)
Anova(fit2_3, type = 3)
# NA test
# Move as response
fit2_4 <- lm(Move ~ Trt + FeatherScore + Group, data = data)
Anova(fit2_4, type = 3)
# Vocal as response
fit2_5 <- lm(Vocal ~ Trt + FeatherScore + Group, data = data)
Anova(fit2_5, type = 3)
# Pecks as response
fit2_6 <- lm(Pecks ~ Trt + FeatherScore + Group, data = data)
summary(fit2_6)
Anova(fit2_6, type = 3)
diff1 <- emmeans(fit2_6, pairwise ~ Trt)
plot(diff1$contrasts)
# Jumps as response
fit2_7 <- lm(Jumps ~ Trt + FeatherScore + Group, data = data)
Anova(fit2_7, type = 3)
diff2 <- emmeans(fit2_7, pairwise ~ FeatherScore)
plot(diff2$contrasts)
# Defecate as response
fit2_8 <- lm(Defecate ~ Trt + FeatherScore + Group, data = data)
Anova(fit2_8, type = 3)

### normality check
par(mfrow = c(2, 4))
qqnorm(resid(fit2_1))
qqline(resid(fit2_1))

qqnorm(resid(fit2_2))
qqline(resid(fit2_2))

qqnorm(resid(fit2_3))
qqline(resid(fit2_3))

qqnorm(resid(fit2_4))
qqline(resid(fit2_4))

qqnorm(resid(fit2_5))
qqline(resid(fit2_5))

qqnorm(resid(fit2_6))
qqline(resid(fit2_6))

qqnorm(resid(fit2_7))
qqline(resid(fit2_7))

qqnorm(resid(fit2_8))
qqline(resid(fit2_8))

plot(fitted(fit2_1), resid(fit2_1))
plot(fitted(fit2_2), resid(fit2_2))
plot(fitted(fit2_3), resid(fit2_3))
plot(fitted(fit2_4), resid(fit2_4))
plot(fitted(fit2_5), resid(fit2_5))
plot(fitted(fit2_6), resid(fit2_6))
plot(fitted(fit2_7), resid(fit2_7))
plot(fitted(fit2_8), resid(fit2_8))

## Method2: new levels Response
### variables as factors

# pecks (0: not peck, 1: peck)
data$Pecks <- as.factor(ifelse(data$Pecks == 0, "0", "1"))
# jump (0: no jump, 1: jump)
data$Jumps <- as.factor(ifelse(data$Jumps == 0, "0", "1"))
# defecate (0: no defecate, 1: defecate)
data$Defecate <- as.factor(ifelse(data$Defecate == 0, "0", "1"))
# headmove
HM1 <- ifelse(data$HeadMove <100 & data$HeadMove > 0, 1, data$HeadMove)
HM2 <- ifelse(HM1 <200 & HM1 >= 100, 2, HM1)
HM3 <- ifelse(HM2 <300 & HM2 >= 200, 3, HM2)
HM4 <- ifelse(HM3 <400 & HM3 >= 300, 4, HM3)
HM5 <- ifelse(HM4 <500 & HM4 >= 400, 5, HM4)
HM6 <- ifelse(HM5 <600 & HM5 >= 500, 6, HM5)
HM7 <- ifelse(HM6 >= 600, 7, HM6)
data$HeadMove <- as.factor(HM7)
#TIDuration
TID1 <- ifelse(data$TIDuration <100 & data$TIDuration > 0, 1, data$TIDuration)
TID2 <- ifelse(TID1 <200 & TID1 >= 100, 2, TID1)
TID3 <- ifelse(TID2 <300 & TID2 >= 200, 3, TID2)
TID4 <- ifelse(TID3 <400 & TID3 >= 300, 4, TID3)
TID5 <- ifelse(TID4 <500 & TID4 >= 400, 5, TID4)
TID6 <- ifelse(TID5 <600 & TID5 >= 500, 6, TID5)
TID7 <- ifelse(TID6 >= 600, 7, TID6)
data$TIDuration <- as.factor(TID7)
# Move
Move1 <- ifelse(data$Move <100 & data$Move > 0, 1, data$Move)
Move2 <- ifelse(Move1 <200 & Move1 >= 100, 2, Move1)
Move3 <- ifelse(Move2 <300 & Move2 >= 200, 3, Move2)
Move4 <- ifelse(Move3 <400 & Move3 >= 300, 4, Move3)
Move5 <- ifelse(Move4 <500 & Move4 >= 400, 5, Move4)
Move6 <- ifelse(Move5 <600 & Move5 >= 500, 6, Move5)
Move7 <- ifelse(Move6 >= 600, 7, Move6)
data$Move <- as.factor(Move7)
#Vocal
Vocal1 <- ifelse(data$Vocal <100 & data$Vocal > 0, 1, data$Vocal)
Vocal2 <- ifelse(Vocal1 <200 & Vocal1 >= 100, 2, Vocal1)
Vocal3 <- ifelse(Vocal2 <300 & Vocal2 >= 200, 3, Vocal2)
Vocal4 <- ifelse(Vocal3 <400 & Vocal3 >= 300, 4, Vocal3)
Vocal5 <- ifelse(Vocal4 <500 & Vocal4 >= 400, 5, Vocal4)
Vocal6 <- ifelse(Vocal5 <600 & Vocal5 >= 500, 6, Vocal5)
Vocal7 <- ifelse(Vocal6 >= 600, 7, Vocal6)
data$Vocal <- as.factor(Vocal7)

### model with interaction
# set the control trt to be the base group
data$Trt <- relevel(data$Trt, ref = "Control")

# TI test
# TIDuration as response
fit3_1 <- multinom(TIDuration ~ Trt * FeatherScore + Group, data)
Anova(fit3_1, type = 3, singular.ok = T)
# HeadMove as response
fit3_3 <- multinom(HeadMove ~ Trt * FeatherScore + Group, data = data)
Anova(fit3_3, type = 3, singular.ok = T)
# NA test
# Move as response
fit3_4 <- multinom(Move ~ Trt * FeatherScore + Group, data = data)
Anova(fit3_4, type = 3, singular.ok = T)
# Vocal as response
fit3_5 <- multinom(Vocal ~ Trt * FeatherScore + Group, data = data)
Anova(fit3_5, type = 3, singular.ok = T)
# Pecks as response
fit3_6 <- glm(Pecks ~ Trt * FeatherScore + Group, family = binomial, data)
Anova(fit3_6, type = 3, singular.ok = T)
# Jumps as response
fit3_7 <- glm(Jumps ~ Trt * FeatherScore + Group, family = binomial, data)
Anova(fit3_7, type = 3, singular.ok = T)
# Defecate as response
fit3_8 <- glm(Defecate ~ Trt * FeatherScore + Group, family = binomial, data)
Anova(fit3_8, type = 3, singular.ok = T)

### model without interaction
# TI test
# TIDuration as response
fit4_1 <- multinom(TIDuration ~ Trt + FeatherScore + Group, data = data)
Anova(fit4_1, type = 3, singular.ok = T)
# HeadMove as response
fit4_3 <- multinom(HeadMove ~ Trt + FeatherScore + Group, data = data)
Anova(fit4_3, type = 3, singular.ok = T)
# NA test
# Move as response
fit4_4 <- multinom(Move ~ Trt + FeatherScore + Group, data = data)
Anova(fit4_4, type = 3, singular.ok = T)
# Vocal as response
fit4_5 <- multinom(Vocal ~ Trt + FeatherScore + Group, data = data)
Anova(fit4_5, type = 3, singular.ok = T)
# Pecks as response
fit4_6 <- glm(Pecks ~ Trt + FeatherScore + Group, family = binomial, data)
Anova(fit4_6, type = 3, singular.ok = T)
# Jumps as response
fit4_7 <- glm(Jumps ~ Trt + FeatherScore + Group, family = binomial, data)
Anova(fit4_7, type = 3, singular.ok = T)

## Multivariate Multiple Regression

### continuous TI Test
TIresponse1 <- with(data, cbind(Inductions, TIDuration, HeadMove))
# with interaction
TImlm1 <- manova(TIresponse1 ~ Trt * FeatherScore + Group, data = data)
summary(TImlm1)
# without interaction
TImlm2 <- manova(TIresponse1 ~ Trt + FeatherScore + Group, data = data)
summary(TImlm2)

### continuous NA Test
NAresponse1 <- with(data, cbind(Move, Vocal, Pecks, Jumps, Defecate))
# with interaction
NAmlm1 <- manova(NAresponse1 ~ Trt * FeatherScore + Group, data = data)
summary(NAmlm1)
# without interaction
NAmlm2 <- manova(NAresponse1 ~ Trt + FeatherScore + Group, data = data)
summary(NAmlm2)

### continuous TI & NA
response1 <- cbind(TIresponse1, NAresponse1)
# with interaction
TINAmlm1 <- manova(response1 ~ Trt * FeatherScore + Group, data = data)
summary(TINAmlm1)
# without interaction
TINAmlm2 <- manova(response1 ~ Trt + FeatherScore + Group, data = data)
summary(TINAmlm2)

### categorical TI Test
TIresponse2 <- with(data, cbind(as.factor(Inductions), as.factor(TIDuration), as.factor(HeadMove)))
# with interaction
TImlm3 <- manova(TIresponse2 ~ Trt * FeatherScore + Group, data = data)
summary(TImlm3)
# without interaction
TImlm4 <- manova(TIresponse2 ~ Trt + FeatherScore + Group, data = data)
summary(TImlm4)

### categorical NA Test
NAresponse2 <- with(data, cbind(as.factor(Move), as.factor(Vocal),
                                as.factor(Pecks), as.factor(Jumps), as.factor(Defecate)))
# with interaction
NAmlm3 <- manova(NAresponse2 ~ Trt * FeatherScore + Group, data = data)
summary(NAmlm3)
# without interaction
NAmlm4 <- manova(NAresponse2 ~ Trt + FeatherScore + Group, data = data)
summary(NAmlm4)

### categorical TI&NA
response2 <- cbind(TIresponse2, NAresponse2)
# with interaction
TINAmlm3 <- manova(response2 ~ Trt * FeatherScore + Group, data = data)
summary(TINAmlm3)
# without interaction
TINAmlm4 <- manova(response2 ~ Trt + FeatherScore + Group, data = data)
summary(TINAmlm4)
