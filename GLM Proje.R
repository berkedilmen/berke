yas = (1+9+4+6+0+0+3+3+5+3+4)
hasar_sayisi = rpois(100,yas)
hasar_sayisi

gender <- as.factor(rep(1:2, each=50, len=100))
gender2 = sample(gender)
gender2

vehicle_type <- as.factor(rep(1:4, each=17, len=100))
vehicle_type2 = sample(vehicle_type)
vehicle_type2

cities <- as.factor(rep(1:5, each=6, len=100))
cities2 = sample(cities)
cities2

non_damaged <- as.factor(rep(1:3, each=1, len=100))
non_damaged2 = sample(non_damaged)
non_damaged2

proje <- glm(hasar_sayisi ~ gender2 + vehicle_type2 + cities2 + non_damaged2 ,fam = poisson(link = log))
summary(proje)
names(proje)

proje$coef

proje$fitted.values

proje$linear.predictors

plot(proje$fitted.values, hasar_sayisi, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(proje$fitted ~ hasar_sayisi), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

AIC(proje)


anova(proje, test="Chisq")


########################################################
########################################################
proje1 <- glm(hasar_sayisi ~ gender2 + vehicle_type2 ,fam = poisson(link = log))
summary(proje1)
names(proje1)

proje1$coef

proje1$fitted.values

proje1$linear.predictors

plot(proje1$fitted.values, hasar_sayisi, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(proje1$fitted ~ hasar_sayisi), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

AIC(proje1)

anova(proje1, test="Chisq")

########################################################
########################################################

proje2 <- glm(hasar_sayisi ~ non_damaged2 + vehicle_type2 ,fam = poisson(link = log))
summary(proje2)
names(proje2)

proje2$coef

proje2$fitted.values

proje2$linear.predictors

plot(proje2$fitted.values, hasar_sayisi, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(proje2$fitted ~ hasar_sayisi), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

AIC(proje2)

anova(proje2, test="Chisq")
########################################################
########################################################

proje3 <- glm(hasar_sayisi ~ non_damaged2 + gender2 ,fam = poisson(link = log))
summary(proje3)
names(proje3)

proje3$coef

proje3$fitted.values

proje3$linear.predictors

plot(proje3$fitted.values, hasar_sayisi, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(proje3$fitted ~ hasar_sayisi), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

AIC(proje3)

anova(proje3, test="Chisq")
########################################################
########################################################

proje4 <- glm(hasar_sayisi ~ non_damaged2 + cities2 ,fam = poisson(link = log))
summary(proje4)
names(proje4)

proje4$coef

proje4$fitted.values

proje4$linear.predictors

plot(proje4$fitted.values, hasar_sayisi, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(proje4$fitted ~ hasar_sayisi), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

AIC(proje4)

anova(proje4, test="Chisq")
########################################################
########################################################

proje5 <- glm(hasar_sayisi ~  vehicle_type2 + cities2 ,fam = poisson(link = log))
summary(proje5)
names(proje5)

proje5$coef

proje5$fitted.values

proje5$linear.predictors

plot(proje5$fitted.values, hasar_sayisi, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(proje5$fitted ~ hasar_sayisi), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

AIC(proje5)

anova(proje5, test="Chisq")
########################################################
########################################################

proje6 <- glm(hasar_sayisi ~ gender2 + cities2 ,fam = poisson(link = log))
summary(proje6)
names(proje6)

proje6$coef

proje6$fitted.values

proje6$linear.predictors

plot(proje6$fitted.values, hasar_sayisi, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(proje6$fitted ~ hasar_sayisi), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

AIC(proje6)

anova(proje6, test="Chisq")
########################################################
########################################################

proje7 <- glm(hasar_sayisi ~ non_damaged2 + cities2 + gender2 ,fam = poisson(link = log))
summary(proje7)
names(proje7)

proje7$coef

proje7$fitted.values

proje7$linear.predictors

plot(proje7$fitted.values, hasar_sayisi, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(proje7$fitted ~ hasar_sayisi), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

AIC(proje7)

anova(proje7, test="Chisq")
########################################################
########################################################

proje8 <- glm(hasar_sayisi ~ non_damaged2 + gender2 + vehicle_type2 ,fam = poisson(link = log))
summary(proje8)
names(proje8)

proje8$coef

proje8$fitted.values

proje8$linear.predictors

plot(proje8$fitted.values, hasar_sayisi, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(proje8$fitted ~ hasar_sayisi), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

AIC(proje8)

anova(proje8, test="Chisq")
########################################################
########################################################

proje9 <- glm(hasar_sayisi ~ cities2 + gender2 + vehicle_type2 ,fam = poisson(link = log))
summary(proje9)
names(proje9)

proje9$coef

proje9$fitted.values

proje9$linear.predictors

plot(proje9$fitted.values, hasar_sayisi, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(proje9$fitted ~ hasar_sayisi), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

AIC(proje9)

anova(proje9, test="Chisq")
########################################################
########################################################

proje10 <- glm(hasar_sayisi ~ non_damaged2 + cities2 + vehicle_type2 ,fam = poisson(link = log))
summary(proje10)
names(proje10)

proje10$coef

proje10$fitted.values

proje10$linear.predictors

plot(proje10$fitted.values, hasar_sayisi, xlab = "Fitted values", ylab = "Observed claims")
abline(lm(proje10$fitted ~ hasar_sayisi), col="light blue", lwd=2)
abline(0, 1, col = "dark blue", lwd=2)

AIC(proje10)

anova(proje10, test="Chisq")

