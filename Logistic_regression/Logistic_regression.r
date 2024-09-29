library(aod)
library(ggplot2)
mydata <- read.csv("binary.csv")

# 1] Description des données :
head(mydata)
summary(mydata)
sapply(mydata, sd)
xtabs(~admit + rank, data = mydata)


# 2] Utilisation du modèle logit :

  # a) Estimation des paramètres du modèle :
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)
confint(mylogit)

  # b) Test de Wald :
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4 :6)
    #Tester la différence entre le coefficient de rank=2 et le coefficient de rank=3 :
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

  # c) odds-ratios :
    #odds-ratios seulement :
exp(coef(mylogit))
    #odds ratios et 95% IC :
exp(cbind(OR = coef(mylogit), confint(mylogit)))

  # d) Probabilités prédites :
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1 :4)))
newdata1
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 4), gpa = mean(gpa), rank = factor(rep(1 :4, each = 100))))

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",se= TRUE))
newdata3 <- within(newdata3, { PredictedProb <- plogis(fit)
                                              LL <- plogis(fit - (1.96 * se.fit))
                                              UL <- plogis(fit + (1.96 * se.fit))})
head(newdata3)

ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                              ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank), size = 1)

  # e) La statistique de test de la différence de déviance pour les deux modèles :
with(mylogit, null.deviance - deviance)
with(mylogit, df.null - df.residual)
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(mylogit)


