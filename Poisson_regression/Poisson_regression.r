require(ggplot2)
require(sandwich)
require(msm) 
p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")


# 1] Description des données :
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)
# Num awards : une variable qui represente le nombre des recompenses aquieuis durant une année scolaire
# Le min est 0, le max est 6 
# Prog : Le programme d'étude : general, academic et vocational  
# Math : le score final dans la matiere mathematique : Min 33, max 75 ( /100)
# La moyenne et la mediane du score finale sont egaux, ce qui peut être interpreté par le faite de ne pas avoir trop de valeurs aberrantes. 
ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")
#La histogram resultant du bloc au dessus, montre les chiffres moyens des récompenses par type de programme et semble suggérer que le type de programme est un bon candidat pour prédire le nombre de récompenses, notre variable de résultat, car la valeur moyenne du résultat semble varier selon le programme.


# 2 ] Modelisation 
m1 <- glm(num_awards ~ prog + math, family="poisson", data=p) 
summary(m1)
cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

r.est
# Le coefficient du variable math est de 0,07. Cela signifie que le log count pour une augmentation d'une unité en mathématiques est de 0,07. 
# La variable indicatrice progAcademic compare entre prog = "Academic" et prog = "General", le log count attendu pour prog = "Academic" augmente d'environ 1,1.
# La variable indicatrice progVocational représente la différence attendue dans le log count (environ 0,37) entre prog = "Vocational" et le groupe de référence (prog = "General").


# 3 ] Test de siginificativité : 
with(m1, cbind(res.deviance = deviance, df = df.residual,
              p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# On a utilisé chi squared test pour determiné la siginificativité globale du modèle, si p-value < 0.05, alors le modele n'est pas significative et n'explique pas les données ( donc l'hypothese de linearité est detruite). LE test chi-deux se base sur la comparaison de la déviance entre le modèle ideale et le note modèle. 
# P > .05 donc notre modèle explique bien les données et globalelement siginificatif. 


# 4 ] Test du modèle : 
(s1 <- data.frame(math = mean(p$math),
                  prog = factor(1:3, levels = 1:3, labels = levels(p$prog))))
predict(m1, s1, type="response", se.fit=TRUE)
# On remarque que le nombre prédit des recompenses est 0.2 pour le programme 1 ( general), 0.62 pour le programme Academique, et 0.3 pour le programme vocational. 
## calculate and store predicted values
p$phat <- predict(m1, type="response")

## order by program and then by math
p <- p[with(p, order(prog, math)), ]

## create the plot
ggplot(p, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")
# Le graphe resultant du bloc ci-dessus confirme les resultats precedentes. La graphe confirme que le modèle favorise les programme academique dans l'attribution des recompenses, essentiellement si la personne ait un bon score en math. 

