# Lire les données.

data <- read.csv("basketball.csv", header=TRUE)
points = data$X5 # Y : points
height = data$X1 # X1: height
weight = data$X2 # X2: weight
field = data$X3  # X3: field
free = data$X4   # X4: free
n = length(points)   # 54 joueurs



# Statistiques descriptives du jeu de données.
head(data)
summary(points)
summary(height)
summary(weight)
summary(field)
summary(free)


# Matrice de diagramme de dispersion de la variable de réponse et de chaque prédicteur
pairs(points ~ height + weight + free + field, cex.labels = 2)


### Approche simple ###
x = cbind(1, field, free)
xtx = t(x) %*% x


# Vérifier la multicollinéarité
lambda = eigen(xtx)$values
lambda                  # 3 valeurs propres 94.7953878,  0.3816589,  0.1344273
lambda[1] / lambda[3]   # Nombre condition est 705.1798

cor_matrix <- cor(cbind(points, height, weight, field, free))
cor_matrix

# (X′X)−1 
xtxi = solve(xtx)
xtx %*% xtxi      
# proche de la matrice identité
# 1.000000e+00  1.199041e-14    0
# 7.105427e-15  1.000000e+00    0
# 7.105427e-15 -7.327472e-15    1

# Calculer beta chapeau   
#β = (X′X)−1 X′Y,.
beta.hat = xtxi %*% t(x) %*% points
beta.hat
# intercept -15.27738
# field     35.82503
# free      14.79905

#res = Y-Y.hat = Y - X (X′X)−1 X′Y =(In - X (X′X)−1 X′) Y
hat.matrix = x %*% xtxi %*% t(x)
residual = (diag(n) - hat.matrix) %*% points
# somme des carrés des résidus
SS.res = sum(residual * residual)
SS.res # 1516.422
# carré moyen des résidus σ2.hatβ = σ².hat (X′X)−1 = (|ε|²/(n − p)) (X′X)−1
sigma.squared.hat = SS.res/(n - 2)
sigma.squared.hat # 29.16196

# Vérifier l'hypothèse normale .
s = residual/sqrt(sigma.squared.hat)
qqnorm(s, main = "Diagramme QQ normal des résidus standardisés")
qqline(s, col = "blue")
abline(a = 0, b = 1, col = 'red')

# Vérifier l'homogénéité.
fit_value = x%*%beta.hat
plot(fit_value, residual, main = "Résidus vs. valeurs ajustées")
abline(0, 0, col = "red")

# Test d'hypothèse
tvaluebeta1hat <- beta.hat[2] / sqrt(sigma.squared.hat * xtxi[2, 2])
tvaluebeta1hat
pvalue1 = 2 * (1 - pt(abs(tvaluebeta1hat), df = n - 2))
pvalue1

tvaluebeta2hat <- beta.hat[3] / sqrt(sigma.squared.hat * xtxi[3, 3])
tvaluebeta2hat
pvalue2 = 2 * (1 - pt(abs(tvaluebeta2hat), df = n - 2))
pvalue2

# La droite dans le graphique ci-dessus fournit une preuve supplémentaire
# que notre modèle est un modèle valide pour cet ensemble de données.
plot(x%*%beta.hat, points,xlab="Valeurs ajustées", main = "Valeurs réelles vs. valeurs ajustées")
abline(lsfit(x%*%beta.hat, points), col = "red")

# Confirmer les résultats en utilisant la fonction lm.
m1 <- lm(points~field+free)
summary(m1)


### Recherche séquentielle pour trouver un meilleur modèle ###

# Corrélation entre la hauteur et le poids
cor(height, weight)   # 0.834324

m0 <- lm(points~height+weight+field+free+field:free)

# Sélection avant basée sur le BIC
mint <- lm(points~1)
forwardBIC <- step(mint,
                   scope=list(
                     lower=~1,upper=~height+weight+field+free+field:free),
                   direction="forward", k=log(n))

# Créer un graphique à barres de ces BICs.
fBIC <- c(194.66, 192.07, 191.32, 191.32)
barplot(fBIC, main="Sélection avant par BIC: BIC de différents modèles",
        ylab="BIC", space = 0.5,names.arg=c("Début","+ field","+ height", "+ none"),
        ylim = c(180, 200), border="red")


# Elimination arrière basée sur le BIC
backBIC <- step(m0,direction="backward", k=log(n))

# Créer des graphiques à barres de ces BICs.
bBIC <- c(199.87, 195.96, 193.10, 191.32, 191.32)
barplot(bBIC, main="Élimination arrière par BIC: BIC de différents modèles",
        ylab="BIC", space = 0.5, names.arg=c("Complet","- weight","- field:free", "- free", "- none"),
        ylim = c(180, 200), border="red")


# Calculer le modèle final
x = cbind(1, height, field)
xtx = t(x) %*% x
xtxi = solve(xtx)

# Vérifier la multicollinéarité
xtx %*% xtxi
#  1.000000e+00 -7.105427e-15 -2.842171e-14
# -2.842171e-14  1.000000e+00  0.000000e+00
# -6.217249e-15 -5.329071e-15  1.000000e+00

# Calculer beta chapeau.
beta.hat = xtxi %*% t(x) %*% points
beta.hat
# 15.209793
# -4.034628
# 51.562276

hat.matrix = x %*% xtxi %*% t(x)
residual = (diag(n) - hat.matrix) %*% points
# somme des carrés des résidus
SS.res = sum(residual * residual)
SS.res # 1495.732
# carré moyen des résidus
sigma.squared.hat = SS.res/(n - 2)
sigma.squared.hat # 28.76407

# Vérifier l'hypothèse normale.
s = residual/sqrt(sigma.squared.hat)
qqnorm(s, main = "Diagramme QQ normal des résidus standardisés")
qqline(s, col = "blue")
abline(a = 0, b = 1, col = 'red')

# Vérifier l'homogénéité.
fit_value = x%*%beta.hat
plot(fit_value, residual, main = "Résidus vs. valeurs ajustées")
abline(0, 0, col = "red")

# Test d'hypothèse
tvaluebeta1hat <- beta.hat[2] / sqrt(sigma.squared.hat * xtxi[2, 2])
tvaluebeta1hat
pvalue1 = 2 * (1 - pt(abs(tvaluebeta1hat), df = n - 2))
pvalue1

tvaluebeta2hat <- beta.hat[3] / sqrt(sigma.squared.hat * xtxi[3, 3])
tvaluebeta2hat
pvalue2 = 2 * (1 - pt(abs(tvaluebeta2hat), df = n - 2))
pvalue2


# La droite dans le graphique ci-dessus fournit une preuve supplémentaire
# que notre modèle est un modèle valide pour cet ensemble de données.
plot(x%*%beta.hat, points,xlab="Valeurs ajustées", main = "Valeurs réelles vs. valeurs ajustées")
abline(lsfit(x%*%beta.hat, points), col = "red")

# Confirmer les résultats en utilisant la fonction lm.
m2 <- lm(points~height+field)
summary(m2)

# Joueur A
predict1 = beta.hat[1] + beta.hat[2] * 6.3 + beta.hat[3] * 0.45
predict1 # 12.99466

# Joueur B
predict2 = beta.hat[1] + beta.hat[2] * 6.4 + beta.hat[3] * 0.46
predict2 # 13.10682