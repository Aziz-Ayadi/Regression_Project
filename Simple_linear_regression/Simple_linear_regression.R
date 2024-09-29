# Charger les données
data <- read.csv("Salary_Data.csv", header=TRUE)

# Extraire les variables
points <- data$Salary # Y
height <- data$YearsExperience # X1
n <- length(points)   # Nombre d'observations

# Régression linéaire simple - Approche classique avec des formules mathématiques
# Calculer les sommes nécessaires
sum_x <- sum(height)
sum_y <- sum(points)
sum_x_squared <- sum(height^2)
sum_xy <- sum(height * points)

# Calculer les coefficients
slope <- (n * sum_xy - sum_x * sum_y) / (n * sum_x_squared - sum_x^2)
intercept <- (sum_y - slope * sum_x) / n

# Afficher l'équation
cat("Approche classique:\n")
cat("Y =", round(intercept, 2), "+", round(slope, 2), "* X\n\n")

# Régression linéaire simple - Utilisation de la fonction lm
lm_model <- lm(points ~ height, data=data)

# Afficher le résumé de lm
cat("Approche de la fonction lm:\n")
summary(lm_model)

# Tracer les données et la ligne de régression
par(mfrow=c(1, 2)) # Créer une disposition 1x2 pour les graphiques

# Nuage de points
plot(height, points, main="Nuage de points", xlab="Expérience en années", ylab="Salaire")

# Ajouter la ligne de régression
abline(coef=c(intercept, slope), col="red")

# Graphique des résidus par rapport aux valeurs ajustées
plot(lm_model, which=1)

# Effectuer un test d'hypothèse (test t pour la pente)
cat("\nTest d'hypothèse:\n")
t_test <- summary(lm_model)$coefficients["height", "t value"]
p_value <- 2 * pt(-abs(t_test), df=n-2) # Test bilatéral

cat("Valeur t :", round(t_test, 3), "\n")
cat("Valeur p :", format(p_value, scientific=TRUE, digits=3), "\n")

# Réinitialiser la disposition des graphiques
par(mfrow=c(1, 1))
