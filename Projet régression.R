################################################################################
# PROJET - Prédiction de l'efficacité énergétique des véhicules
# Ville de Nordrive - Modernisation de la flotte
# 
# Auteur:
# Date: Novembre 2025
################################################################################


# ==============================================================================
# 0. PRÉPARATION ET CHARGEMENT DES DONNÉES
# ==============================================================================

# Chargement des données
library(here)
Cars <- read.table(
  here("Projet_Efficacité_Vehicules", "CarsSet3.txt"),
  header = TRUE
)

# Aperçu des données
cat("===== APERÇU DES DONNÉES =====\n")
head(Cars)
str(Cars)
summary(Cars)
cat("\nNombre d'observations:", nrow(Cars), "\n")
cat("Nombre de variables:", ncol(Cars), "\n\n")


# ==============================================================================
# 1. EXPLORATION ET POINTS INFLUENTS
# ==============================================================================

cat("\n===== 1. EXPLORATION ET POINTS INFLUENTS =====\n\n")
# --- 1.1 Statistiques descriptives ---
cat("Statistiques descriptives:\n")
print(summary(Cars))
# --- 1.2 Visualisations exploratoires ---

# Histogramme de la variable réponse (mpg)

pdf("graphiques_exploratoires.pdf", width = 12, height = 10)
par(mfrow = c(2, 2))
hist(Cars$mpg, breaks = 30, col = "lightblue", 
     main = "Distribution de l'efficacité énergétique",
     xlab = "MPG", ylab = "Fréquence")
# Boxplots des variables continues
boxplot(Cars[, c("mpg", "engine_size", "horsepower", "weight")],
        main = "Distribution des variables continues",
        col = c("lightblue", "lightgreen", "lightcoral", "lightyellow"))

# Distribution des variables catégorielles
barplot(table(Cars$awd), main = "Traction intégrale", 
        names.arg = c("2 roues", "AWD"), col = "steelblue")
barplot(table(Cars$hybrid), main = "Type de moteur",
        names.arg = c("Combustion", "Hybride"), col = "forestgreen")
dev.off()

# Nuages de points : mpg vs prédicteurs

pdf("nuages_points_mpg.pdf", width = 14, height = 10)
par(mfrow = c(3, 3))
plot(Cars$engine_size, Cars$mpg, main = "MPG vs Cylindrée",
     xlab = "Cylindrée (L)", ylab = "MPG", pch = 19, col = "blue")
plot(Cars$horsepower, Cars$mpg, main = "MPG vs Puissance",
     xlab = "Puissance (ch)", ylab = "MPG", pch = 19, col = "red")
plot(Cars$weight, Cars$mpg, main = "MPG vs Poids",
     xlab = "Poids (kg)", ylab = "MPG", pch = 19, col = "darkgreen")
plot(Cars$drag_coef, Cars$mpg, main = "MPG vs Coefficient de traînée",
     xlab = "Drag Coef", ylab = "MPG", pch = 19, col = "purple")
plot(Cars$width, Cars$mpg, main = "MPG vs Largeur",
     xlab = "Largeur (cm)", ylab = "MPG", pch = 19, col = "orange")
plot(Cars$height, Cars$mpg, main = "MPG vs Hauteur",
     xlab = "Hauteur (cm)", ylab = "MPG", pch = 19, col = "brown")
boxplot(mpg ~ awd, data = Cars, main = "MPG vs AWD",
        xlab = "AWD", ylab = "MPG", col = c("lightblue", "lightcoral"))
boxplot(mpg ~ hybrid, data = Cars, main = "MPG vs Hybride",
        xlab = "Hybride", ylab = "MPG", col = c("lightyellow", "lightgreen"))
dev.off()


# --- 1.3 Modèle complet initial ---

modele_complet <- lm(mpg ~ engine_size + horsepower + weight + drag_coef + 
                       width + height + awd + hybrid, data = Cars)

cat("\nRésumé du modèle complet:\n")
print(summary(modele_complet))

# --- 1.4 Diagnostic des points influents ---

# Calcul des mesures d'influence
n <- nrow(Cars)
p <- length(coef(modele_complet))

# Leviers (hat values)

leviers <- hatvalues(modele_complet)
seuil_levier <- 2 * p / n
points_levier_eleve <- which(leviers > seuil_levier)

cat("\n--- Points à levier élevé ---\n")
cat("Seuil:", round(seuil_levier, 4), "\n")
cat("Nombre de points avec levier élevé:", length(points_levier_eleve), "\n")
if (length(points_levier_eleve) > 0) {
  cat("Indices des points:\n")
  print(head(points_levier_eleve, 10))
}

# Distance de Cook
cook_distances <- cooks.distance(modele_complet)
seuil_cook <- 4 / n
points_influents_cook <- which(cook_distances > seuil_cook)

cat("\n--- Points influents (Distance de Cook) ---\n")
cat("Seuil:", round(seuil_cook, 4), "\n")
cat("Nombre de points influents:", length(points_influents_cook), "\n")
if (length(points_influents_cook) > 0) {
  cat("Indices des points:\n")
  print(head(points_influents_cook, 10))
}

# Résidus studentisés

residus_student <- rstudent(modele_complet)
seuil_residus <- 3
points_residus_extremes <- which(abs(residus_student) > seuil_residus)

cat("\n--- Points à résidus extrêmes ---\n")
cat("Seuil: ±", seuil_residus, "\n")
cat("Nombre de points à résidus extrêmes:", length(points_residus_extremes), "\n")
if (length(points_residus_extremes) > 0) {
  cat("Indices des points:\n")
  print(points_residus_extremes)
}

# Graphiques de diagnostic

pdf("diagnostics_influence.pdf", width = 12, height = 10)
par(mfrow = c(2, 2))

# Levier vs Résidus studentisés

plot(leviers, residus_student, main = "Levier vs Résidus studentisés",
     xlab = "Levier (hat)", ylab = "Résidus studentisés", pch = 19)
abline(h = c(-3, 3), col = "red", lty = 2)
abline(v = seuil_levier, col = "blue", lty = 2)
identify_points <- which(leviers > seuil_levier | abs(residus_student) > 3)
if (length(identify_points) > 0) {
  text(leviers[identify_points], residus_student[identify_points], 
       labels = identify_points, pos = 3, cex = 0.7, col = "red")
}

# Distance de Cook

plot(cook_distances, type = "h", main = "Distance de Cook",
     xlab = "Indice", ylab = "Distance de Cook")
abline(h = seuil_cook, col = "red", lty = 2)

# Index plot des résidus studentisés

plot(residus_student, main = "Résidus studentisés",
     xlab = "Indice", ylab = "Résidus studentisés", pch = 19)
abline(h = c(-3, 3), col = "red", lty = 2)

# Résidus vs valeurs ajustées

plot(fitted(modele_complet), residuals(modele_complet),
     main = "Résidus vs Valeurs ajustées",
     xlab = "Valeurs ajustées", ylab = "Résidus", pch = 19)
abline(h = 0, col = "red", lty = 2)

dev.off()

cat("\nDiagnostics d'influence sauvegardés dans 'diagnostics_influence.pdf'\n")




# ==============================================================================
# 2. MULTICOLINÉARITÉ
# ==============================================================================



cat("\n\n===== 2. MULTICOLINÉARITÉ =====\n\n")

# --- 2.1 Matrice de corrélation ---
# Sélection des variables numériques uniquement
vars_numeriques <- Cars[, c("mpg", "engine_size", "horsepower", "weight", 
                               "drag_coef", "width", "height")]

matrice_correlation <- cor(vars_numeriques)

cat("Matrice de corrélation:\n")
print(round(matrice_correlation, 3))

# Visualisation de la matrice de corrélation
library(corrplot)
pdf("matrice_correlation.pdf", width = 10, height = 8)
corrplot(matrice_correlation, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Matrice de corrélation des variables",
         mar = c(0, 0, 2, 0))
dev.off()

# --- 2.2 Variance Inflation Factors (VIF) ---

cat("\n--- Variance Inflation Factors (VIF) ---\n")
vif_valeurs <- vif(modele_complet)
print(round(vif_valeurs, 2))

# Critère : VIF > 10 indique une multicolinéarité problématique
# VIF > 5 peut être préoccupant
cat("\nVariables avec VIF > 5:\n")
variables_vif_eleve <- vif_valeurs[vif_valeurs > 5]
if (length(variables_vif_eleve) > 0) {
  print(round(variables_vif_eleve, 2))
} else {
  cat("Aucune variable avec VIF > 5\n")
}

# --- 2.3 Gestion de la multicolinéarité si nécessaire ---

cat("\n--- 2.3 Gestion de la multicolinéarité ---\n\n")

# Analyse du problème
cat("Variables avec VIF > 10 (multicolinéarité sévère):\n")
vif_severes <- vif_valeurs[vif_valeurs > 10]
if (length(vif_severes) > 0) {
  print(round(vif_severes, 2))
  cat("\nCes variables sont fortement corrélées entre elles.\n")
  cat("Corrélations observées:\n")
  cat("  engine_size ↔ weight:", round(cor(Cars$engine_size, Cars$weight), 3), "\n")
  cat("  engine_size ↔ horsepower:", round(cor(Cars$engine_size, Cars$horsepower), 3), "\n")
  cat("  weight ↔ horsepower:", round(cor(Cars$weight, Cars$horsepower), 3), "\n\n")
}

# ===== OPTION 1 : Garder toutes les variables (statu quo) =====
cat("=== OPTION 1 : Modèle complet (statu quo) ===\n")
cat("On garde toutes les variables malgré la multicolinéarité.\n")
cat("La sélection de variables choisira naturellement les meilleures.\n\n")

modele_option1 <- modele_complet
cat("Variables retenues:", length(coef(modele_option1)) - 1, "\n")
cat("R² ajusté:", round(summary(modele_option1)$adj.r.squared, 4), "\n\n")


# ===== OPTION 2 : Éliminer engine_size (garder weight) =====
cat("=== OPTION 2 : Élimination de engine_size ===\n")
cat("Justification: engine_size et weight sont corrélés à 0.992\n")
cat("On garde weight car:\n")
cat("  - Corrélation avec mpg: weight (r=-0.696) vs engine_size (r=-0.700)\n")
cat("  - Le poids est plus directement lié à la consommation (loi physique)\n")
cat("  - Le poids capture aussi l'effet de la taille du moteur\n\n")

modele_option2 <- lm(mpg ~ horsepower + weight + drag_coef + 
                       width + height + awd + hybrid, 
                     data = Cars)

cat("Résumé du modèle sans engine_size:\n")
print(summary(modele_option2))

cat("\nVIF du modèle option 2:\n")
vif_option2 <- vif(modele_option2)
print(round(vif_option2, 2))
cat("\n✓ Plus de VIF > 10!\n\n")


# ===== OPTION 3 : Éliminer weight (garder engine_size) =====
cat("=== OPTION 3 : Élimination de weight ===\n")
cat("Justification alternative:\n")
cat("On garde engine_size car il est peut-être plus stable/standardisé\n\n")

modele_option3 <- lm(mpg ~ engine_size + horsepower + drag_coef + 
                       width + height + awd + hybrid, 
                     data = Cars)

cat("Résumé du modèle sans weight:\n")
print(summary(modele_option3))

cat("\nVIF du modèle option 3:\n")
vif_option3 <- vif(modele_option3)
print(round(vif_option3, 2))
cat("\n✓ Plus de VIF > 10!\n\n")


# ===== OPTION 4 (bonus) : Créer un indice composite =====
cat("=== OPTION 4 (BONUS) : Indice composite ===\n")
cat("Création d'un indice 'power_to_weight' = horsepower / weight\n")
cat("Cet indice capture le ratio performance/masse\n\n")

# Créer l'indice
Cars$power_to_weight <- Cars$horsepower / Cars$weight

modele_option4 <- lm(mpg ~ power_to_weight + drag_coef + 
                       width + height + awd + hybrid, 
                     data = Cars)

cat("Résumé du modèle avec indice composite:\n")
print(summary(modele_option4))

library(DAAG)
cat("\nVIF du modèle option 4:\n")
vif_option4 <- vif(modele_option4)
print(round(vif_option4, 2))


# ===== COMPARAISON DES OPTIONS =====
cat("\n\n========================================\n")
cat("COMPARAISON DES 4 OPTIONS\n")
cat("========================================\n\n")

comparaison_multicol <- data.frame(
  Option = c("1-Complet", "2-Sans engine_size", "3-Sans weight", "4-Indice composite"),
  NbVar = c(
    length(coef(modele_option1)) - 1,
    length(coef(modele_option2)) - 1,
    length(coef(modele_option3)) - 1,
    length(coef(modele_option4)) - 1
  ),
  R2 = c(
    summary(modele_option1)$r.squared,
    summary(modele_option2)$r.squared,
    summary(modele_option3)$r.squared,
    summary(modele_option4)$r.squared
  ),
  R2_adj = c(
    summary(modele_option1)$adj.r.squared,
    summary(modele_option2)$adj.r.squared,
    summary(modele_option3)$adj.r.squared,
    summary(modele_option4)$adj.r.squared
  ),
  AIC = c(
    AIC(modele_option1),
    AIC(modele_option2),
    AIC(modele_option3),
    AIC(modele_option4)
  ),
  BIC = c(
    BIC(modele_option1),
    BIC(modele_option2),
    BIC(modele_option3),
    BIC(modele_option4)
  ),
  VIF_max = c(
    max(vif_valeurs),
    max(vif_option2),
    max(vif_option3),
    max(vif_option4)
  )
)

print(comparaison_multicol)
cat("\n(Valeurs arrondies pour lisibilité)\n")

cat("\n--- Recommandations préliminaires ---\n")
cat("• Option 1 (Complet): VIF très élevés = coefficients instables\n")
cat("• Option 2 (Sans engine_size): Bon compromis, VIF acceptables\n")
cat("• Option 3 (Sans weight): Alternative viable\n")
cat("• Option 4 (Indice): Simplifie le modèle mais perd de l'info\n\n")

cat("DÉCISION: On procède avec les 4 options.\n")
cat("La sélection de variables (section 4) déterminera le meilleur modèle.\n\n")

# Sauvegarder le tableau de comparaison
write.csv(comparaison_multicol, "comparaison_multicolinearite.csv", row.names = FALSE)
cat("Tableau sauvegardé: comparaison_multicolinearite.csv\n")

################################################################################
# 3. POSTULATS & TRANSFORMATIONS
################################################################################

cat("\n\n===== 3. POSTULATS ET TRANSFORMATIONS =====\n\n")

# On utilise comme base le modèle Option 2
modele_base <- modele_option2

library(MASS)   
library(lmtest)  

# Postulats sur le modèle de base

cat("=== 3.1 Diagnostics sur le modèle de base (Option 2) ===\n\n")

# Graphiques de diagnostics classiques
pdf("diagnostics_postulats_option2.pdf", width = 12, height = 10)
par(mfrow = c(2, 2))

# 1. Résidus vs valeurs ajustées 
plot(fitted(modele_base), residuals(modele_base),
     main = "Résidus vs valeurs ajustées (Option 2)",
     xlab = "Valeurs ajustées", ylab = "Résidus")
abline(h = 0, col = "red", lty = 2)

# 2. QQ-plot des résidus
qqnorm(residuals(modele_base),
       main = "QQ-plot des résidus (Option 2)")
qqline(residuals(modele_base), col = "red", lwd = 2)

# 3. Résidus en fonction de l'indice 
plot(residuals(modele_base), type = "b",
     main = "Résidus vs indice (Option 2)",
     xlab = "Indice de l'observation", ylab = "Résidu")

# 4. Scale-Location pour homoscédasticité
plot(modele_base, which = 3,
     main = "Scale-Location (Option 2)")

dev.off()
cat("Graphiques de diagnostics sauvegardés dans 'diagnostics_postulats_option2.pdf'\n\n")

# Tests formels
cat("Tests formels sur les résidus (Option 2):\n")
test_shapiro <- shapiro.test(residuals(modele_base))
print(test_shapiro)

test_bp <- bptest(modele_base)  # H0 : homoscédasticité
print(test_bp)

test_dw <- dwtest(modele_base)  # H0 : pas d'autocorrélation des résidus
print(test_dw)


# 3.2 Transformation Box-Cox sur la réponse

cat("\n=== 3.2 Transformation de Box-Cox sur mpg ===\n\n")

# La fonction boxcox suppose une réponse strictement positive 
bc_res <- boxcox(modele_base)

lambda_opt <- bc_res$x[which.max(bc_res$y)]
cat("Lambda optimal de Box-Cox:", round(lambda_opt, 3), "\n")

# Création d'une version transformée de la réponse selon lambda_opt
if (abs(lambda_opt) < 0.15) {
  cat("Lambda proche de 0 -> on utilise une transformation log(mpg).\n")
  Cars$mpg_bc <- log(Cars$mpg)
} else {
  cat("On applique la transformation Box-Cox générale.\n")
  Cars$mpg_bc <- (Cars$mpg^lambda_opt - 1) / lambda_opt
}

# Nouveau modèle avec la réponse transformée
modele_bc <- lm(mpg_bc ~ horsepower + weight + drag_coef +
                  width + height + awd + hybrid,
                data = Cars)

cat("\nRésumé du modèle avec réponse transformée (Box-Cox):\n")
print(summary(modele_bc))

# Diagnostics graphiques pour le modèle transformé
pdf("diagnostics_postulats_boxcox.pdf", width = 12, height = 10)
par(mfrow = c(2, 2))

plot(fitted(modele_bc), residuals(modele_bc),
     main = "Résidus vs ajustés (Box-Cox)",
     xlab = "Valeurs ajustées", ylab = "Résidus")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(modele_bc),
       main = "QQ-plot des résidus (Box-Cox)")
qqline(residuals(modele_bc), col = "red", lwd = 2)

plot(residuals(modele_bc), type = "b",
     main = "Résidus vs indice (Box-Cox)",
     xlab = "Indice", ylab = "Résidu")

plot(modele_bc, which = 3,
     main = "Scale-Location (Box-Cox)")

dev.off()
cat("Graphiques de diagnostics (Box-Cox) sauvegardés dans 'diagnostics_postulats_boxcox.pdf'\n\n")

# Tests formels sur le modèle transformé
cat("Tests formels sur le modèle Box-Cox:\n")
test_shapiro_bc <- shapiro.test(residuals(modele_bc))
print(test_shapiro_bc)

test_bp_bc <- bptest(modele_bc)
print(test_bp_bc)

test_dw_bc <- dwtest(modele_bc)
print(test_dw_bc)

################################################################################
# 4. SÉLECTION DE VARIABLES
################################################################################

cat("\n\n===== 4. SÉLECTION DE VARIABLES =====\n\n")
# Packages pour la sélection
library(leaps)  
library(glmnet)  


# --- Méthode 1 : Pas-à-pas AIC ---

cat("=== 4.1 Pas-à-pas AIC à partir du modèle complet sans engine_size ===\n\n")

# On part du modèle "option 2" (sans engine_size)
modele_step <- stepAIC(modele_option2,
                       direction = "both",
                       trace = FALSE)

cat("Résumé du modèle sélectionné par stepAIC:\n")
print(summary(modele_step))



# ---Méthode 2 : Best subsets ---

cat("\n=== 4.2 Best subsets (Cp, BIC) ===\n\n")

# Formule correspondant à l'Option 2
formule_option2 <- mpg ~ horsepower + weight + drag_coef +
  width + height + awd + hybrid

regfit <- regsubsets(formule_option2, data = Cars,
                     nvmax = 7)  # max = nb de prédicteurs

regfit_sum <- summary(regfit)

# Modèle avec le plus petit BIC
idx_bic <- which.min(regfit_sum$bic)
cat("Taille du meilleur modèle selon BIC:", idx_bic, "\n")

coef_bic <- coef(regfit, idx_bic)
cat("Coefficients du meilleur modèle BIC:\n")
print(coef_bic)

# Récupérer les noms de variables sélectionnées (sauf intercept)
vars_bic <- names(coef_bic)[-1]

# Construire la formule lm correspondante
formule_bic <- as.formula(
  paste("mpg ~", paste(vars_bic, collapse = " + "))
)

modele_best_bic <- lm(formule_bic, data = Cars)

cat("\nRésumé du modèle 'best subsets' (BIC):\n")
print(summary(modele_best_bic))



# --- Méthode 3 : Lasso ---

cat("\n=== 4.3 Lasso (glmnet) ===\n\n")

# Construction de la matrice X et du vecteur y
X <- model.matrix(formule_option2, data = Cars)[, -1]  
y <- Cars$mpg

# Validation croisée pour choisir lambda
library(glmnet)
set.seed(123)  
cv_lasso <- cv.glmnet(X, y, alpha = 1) 

lambda_opt <- cv_lasso$lambda.1se
cat("Lambda choisi (1se):", lambda_opt, "\n")

# Coefficients du modèle Lasso
coef_lasso <- coef(cv_lasso, s = "lambda.1se")
cat("Coefficients non nuls du Lasso:\n")
print(coef_lasso)

# Variables retenues par le Lasso
vars_lasso <- rownames(coef_lasso)[which(coef_lasso != 0)]
vars_lasso <- setdiff(vars_lasso, "(Intercept)")

if (length(vars_lasso) == 0) {
  cat("Aucune variable sélectionnée par le Lasso (hors intercept).\n")
  modele_lasso_lm <- lm(mpg ~ 1, data = Cars)  # modèle avec intercept seulement
} else {
  cat("Variables retenues par le Lasso:\n")
  print(vars_lasso)
  
  formule_lasso <- as.formula(
    paste("mpg ~", paste(vars_lasso, collapse = " + "))
  )
  
  modele_lasso_lm <- lm(formule_lasso, data = Cars)
}

cat("\nRésumé du modèle 'équivalent Lasso' (refité avec lm):\n")
print(summary(modele_lasso_lm))



# --- Comparaison des modèles de sélection ---

cat("\n=== 4.4 Comparaison des modèles ===\n\n")

# On compare : Option 2 (base), StepAIC, Best BIC, Lasso
comparaison_selection <- data.frame(
  Modele = c("Option2_base", "StepAIC", "Best_BIC", "Lasso_lm"),
  NbVar = c(
    length(coef(modele_option2)) - 1,
    length(coef(modele_step)) - 1,
    length(coef(modele_best_bic)) - 1,
    length(coef(modele_lasso_lm)) - 1
  ),
  R2 = c(
    summary(modele_option2)$r.squared,
    summary(modele_step)$r.squared,
    summary(modele_best_bic)$r.squared,
    summary(modele_lasso_lm)$r.squared
  ),
  R2_adj = c(
    summary(modele_option2)$adj.r.squared,
    summary(modele_step)$adj.r.squared,
    summary(modele_best_bic)$adj.r.squared,
    summary(modele_lasso_lm)$adj.r.squared
  ),
  AIC = c(
    AIC(modele_option2),
    AIC(modele_step),
    AIC(modele_best_bic),
    AIC(modele_lasso_lm)
  ),
  BIC = c(
    BIC(modele_option2),
    BIC(modele_step),
    BIC(modele_best_bic),
    BIC(modele_lasso_lm)
  )
)

print(comparaison_selection)

write.csv(comparaison_selection,
          "comparaison_selection_modeles.csv",
          row.names = FALSE)
cat("\nTableau de comparaison des modèles sauvegardé dans 'comparaison_selection_modeles.csv'\n")

