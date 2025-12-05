# 1.2.3 Méthode print pour formater l’impression des sorties de nos fonctions


stats_desc <- function(x, format_sortie = c("vecteur", "matrice", "liste"), ...) { # Calcul
  if (is.numeric(x)) {
    stats <- c(min = min(x, ...), moy = mean(x, ...), max = max(x, ...))
  } else if (is.character(x) || is.factor(x)) { stats <- table(x)
  } else { stats <- NA
  }
  # Production de la sortie
  format_sortie <- match.arg(format_sortie) 
  if (format_sortie == "matrice") {
    stats <- as.matrix(stats)
    colnames(stats) <- if (is.character(x) || is.factor(x)) "frequence" else "stat" } else if (format_sortie == "liste") {
      stats <- as.list(stats)
    }
  out <- list(stats = stats)
  class(out) <- "stats_desc"
  return(out)
}

print.stats_desc <- function(x, ...) { 
  cat("Statistiques descriptives :\n") 
  cat("***************************\n") 
  print(x$stats, ...) 
  cat("***************************\n") 
  invisible(x)
}

# EXERCICES SUPPLEMENTAIRES
#Q1
print.optim <- function(x, ...) {
  if(x$convergence == 0){
  cat("L'algorithme a convergé.\n")
  } else{
    cat("L'algorithme n'a pas convergé.\n")
  }
  cat("Valeur optimale des paramètres :\n")
  print(x$par)
  cat("Valeur de la fonction optimisée en ces paramètres :\n ")
  print(x$value)
  invisible(x)
}



quadratique <- function(x) {
  10*x[1]^2 + 7*x[2]^2 + 4*x[1]*x[2] - 5*x[1] + 7*x[2] + 3
}
test1 <- optim(c(1,1), quadratique)
class(test1) <- "optim"
print(test1)
