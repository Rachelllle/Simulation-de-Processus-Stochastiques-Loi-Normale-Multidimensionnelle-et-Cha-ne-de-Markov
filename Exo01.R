

generate_random_vector <- function(m, C, n) {

  # Décomposition de Cholesky de C
  L <- chol(C)
  
  # Initialiser un vecteur vide pour Z
  Z <- numeric(length(m))
  X <- matrix(0,  ncol = length(m), nrow = n)


  for( in 1:length(m)){
  # Générer un vecteur Z de loi N(0, I)
  	for (i in 1:length(m)) {
    		u1 <- runif(1)
    		u2 <- runif(1)
   		z_i <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
    		Z[i] <- z_i
      }
  
  # Calculer le vecteur aléatoire X
   X[i,] <- m + t(L) %*% t(Z)
  }

  return(X)
}

# Données d'entrée
m <- c(1, 1, 2)
C <- matrix(c(1, 1, 3, 1, 2, 4, 3, 4, 1), nrow = 3, byrow = TRUE)

# Générer un vecteur aléatoire X
result <- generate_random_vector(m, C)

# Afficher le résultat
print(result)
