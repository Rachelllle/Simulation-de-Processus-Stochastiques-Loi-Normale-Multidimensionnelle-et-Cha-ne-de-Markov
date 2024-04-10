simulate_markov_chain <- function(P, pi_0, n) {
  
  # Vecteur d'états avec n éléments
  states <- numeric(n)

  # Calculer la fonction de distribution cumulative (CDF) pour pi_0
  cdf <- cumsum(pi_0)

  # Générer un nombre aléatoire u0 pour l'état initial
  u0 <- runif(1)

  # Choisir l'état initial en fonction de la comparaison avec la CDF
  states[1] <- which(u0 <= cdf)[1]

  # Mettre à jour p avec les probabilités de la première étape
  p <- P[states[1], ]
  
  for (i in 2:n) {

    # Calculer la fonction de distribution cumulative (CDF) pour la ligne choisie dans P
    cdf <- cumsum(p)

    # Générer un nombre aléatoire u
    u <- runif(1)
    
    # Choisir l'état suivant en fonction de la comparaison avec la CDF
    states[i] <- which(u <= cdf)[1]

    # Mettre à jour p avec les probabilités de l'état actuel
    p <- P[states[i], ]
  }
  
  return(states)
}

# Définir la matrice de transition P et les probabilités initiales pi_0
P <- matrix(c(0.7, 0.3, 0.2, 0.8), nrow = 2, byrow = TRUE)
pi_0 <- c(0.5, 0.5)

# Définir le nombre d'étapes
n <- 10

# Exécuter la simulation
resultat <- simulate_markov_chain(P, pi_0, n)
print(resultat)
