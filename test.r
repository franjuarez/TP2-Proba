x <- c(20, 4, 6, 8, 10, 101, 99, 70, 80, 50) #10


estimar_mediana <- function(x) {
  func_empirica <- ecdf(x)
  

  empirica_minima <- func_empirica(x[1])
  mediana_minima <- x[1]
    
    
  for (i in 2:length(x)) {
    empirica_actual <- func_empirica(x[i])
    
    
    if (empirica_actual >= 0.5) {
      if (empirica_minima < 0.5 || empirica_actual < empirica_minima) {
        empirica_minima <- empirica_actual
        mediana_minima <- x[i] #da 4,6, y 8 bastantes veces, ta bien? Parece q si
      }
    }
  }
  return(mediana_minima)
}

intervalo_boot <- function(x, nivel) {
  B = 1000
  replicaciones_bootstrap <- c() # VECTOR de medianas (titas sombrero)
  for (i in 1:B) {
    bootstrap_x <- sample(x, replace = TRUE)
    replicaciones_bootstrap[i] <- estimar_mediana(bootstrap_x)
  }
  
  alpha <- (1 - nivel) / 2
  lower <- quantile(replicaciones_bootstrap, alpha)
  upper <- quantile(replicaciones_bootstrap, 1 - alpha)

  return(c(lower, upper))
}

#intervalo_boot(x,0.95)

cubrimiento_empirico <- function(n) {
  lambda <- 5  # Parámetro de la distribución exponencial
  nivel <- 0.05
  
  # Definir la distribución exponencial
  distribucion <- rexp(n, rate = lambda)
  
  # Simulación de 1000 muestras aleatorias y cálculo de intervalos de confianza
  num_simulaciones <- 1000
  cubrimiento <- numeric(num_simulaciones)  # Almacenar los resultados de cubrimiento
  longitudes <- numeric(num_simulaciones)  # Almacenar las longitudes de los intervalos
  intervalos <- c()
  
  for (i in 1:num_simulaciones) {
    muestra_i <- sample(distribucion, n, replace = TRUE)
    mediana_i <- estimar_mediana(muestra)
    alpha <- (1 - nivel) / 2
    lower <- quantile(muestra, alpha)
    upper <- quantile(muestra, 1 - alpha)
    
    # Comprobamos si la verdadera mediana (1/lambda) está dentro del intervalo
    cubrimiento[i] <- intervalo[1] <= (1/lambda) && intervalo[2] >= (1/lambda)
    
    # Calculamos la longitud del intervalo
    longitudes[i] <- intervalo[2] - intervalo[1]
  }
  
  # Proporción de intervalos que contienen la verdadera mediana
  proporcion_cubrimiento <- sum(cubrimiento) / num_simulaciones
  
  # Longitud promedio de los intervalos
  longitud_promedio <- mean(longitudes)
  
  # Devolvemos la lista con los resultados
  return(list(proporcion_cubrimiento = proporcion_cubrimiento, longitud_promedio = longitud_promedio))
}




