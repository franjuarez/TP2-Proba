x <- c(20, 4, 6, 8, 10, 101, 99, 70, 80, 50)


estimar_mediana <- function(x) {
  func_empirica <- ecdf(x)
  

  empirica_minima <- func_empirica(x[1])
  mediana_minima <- x[1]
    
    
  for (i in 2:length(x)) {
    empirica_actual <- func_empirica(x[i])
    
    
    if (empirica_actual >= 0.5) {
      if (empirica_minima < 0.5 | empirica_actual < empirica_minima) {
        empirica_minima <- empirica_actual
        mediana_minima <- x[i] ############ da 4,6, y 8 bastantes veces, ta bien?
        
        #if (mediana_minima <= 6) {
        #  print(mediana_minima)
        #  print(x)
        #}
        
      }
    }
  }
  return (mediana_minima)
}

intervalo_boot <- function(x, nivel) {
  B = 1000
  replicaciones_bootstrap <- c() # VECTOR de medianas (titas sombrero)
  for (i in 1:B) {
    bootstrap_x <- sample(x, replace = TRUE)
    replicaciones_bootstrap[i] <- estimar_mediana(bootstrap_x)
    
  }
  
  
  print(replicaciones_bootstrap)
  
  
  cota_inf <- quantile(replicaciones_bootstrap, nivel/2)
  cota_sup <- quantile(replicaciones_bootstrap, 1 - nivel/2)
  
  #P(cota_inf <= mediana_real <= cota_sup) >= 1-nivel
  
  
}


intervalo_boot(x,0.95)
