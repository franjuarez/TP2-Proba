x <- c(480.5033, 151.2696, 37.1686, 147.8196, 311.9625, 245.7312, 25.8936, 1328.6803, 8.82, 66.0797, 112.1126, 338.2566, 
       393.5251, 113.2318, 45.1083, 315.411, 54.9601, 109.5206, 238.4002, 140.5416, 279.9851, 76.1324, 118.2646, 49.8022,
       199.078, 96.8138, 83.6227, 46.9447, 530.5744, 242.2817, 25.7543, 95.1236, 16.726, 19.1484, 104.8183, 64.7622, 
       134.0152, 4.3256, 84.7861, 17.1871, 63.4184, 29.425, 79.8002, 65.2496, 26.8381, 104.4241, 149.965, 43.1056, 
       215.5325, 52.124, 90.7957, 32.6949, 60.6023, 21.4754, 3.4372, 217.2645, 660.9188, 19.0902, 84.9835, 126.4881, 
       353.9214, 222.0169, 27.6702, 86.4604, 26.4005, 7.2625, 14.516, 73.2424, 28.9211, 168.8319, 92.2618, 523.264, 
       95.9938, 43.0798, 57.7511, 571.3289, 121.6107, 515.5202, 80.1894, 11.436, 425.4268, 385.9359, 127.6781, 234.5885, 
       107.8463, 17.5475, 26.0558, 94.6805, 135.9521, 125.991, 80.2271, 22.8104, 111.8666, 111.2891, 73.0855, 134.0027, 
       46.794, 423.9612, 166.4607, 108.5671, 222.6103, 121.794, 33.0753, 7134.8657, 81.7411, 58.5533, 3.9149, 17.2704, 
       35.1165, 61.6039, 13.8661, 307.1884, 17.8825, 8377.0515, 147.7728, 118.1849, 9.2175, 18.2453, 15.0277, 56.2539, 
       892.2309, 523.3558, 19.8553, 102.0794, 73.4059, 4.7181, 15.0061, 60.233, 241.8691, 71.6336, 30.9722, 53.6521, 
       2.2133, 104.4845, 19.5584, 367.5664, 62.7119, 103.3737, 260.8997, 97.5753, 274.0285, 43.4103, 236.4135, 4.7207, 
       3.1631, 9.4561, 76.7562, 76.488, 105.2658, 82.6994, 29.4104, 61.1291, 299.208, 1892.3682, 91.4994, 30.9419, 
       640.6685, 136.3786, 70.3966, 49.0274, 390.8424, 51.2521, 457.8587, 4.1343, 20.6456, 26.4532, 21402.7052, 254.3282, 
       46.5788, 3.9951, 148.4458, 2.2077, 1672.075, 1736.7367, 85.8679, 41.2211, 18.0107, 243.1443, 3.7223, 325.3378, 
       898.3824, 27.441, 3.7369, 88.9865, 16.028, 655.8023, 241.5167, 81.1802, 432.6059, 295.4934, 320.2815, 164.8333, 
       371.1265, 164.6057, 931.6833, 149.6847, 234.0435, 590.4278, 265.8923, 284.5821, 214.3136, 148.614, 170.4, 97.3867, 
       288.7917, 1186.463, 1008.6349, 0.138, 38.9971, 108.2522, 183.6731, 48.5886, 1300.6471, 219.57, 233.3111, 247.4, 
       18148.5, 560.7, 4807.0588, 641.54, 210.2533, 39.2565, 71.0167, 174.7143, 639, 41.9781, 379.8667, 439.76, 24.1322, 
       43, 0.3114, 7.4138, 189.3, 1177.2727)
#X: densidad de poblacion por km de muchos paises del mundo
#https://www.kaggle.com/datasets/rajkumarpandey02/2023-world-population-by-country?resource=download


set.seed(103016)

# A
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
  B = 250
  replicaciones_bootstrap <- c() # VECTOR de medianas (titas sombrero)
  for (i in 1:B) {
    bootstrap_x <- sample(x, replace = TRUE)
    replicaciones_bootstrap[i] <- estimar_mediana(bootstrap_x)
  }
  
  alpha <- (1 - nivel)
  a <- quantile(replicaciones_bootstrap, alpha / 2)
  b <- quantile(replicaciones_bootstrap, 1 - (alpha / 2))

  return(c(a, b))
}


# B
cubrimiento_empirico <- function(n) {
  lambda <- 5
  nivel <- 0.95
  mediana_real <- 0.13863
  # explicar que hacemos https://youtu.be/0s3h1Tfysog?t=80 en el informe con probability distributions
  # Simulación de 1000 muestras aleatorias y cálculo de intervalos de confianza
  num_simulaciones <- 250
  
  # M.A. ~ Exp(5)
  m_a_exponencial <- rexp(n, rate = lambda)
  hist(m_a_exponencial)
  
  longitudes <- numeric(num_simulaciones)  
  cant_cubiertos <- 0
  
  for (i in 1:num_simulaciones) {
    # Simulo
    intervalo_i <- intervalo_boot(m_a_exponencial, nivel)
    a <- intervalo_i[1]
    b <- intervalo_i[2]
    
    # Actualizamos la cantidad de intervalos que cubren la mediana
    # para luego devolver la proporcion

    if (a <= mediana_real && mediana_real <= b) {
      cant_cubiertos <- cant_cubiertos + 1
    }
    
    # Lista de longitudes (despues devolvemos el promedio)
    longitudes[i] <- b - a
  }

  cubrimiento <- cant_cubiertos / num_simulaciones
  
  longitud_promedio <- mean(longitudes)
  
  # Devolvemos la lista con los resultados
  return(list(cubrimiento = cubrimiento, longitud_promedio = longitud_promedio))
}

print('Intervalo de confianza para x con nivel 0.95')
intervalo_boot(x, 0.95)

# C
print('Cubrimiento empirico con n = 10')
c1 <- cubrimiento_empirico(10)
print(c1)

print('Cubrimiento empirico con n = 100')
c2 <- cubrimiento_empirico(100)
print(c2)

print('Cubrimiento empirico con n = 500')
c3 <- cubrimiento_empirico(500)
print(c3)

# el cubrimiento da 1 casi todas las veces que usamos nivel=0.95
# si usamos por ejemplo nivel=0.75, vemos que tenemos un cubrimiento
# bastante malo (asi que no es pq ande mal la funcion)


df2 <- data.frame(
  n = c(10, 100, 500),
  cubrimiento = sapply(list(c1, c2, c3), function(x) x$cubrimiento),
  longitud_promedio = sapply(list(c1, c2, c3), function(x) x$longitud_promedio)
)
print(df2)

#