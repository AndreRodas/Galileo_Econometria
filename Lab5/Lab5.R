#autor: Andr� Rodas
#Objetivo: Desarrollar funci�n en R que permita calcular los valores de los estimadores utilizando la ecuaci�n normal. Funci�n debe de ser capaz de calcular los estimadores (par�metros) para cualquier modelo de dos variables (X y Y) y cualquier polinimio de orden p, la firma de la funci�n que debe de utilizar es la siguiente


NEEstimator <- function(Y,X,p) {
  # data: cualquier dataframe
  # Y: indice de la variable a predecir
  # X: indice de la variable predictora
  # p: grado del polinimio
  
  # Ecuaci�n normal: 
  # beta = (A.T*A).inv * (A.T*Y)
  # Matriz X depende del grado del polinomio que se desea calcular
  
  model <- lm(Y ~ poly(X,p))
  
  
  return(model)
}

set.seed(30)
x <- seq(from=0, to=20, by=0.1)
y <- 500 + 0.3 * (q-10)^3
ruido <- rnorm(length(q), mean=10, sd=80)
ruido.y <- y + ruido
plot(q,ruido.y,col='blue',xlab='q',main='Datos generados')


#Cambiar AQUI numero de polinomio "p"
model <- NEEstimator(ruido.y,x,p=3)
summary(model)

xx <- seq(from=0, to=20, by=0.1)
lines(xx, predict(model, data.frame(x=xx)), col='red')
