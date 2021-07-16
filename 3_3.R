library(ggplot2)
library(tidyverse)

beads <- rep(c("red", "blue"), times = c(2,3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0)

ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

#Un casino quiere saber si le gana o pierde a una ruleta. Primero definimos la ruleta.
color <- rep(c("Black", "Red", "Green"), times = c(18, 18,2))
#Van a jugar mil personas.
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
#Primeros 10 resultados
X[1:10]

#Otra forma de hacerlo pero con solo una linea de codigo.
X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))
S <- sum(X)
S

#S son las ganancias del casino. Queremos conocer la probabilidad de S menor a 0.
#Corremos una simulacion Monte Carlo 10,000 veces repitiendo lo que ya hicimos.
B <- 10000
S <- replicate(B, {
  X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))
  sum(X)
})

a <- 0
mean(S < a)


mean(S)
sd(S)

s <- seq(min(S), max(S), length = 100)
normal_density <- data.frame(s = s, f=dnorm(s, mean(S), sd(S)))
data.frame(S=S) %>% ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s,f), color = "red")

#El valor esperado de una variable con dos posibles resultados es ap + b(1-p) donde p es la probabilidad de a.
#Multiplicado por n es el valor esperado de eventos de una variable aleatoria.
mu <- n * (20-18)/38

#El error estandar de una variable con dos posibles resultados es abs(b-a) * sqrt(p(1-p)).
#Multiplicado por sqrt(n) es el error estandar de eventos de una variable aleatoria.
se <- sqrt(n) * 2 * sqrt(90)/19

#Probabilidad de que el casino gane 0 o menos.
pnorm(0, mu, se)
