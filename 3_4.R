#Un banco con 2% de morosidad
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
#0 pierden, 1 ganan
defaults <- sample(c(0,1), n, prob = c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

B <- 10000
losses <- replicate(B, {
  defaults <- sample(c(0,1), n, prob = c(1-p, p), replace = TRUE)
  sum(defaults * loss_per_foreclosure)
})

library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, color = "black")

#Tambien lo podemos hacer con un CLT en lugar de Monte Carlo
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

#Esto es lo que perderiamos en promedio sin una tasa de interes. Ahora, para cualcular la tasa
#sustituimos en la formula del valor esperado que vimos en el pasado.
t <- -loss_per_foreclosure*p/(1-p)
#Sabemos que los prestamos son de 180000, por lo que la tasa seria de aproximadamente 2%:
t/180000

#pero como esa tasa de interes es igual a nuestra morosidad hay un 50% de probabilidad de que el banco pierda.
#podemos buscar la probabilidad de solo perder en el 1% de los casos con un qnorm(0.01) que llamaremos z
l <- loss_per_foreclosure
z <- qnorm(0.01)
#los sustituimos en esta formula
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

#Con estos datos corremos una Monte Carlo y nos dan los mismos resultados
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

#Un empleado te dice que con mayor numero de prestamos puedes ganar mas, incluso si subes la tasa de interes y aumenta la morosidad promedio.
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

#Incluso podemos calcular el numero de prestamos necesario para mantener la probabilidad de perder en 1%.
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

#Monte Carlo esta de acuerdo
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

#El problema es que todo esto asume que la probabilidad de morosidad es independiente e ignora que uno puede afectar a otro.
#Cuando abrimos la posibilidad de que todos dejen de pagar al mismo tiempo, esto cambia
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million
