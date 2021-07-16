#PROBABILIDAD CONTINUA (NO DISCRETA)

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
x

F <- function(a) mean(x <= a)
1 - F(70.5)    # probability of male taller than 70 inches

F(70)-F(50)

1 - pnorm(70.5, mean(x), sd(x))


# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

#Una base de datos que se ve igual a nuestra base de datos.
x <- heights %>% filter(sex == "Male") %>% .$height
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)
ds_theme_set()
data.frame(simulated_heights=simulated_heights) %>% ggplot(aes(simulated_heights)) +
  geom_histogram(col = "black", binwidth = 2)

#Probabilidad de que de 800 personas el mas alto mida mas de 7 pies.
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)
  max(simulated_data)
})
mean(tallest >= 7*12)

# R uses a convention that lets us remember the names of these functions.
# Namely, using the letters d for density, q
# for quantile, p for probability density function, and r for random.

#norm is the shorthand for normal distribution.
#t is the shorthand of student's t distribution.
#dt es la funcion de densidad para una distribucion t de student.

#La altura de una persona en el percentil 99 o en el 1 se calcula asi
qnorm(.99, avg, s)
qnorm(.01, avg, s)

# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)

# Create an object called `highestIQ` that contains the highest IQ score from each random distribution of 10,000 people.
highestIQ <- replicate(B, {
  grupo <- rnorm(10000, 100, 15)
  max(grupo)
})

# Make a histogram of the highest IQ scores.
hist(highestIQ)
