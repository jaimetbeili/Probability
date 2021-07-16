#PARTE UNO

# Sacar una canica al azar de una bolsa con dos rojas y tres azules.
beads <- rep(c("red","blue"), times = c(2,3))
beads
sample(beads,1)

# Repetir el proceso anterior 10,000 veces para encontrar la probabilidad.
B <- 10000
events <- replicate(B, sample(beads,1))
tab <- table(events)
tab

# Repetir sin usar la funcion de replicate.
events <- sample(beads, B, replace = TRUE)
# Tabla con probabilidad en lugar de eventos.
prop.table(table(events))

set.seed(1986, sample.kind="Rounding")

# Otra forma de calcular probabilidad.
mean(beads == "blue")
mean(beads == "red")

#PARTE DOS

# joining strings with paste. Crea un tres de corazon.
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste.
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid. Todas las combinaciones de dos listas.
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

#Para crear un deck de cartas:
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

library(gtools)
#De 5 numeros (1:5), dame todas las permutaciones de grupos de 2.
permutations(5,2)

#De 10 numeros permuta en grupos de 7 con un vector de 0 a 9.
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
#Y dame 5 aleatorios
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

#Todas las manos de dos cartas posibles.
hands <- permutations(52,2, v = deck)
#Todas las posibilidades de la primera carta (51 por cada carta del deck).
first_card <- hands[,1]
#Todas las posibilidades de la segunda carta (51 por cada carta del deck).
second_card <- hands[,2]
#Todas las manos con un rey en la primera carta.
sum(first_card %in% kings)
#Todas las manos con un rey en la segunda dado que hubo un rey en la primera.
sum(first_card %in% kings & second_card %in% kings)
#Probabilidad de sacar dos reyes.
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

#La probabilidad de un 21 natural en blackjack:
#Primero definimos un vector con todos los aces.
aces <- paste("Ace", suits)
#Luego un vector con los que valen 10.
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

#Y de nuevo todas las manos posibles.
hands <- combinations(52, 2, v=deck) # all possible hands

#Probabilidad de sacar un as en la primera carta y un 10 en la segunda:
mean(hands[,1] %in% aces & hands[,2] %in% facecard)
#Cambiar el orden del as y el 10 no altera el resultado.
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

#Con una simulacion Monte Carlo llegamos al mismo resultado:
# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)     

#Cumpleanos repetido en un grupo de 50 personas:
#Duplicated nos dice cuando un termino ya aparecio en un vector.
#any(Duplicated(bdays)) nos dice si hay algun cumpleanos repetido.
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
duplicated(c(1,2,3,1,4,3,5))
any(duplicated(bdays))

# Monte Carlo simulation with B=10000 replicates to get the probability.
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)

#Crear una funcion que lo haga para el numero de personas que sea.
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
n <- seq(1, 60)

#Cuando tenemos un vector las operaciones aritmeticas se le aplican a todo:
n*2
#Pero los vectores no siempre pueden ser usados en funciones:
compute_prob(n)
#Para esos casos, podemos usar sapply, que aplica una funcion a todo el vector.
prob <- sapply(n, compute_prob)
prob
plot(n,prob)

#Los calculos anteriores son largos y estimados.
#function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

eprob <- sapply(n, exact_prob)

plot(n, prob)
lines(n, eprob, col = "red")


#Cuantas veces hay que repetir una simulacion para que sirva con Monte Carlo?
B <- 10^seq(1, 5, len = 1000)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

#PARTE TRES

#Regla de la suma indica que:
#PR(A or B) = PR(A)+PR(B)-PR(A and B)
#La probabilidad de un as y luego un facecard es 1/13*16/51.
#La probabilidad de un facecard y luego un as es 16/52*4/51.
#Estas dos sumadas nos dan la probabilidad de un 21 natural.
(1/13)*(16/51)+(16/52)*(4/51)

#Monty Hall Problem: Tres puertas y la posibilidad de cambiar con una simulacion Monte Carlo.
#Estrategia de quedarte con la misma puerta.
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  stick == prize_door
})
mean(stick)

#Estrategia de cambiar de puerta.
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  switch <- doors[!doors %in% c(my_pick,show)]
  switch == prize_door
})
mean(switch)
