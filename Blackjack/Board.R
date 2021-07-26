#Deck de cartas:
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

#Manos:
#Todas las manos de dos cartas posibles.
hands <- permutations(52,2, v = deck)
#Todas las posibilidades de la primera carta.
first_card <- hands[,1]
#Todas las posibilidades de la segunda carta.
second_card <- hands[,2]

#Cada valor:
#Aces.
aces <- paste("Ace", suits)
deuces <- paste("Deuce", suits)
threes <- paste("Three", suits)
fours <- paste("Four", suits)
fives <- paste("Five", suits)
sixes <- paste("Six", suits)
sevens <- paste("Seven", suits)
eights <- paste("Eight", suits)
nines <- paste("Nine", suits)
#Facecards.
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

board <- sample(deck, 52, replace = FALSE)
p1 <- c(board[1],board[5])
p2 <- c(board[2],board[6])
p3 <- c(board[3],board[7])
dealer <- c(board[4],board[8])
x <- 9

p1
p2
p3

dealer[1]
