#Deck de cartas:
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

#Cada valor:
aces <- paste("Ace", suits)
deuces <- paste("Deuce", suits)
threes <- paste("Three", suits)
fours <- paste("Four", suits)
fives <- paste("Five", suits)
sixes <- paste("Six", suits)
sevens <- paste("Seven", suits)
eights <- paste("Eight", suits)
nines <- paste("Nine", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

#Reparte 2 a cada quien:
board <- sample(deck, 52, replace = FALSE)
p1 <- c(board[1],board[7])
p2 <- c(board[2],board[8])
p3 <- c(board[3],board[9])
dealer <- c(board[5],board[11])
x <- 13

#Se evalua la carta abierta del dealer:
dealerone <- ifelse(dealer[1] %in% aces, 11,
                    (ifelse(dealer[1] %in% deuces, 2,
                            (ifelse(dealer[1] %in% threes, 3,
                                    (ifelse(dealer[1] %in% fours, 4,
                                            (ifelse(dealer[1] %in% fives, 5,
                                                    (ifelse(dealer[1] %in% sixes, 6,
                                                            (ifelse(dealer[1] %in% sevens, 7,
                                                                    (ifelse(dealer[1] %in% eights, 8,
                                                                            (ifelse(dealer[1] %in% nines, 9,
                                                                                    (ifelse(dealer[1] %in% facecard, 10)))))))))))))))))))
dealer[1]
dealerone

#Turno del jugador 1:
p1

#Se evaluan sus dos cartas:
p1one <- ifelse(p1[1] %in% aces, 11,
                (ifelse(p1[1] %in% deuces, 2,
                        (ifelse(p1[1] %in% threes, 3,
                                (ifelse(p1[1] %in% fours, 4,
                                        (ifelse(p1[1] %in% fives, 5,
                                                (ifelse(p1[1] %in% sixes, 6,
                                                        (ifelse(p1[1] %in% sevens, 7,
                                                                (ifelse(p1[1] %in% eights, 8,
                                                                        (ifelse(p1[1] %in% nines, 9,
                                                                                (ifelse(p1[1] %in% facecard, 10)))))))))))))))))))
p1two <- ifelse(p1[2] %in% aces, 11,
                (ifelse(p1[2] %in% deuces, 2,
                        (ifelse(p1[2] %in% threes, 3,
                                (ifelse(p1[2] %in% fours, 4,
                                        (ifelse(p1[2] %in% fives, 5,
                                                (ifelse(p1[2] %in% sixes, 6,
                                                        (ifelse(p1[2] %in% sevens, 7,
                                                                (ifelse(p1[2] %in% eights, 8,
                                                                        (ifelse(p1[2] %in% nines, 9,
                                                                                (ifelse(p1[2] %in% facecard, 10)))))))))))))))))))

#Se suman, se evaluan los aces y se decide si pedir o no:
p1board <- p1one + p1two
p1board <- ifelse(p1board > 21 & p1[1] %in% aces, p1board-10,
                  (ifelse(p1board > 21 & p1[2] %in% aces, p1board-10, p1board)))
ifelse(p1board < dealerone + 10, print("Pide"), print("No pide"))
ifelse(p1board < dealerone + 10, p1 <- c(p1,board[x]),p1board)
x <- ifelse(p1board < dealerone + 10, x+1, x)

#Se evalua la posible tercera carta:
p1three <- ifelse(p1[3] %in% aces, 11,
                  (ifelse(p1[3] %in% deuces, 2,
                          (ifelse(p1[3] %in% threes, 3,
                                  (ifelse(p1[3] %in% fours, 4,
                                          (ifelse(p1[3] %in% fives, 5,
                                                  (ifelse(p1[3] %in% sixes, 6,
                                                          (ifelse(p1[3] %in% sevens, 7,
                                                                  (ifelse(p1[3] %in% eights, 8,
                                                                          (ifelse(p1[3] %in% nines, 9,
                                                                                  (ifelse(p1[3] %in% facecard, 10, 0)))))))))))))))))))
#Se imprime la nueva mano:
p1

#Se suman, se evaluan los aces y se decide si pedir o no:
p1.1 <- p1board + p1three
p1.1 <- ifelse(p1.1 > 21 & p1[1] %in% aces, p1.1-10,
                  (ifelse(p1.1 > 21 & p1[2] %in% aces, p1.1-10,
                          (ifelse(p1.1 > 21 & p1[3] %in% aces, p.1-10, p1.1)))))
ifelse(p1.1 < dealerone + 10, print("Pide"), print("No pide"))
ifelse(p1.1 < dealerone + 10, p1 <- c(p1,board[x]),p1.1)
x <- ifelse(p1.1 < dealerone + 10, x+1, x)

#Se evalua la posible cuarta carta:
p1four <- ifelse(p1[4] %in% aces, 11,
                 (ifelse(p1[4] %in% deuces, 2,
                         (ifelse(p1[4] %in% threes, 3,
                                 (ifelse(p1[4] %in% fours, 4,
                                         (ifelse(p1[4] %in% fives, 5,
                                                 (ifelse(p1[4] %in% sixes, 6,
                                                         (ifelse(p1[4] %in% sevens, 7,
                                                                 (ifelse(p1[4] %in% eights, 8,
                                                                         (ifelse(p1[4] %in% nines, 9,
                                                                                 (ifelse(p1[4] %in% facecard, 10, 0)))))))))))))))))))
#Se imprime la nueva mano:
p1

#Se suman, se evaluan los aces y se decide si pedir o no:
p1.2 <- p1.1 + p1four
p1.2 <- ifelse(p1.2 > 21 & p1[1] %in% aces, p1.2-10,
               (ifelse(p1.2 > 21 & p1[2] %in% aces, p1.2-10,
                       (ifelse(p1.2 > 21 & p1[3] %in% aces, p1.2-10, 
                               (ifelse(p1.2 > 21 & p1[4] %in% aces, p1.2-10, p1.2)))))))
ifelse(p1.2 < dealerone + 10, print("Pide"), print("No pide"))
ifelse(p1.2 < dealerone + 10, p1 <- c(p1,board[11]),p1.2)
x <- ifelse(p1.2 < dealerone + 10, x+1, x)

#Se evalua la posible quinta carta.
p1five <- ifelse(p1[5] %in% aces, 11,
                 (ifelse(p1[5] %in% deuces, 2,
                         (ifelse(p1[5] %in% threes, 3,
                                 (ifelse(p1[5] %in% fours, 4,
                                         (ifelse(p1[5] %in% fives, 5,
                                                 (ifelse(p1[5] %in% sixes, 6,
                                                         (ifelse(p1[5] %in% sevens, 7,
                                                                 (ifelse(p1[5] %in% eights, 8,
                                                                         (ifelse(p1[5] %in% nines, 9,
                                                                                 (ifelse(p1[5] %in% facecard, 10, 0)))))))))))))))))))
#Se crea el total y se evaluan los aces:
p1total <- p1one + p1two + p1three + p1four + p1five
p1total <- ifelse(p1total > 21 & p1[1] %in% aces, p1total-10,
                  (ifelse(p1total > 21 & p1[2] %in% aces, p1total-10,
                          (ifelse(p1total > 21 & p1[3] %in% aces, p1total-10,
                                  (ifelse(p1total > 21 & p1[4] %in% aces, p1total-10,
                                          (ifelse(p1total > 21 & p1[5] %in% aces, p1total-10,p1total)))))))))
#Se imprime la nueva mano y el total:
p1
p1total

#Turno del jugador 2:
p2

#Se evaluan sus dos cartas:
p2one <- ifelse(p2[1] %in% aces, 11,
                (ifelse(p2[1] %in% deuces, 2,
                        (ifelse(p2[1] %in% threes, 3,
                                (ifelse(p2[1] %in% fours, 4,
                                        (ifelse(p2[1] %in% fives, 5,
                                                (ifelse(p2[1] %in% sixes, 6,
                                                        (ifelse(p2[1] %in% sevens, 7,
                                                                (ifelse(p2[1] %in% eights, 8,
                                                                        (ifelse(p2[1] %in% nines, 9,
                                                                                (ifelse(p2[1] %in% facecard, 10)))))))))))))))))))
p2two <- ifelse(p2[2] %in% aces, 11,
                (ifelse(p2[2] %in% deuces, 2,
                        (ifelse(p2[2] %in% threes, 3,
                                (ifelse(p2[2] %in% fours, 4,
                                        (ifelse(p2[2] %in% fives, 5,
                                                (ifelse(p2[2] %in% sixes, 6,
                                                        (ifelse(p2[2] %in% sevens, 7,
                                                                (ifelse(p2[2] %in% eights, 8,
                                                                        (ifelse(p2[2] %in% nines, 9,
                                                                                (ifelse(p2[2] %in% facecard, 10)))))))))))))))))))

#Se suman, se evaluan los aces y se decide si pedir o no:
p2board <- p2one + p2two
p2board <- ifelse(p2board > 21 & p2[1] %in% aces, p2board-10,
                  (ifelse(p2board > 21 & p2[2] %in% aces, p2board-10, p2board)))
ifelse(p2board < dealerone + 10, print("Pide"), print("No pide"))
ifelse(p2board < dealerone + 10, p2 <- c(p2,board[12]),p2board)
x <- ifelse(p2board < dealerone + 10, x+1, x)

#Se evalua la posible tercera carta:
p2three <- ifelse(p2[3] %in% aces, 11,
                  (ifelse(p2[3] %in% deuces, 2,
                          (ifelse(p2[3] %in% threes, 3,
                                  (ifelse(p2[3] %in% fours, 4,
                                          (ifelse(p2[3] %in% fives, 5,
                                                  (ifelse(p2[3] %in% sixes, 6,
                                                          (ifelse(p2[3] %in% sevens, 7,
                                                                  (ifelse(p2[3] %in% eights, 8,
                                                                          (ifelse(p2[3] %in% nines, 9,
                                                                                  (ifelse(p2[3] %in% facecard, 10, 0)))))))))))))))))))
#Se imprime la nueva mano:
p2

#Se suman, se evaluan los aces y se decide si pedir o no:
p2.1 <- p2board + p2three
p2.1 <- ifelse(p2.1 > 21 & p2[1] %in% aces, p2.1-10,
               (ifelse(p2.1 > 21 & p2[2] %in% aces, p2.1-10,
                       (ifelse(p2.1 > 21 & p2[3] %in% aces, p.1-10, p2.1)))))
ifelse(p2.1 < dealerone + 10, print("Pide"), print("No pide"))
ifelse(p2.1 < dealerone + 10, p2 <- c(p2,board[13]),p2.1)
x <- ifelse(p2.1 < dealerone + 10, x+1, x)

#Se evalua la posible cuarta carta:
p2four <- ifelse(p2[4] %in% aces, 11,
                 (ifelse(p2[4] %in% deuces, 2,
                         (ifelse(p2[4] %in% threes, 3,
                                 (ifelse(p2[4] %in% fours, 4,
                                         (ifelse(p2[4] %in% fives, 5,
                                                 (ifelse(p2[4] %in% sixes, 6,
                                                         (ifelse(p2[4] %in% sevens, 7,
                                                                 (ifelse(p2[4] %in% eights, 8,
                                                                         (ifelse(p2[4] %in% nines, 9,
                                                                                 (ifelse(p2[4] %in% facecard, 10, 0)))))))))))))))))))
#Se imprime la nueva mano:
p2

#Se suman, se evaluan los aces y se decide si pedir o no:
p2.2 <- p2.1 + p2four
p2.2 <- ifelse(p2.2 > 21 & p2[1] %in% aces, p2.2-10,
               (ifelse(p2.2 > 21 & p2[2] %in% aces, p2.2-10,
                       (ifelse(p2.2 > 21 & p2[3] %in% aces, p2.2-10, 
                               (ifelse(p2.2 > 21 & p2[4] %in% aces, p2.2-10, p2.2)))))))
ifelse(p2.2 < dealerone + 10, print("Pide"), print("No pide"))
ifelse(p2.2 < dealerone + 10, p2 <- c(p2,board[14]),p2.2)
x <- ifelse(p2.2 < dealerone + 10, x+1, x)

#Se evalua la posible quinta carta.
p2five <- ifelse(p2[5] %in% aces, 11,
                 (ifelse(p2[5] %in% deuces, 2,
                         (ifelse(p2[5] %in% threes, 3,
                                 (ifelse(p2[5] %in% fours, 4,
                                         (ifelse(p2[5] %in% fives, 5,
                                                 (ifelse(p2[5] %in% sixes, 6,
                                                         (ifelse(p2[5] %in% sevens, 7,
                                                                 (ifelse(p2[5] %in% eights, 8,
                                                                         (ifelse(p2[5] %in% nines, 9,
                                                                                 (ifelse(p2[5] %in% facecard, 10, 0)))))))))))))))))))
#Se crea el total y se evaluan los aces:
p2total <- p2one + p2two + p2three + p2four + p2five
p2total <- ifelse(p2total > 21 & p2[1] %in% aces, p2total-10,
                  (ifelse(p2total > 21 & p2[2] %in% aces, p2total-10,
                          (ifelse(p2total > 21 & p2[3] %in% aces, p2total-10,
                                  (ifelse(p2total > 21 & p2[4] %in% aces, p2total-10,
                                          (ifelse(p2total > 21 & p2[5] %in% aces, p2total-10,p2total)))))))))
#Se imprime la nueva mano y el total:
p2
p2total

#Turno del jugador 3:
p3

#Se evaluan sus dos cartas:
p3one <- ifelse(p3[1] %in% aces, 11,
                (ifelse(p3[1] %in% deuces, 2,
                        (ifelse(p3[1] %in% threes, 3,
                                (ifelse(p3[1] %in% fours, 4,
                                        (ifelse(p3[1] %in% fives, 5,
                                                (ifelse(p3[1] %in% sixes, 6,
                                                        (ifelse(p3[1] %in% sevens, 7,
                                                                (ifelse(p3[1] %in% eights, 8,
                                                                        (ifelse(p3[1] %in% nines, 9,
                                                                                (ifelse(p3[1] %in% facecard, 10)))))))))))))))))))
p3two <- ifelse(p3[2] %in% aces, 11,
                (ifelse(p3[2] %in% deuces, 2,
                        (ifelse(p3[2] %in% threes, 3,
                                (ifelse(p3[2] %in% fours, 4,
                                        (ifelse(p3[2] %in% fives, 5,
                                                (ifelse(p3[2] %in% sixes, 6,
                                                        (ifelse(p3[2] %in% sevens, 7,
                                                                (ifelse(p3[2] %in% eights, 8,
                                                                        (ifelse(p3[2] %in% nines, 9,
                                                                                (ifelse(p3[2] %in% facecard, 10)))))))))))))))))))

#Se suman, se evaluan los aces y se decide si pedir o no:
p3board <- p3one + p3two
p3board <- ifelse(p3board > 21 & p3[1] %in% aces, p3board-10,
                  (ifelse(p3board > 21 & p3[2] %in% aces, p3board-10, p3board)))
ifelse(p3board < dealerone + 10, print("Pide"), print("No pide"))
ifelse(p3board < dealerone + 10, p3 <- c(p3,board[15]),p3board)
x <- ifelse(p3board < dealerone + 10, x+1, x)

#Se evalua la posible tercera carta:
p3three <- ifelse(p3[3] %in% aces, 11,
                  (ifelse(p3[3] %in% deuces, 2,
                          (ifelse(p3[3] %in% threes, 3,
                                  (ifelse(p3[3] %in% fours, 4,
                                          (ifelse(p3[3] %in% fives, 5,
                                                  (ifelse(p3[3] %in% sixes, 6,
                                                          (ifelse(p3[3] %in% sevens, 7,
                                                                  (ifelse(p3[3] %in% eights, 8,
                                                                          (ifelse(p3[3] %in% nines, 9,
                                                                                  (ifelse(p3[3] %in% facecard, 10, 0)))))))))))))))))))
#Se imprime la nueva mano:
p3

#Se suman, se evaluan los aces y se decide si pedir o no:
p3.1 <- p3board + p3three
p3.1 <- ifelse(p3.1 > 21 & p3[1] %in% aces, p3.1-10,
               (ifelse(p3.1 > 21 & p3[2] %in% aces, p3.1-10,
                       (ifelse(p3.1 > 21 & p3[3] %in% aces, p.1-10, p3.1)))))
ifelse(p3.1 < dealerone + 10, print("Pide"), print("No pide"))
ifelse(p3.1 < dealerone + 10, p3 <- c(p3,board[16]),p3.1)
x <- ifelse(p3.1 < dealerone + 10, x+1, x)

#Se evalua la posible cuarta carta:
p3four <- ifelse(p3[4] %in% aces, 11,
                 (ifelse(p3[4] %in% deuces, 2,
                         (ifelse(p3[4] %in% threes, 3,
                                 (ifelse(p3[4] %in% fours, 4,
                                         (ifelse(p3[4] %in% fives, 5,
                                                 (ifelse(p3[4] %in% sixes, 6,
                                                         (ifelse(p3[4] %in% sevens, 7,
                                                                 (ifelse(p3[4] %in% eights, 8,
                                                                         (ifelse(p3[4] %in% nines, 9,
                                                                                 (ifelse(p3[4] %in% facecard, 10, 0)))))))))))))))))))
#Se imprime la nueva mano:
p3

#Se suman, se evaluan los aces y se decide si pedir o no:
p3.2 <- p3.1 + p3four
p3.2 <- ifelse(p3.2 > 21 & p3[1] %in% aces, p3.2-10,
               (ifelse(p3.2 > 21 & p3[2] %in% aces, p3.2-10,
                       (ifelse(p3.2 > 21 & p3[3] %in% aces, p3.2-10, 
                               (ifelse(p3.2 > 21 & p3[4] %in% aces, p3.2-10, p3.2)))))))
ifelse(p3.2 < dealerone + 10, print("Pide"), print("No pide"))
ifelse(p3.2 < dealerone + 10, p3 <- c(p3,board[17]),p3.2)
x <- ifelse(p3.2 < dealerone + 10, x+1, x)

#Se evalua la posible quinta carta.
p3five <- ifelse(p3[5] %in% aces, 11,
                 (ifelse(p3[5] %in% deuces, 2,
                         (ifelse(p3[5] %in% threes, 3,
                                 (ifelse(p3[5] %in% fours, 4,
                                         (ifelse(p3[5] %in% fives, 5,
                                                 (ifelse(p3[5] %in% sixes, 6,
                                                         (ifelse(p3[5] %in% sevens, 7,
                                                                 (ifelse(p3[5] %in% eights, 8,
                                                                         (ifelse(p3[5] %in% nines, 9,
                                                                                 (ifelse(p3[5] %in% facecard, 10, 0)))))))))))))))))))
#Se crea el total y se evaluan los aces:
p3total <- p3one + p3two + p3three + p3four + p3five
p3total <- ifelse(p3total > 21 & p3[1] %in% aces, p3total-10,
                  (ifelse(p3total > 21 & p3[2] %in% aces, p3total-10,
                          (ifelse(p3total > 21 & p3[3] %in% aces, p3total-10,
                                  (ifelse(p3total > 21 & p3[4] %in% aces, p3total-10,
                                          (ifelse(p3total > 21 & p3[5] %in% aces, p3total-10,p3total)))))))))
#Se imprime la nueva mano y el total:
p3
p3total

#Turno del dealer:
dealer

#Se evalua su segunda carta:
dealertwo <- ifelse(dealer[2] %in% aces, 11,
                    (ifelse(dealer[2] %in% deuces, 2,
                            (ifelse(dealer[2] %in% threes, 3,
                                    (ifelse(dealer[2] %in% fours, 4,
                                            (ifelse(dealer[2] %in% fives, 5,
                                                    (ifelse(dealer[2] %in% sixes, 6,
                                                            (ifelse(dealer[2] %in% sevens, 7,
                                                                    (ifelse(dealer[2] %in% eights, 8,
                                                                            (ifelse(dealer[2] %in% nines, 9,
                                                                                    (ifelse(dealer[2] %in% facecard, 10)))))))))))))))))))
#Se suman, se evaluan los aces y se decide si pedir o no:
dealerboard <- dealerone + dealertwo
dealerboard
dealerboard <- ifelse(dealerboard > 21 & dealer[1] %in% aces, dealerboard-10,
                      (ifelse(dealerboard > 21 & dealer[2] %in% aces, dealerboard-10, dealerboard)))
ifelse(dealerboard > 16, print("No pide"),
       (ifelse(dealerboard > p1total & dealerboard > p2total & dealerboard > p3total, print("No pide"),
               print("Pide"))))
ifelse(dealerboard > 16, dealerboard,
       (ifelse(dealerboard > p1total & dealerboard > p2total & dealerboard > p3total, dealerboard,
               dealer <- c(dealer,board[x]))))
x <- ifelse(dealerboard > 16, x,
       (ifelse(dealerboard > p1total & dealerboard > p2total & dealerboard > p3total, x,
               x+1)))

#Se evalua la posible tercera carta:
dealerthree <- ifelse(dealer[3] %in% aces, 11,
                      (ifelse(dealer[3] %in% deuces, 2,
                              (ifelse(dealer[3] %in% threes, 3,
                                      (ifelse(dealer[3] %in% fours, 4,
                                              (ifelse(dealer[3] %in% fives, 5,
                                                      (ifelse(dealer[3] %in% sixes, 6,
                                                              (ifelse(dealer[3] %in% sevens, 7,
                                                                      (ifelse(dealer[3] %in% eights, 8,
                                                                              (ifelse(dealer[3] %in% nines, 9,
                                                                                      (ifelse(dealer[3] %in% facecard, 10, 0)))))))))))))))))))
#Se imprime la nueva mano:
dealer

#Se suman, se evaluan los aces y se decide si pedir o no:
dealer.1 <- dealerboard + dealerthree
dealer.1 <- ifelse(dealer.1 > 21 & dealer[1] %in% aces, dealer.1-10,
                   (ifelse(dealer.1 > 21 & dealer[2] %in% aces, dealer.1-10,
                           (ifelse(dealer.1 > 21 & dealer[3] %in% aces, p.1-10, dealer.1)))))
ifelse(dealer.1 > 16, print("No pide"),
       (ifelse(dealer.1 > p1total & dealer.1 > p2total & dealer.1 > p3total, print("No pide"),
               print("Pide"))))
ifelse(dealer.1 > 16, dealer.1,
       (ifelse(dealer.1 > p1total & dealer.1 > p2total & dealer.1 > p3total, dealer.1,
               dealer <- c(dealer,board[19]))))
x <- ifelse(dealer.1 > 16, x,
       (ifelse(dealer.1 > p1total & dealer.1 > p2total & dealer.1 > p3total, x,
               x+1)))

#Se evalua la posible cuarta carta:
dealerfour <- ifelse(dealer[4] %in% aces, 11,
                     (ifelse(dealer[4] %in% deuces, 2,
                             (ifelse(dealer[4] %in% threes, 3,
                                     (ifelse(dealer[4] %in% fours, 4,
                                             (ifelse(dealer[4] %in% fives, 5,
                                                     (ifelse(dealer[4] %in% sixes, 6,
                                                             (ifelse(dealer[4] %in% sevens, 7,
                                                                     (ifelse(dealer[4] %in% eights, 8,
                                                                             (ifelse(dealer[4] %in% nines, 9,
                                                                                     (ifelse(dealer[4] %in% facecard, 10, 0)))))))))))))))))))

#Se imprime la nueva mano
dealer

#Se suman, se evaluan los aces y se decide si pedir o no:
dealer.2 <- dealer.1 + dealerfour
dealer.2 <- ifelse(dealer.2 > 21 & dealer[1] %in% aces, dealer.2-10,
                   (ifelse(dealer.2 > 21 & dealer[2] %in% aces, dealer.2-10,
                           (ifelse(dealer.2 > 21 & dealer[3] %in% aces, dealer.2-10, 
                                   (ifelse(dealer.2 > 21 & dealer[4] %in% aces, dealer.2-10, dealer.2)))))))
ifelse(dealer.2 > 16 & dealer.2 < 22, print("No pide"),
       (ifelse(dealer.2 > p1total & dealer.2 > p2total & dealer.2 > p3total, print("No pide"),
               print("Pide"))))
ifelse(dealer.2 > 16 & dealer.2 < 22, dealer.2,
       (ifelse(dealer.2 > p1total & dealer.2 > p2total & dealer.2 > p3total, dealer.2,
               dealer <- c(dealer,board[20]))))

#Se evalua la posible quinta carta:
dealerfive <- ifelse(dealer[5] %in% aces, 11,
                     (ifelse(dealer[5] %in% deuces, 2,
                             (ifelse(dealer[5] %in% threes, 3,
                                     (ifelse(dealer[5] %in% fours, 4,
                                             (ifelse(dealer[5] %in% fives, 5,
                                                     (ifelse(dealer[5] %in% sixes, 6,
                                                             (ifelse(dealer[5] %in% sevens, 7,
                                                                     (ifelse(dealer[5] %in% eights, 8,
                                                                             (ifelse(dealer[5] %in% nines, 9,
                                                                                     (ifelse(dealer[5] %in% facecard, 10, 0)))))))))))))))))))
#Se imprime la nueva mano
dealer

#Se crea el total y se evaluan los aces:
dealertotal <- dealerone + dealertwo + dealerthree + dealerfour + dealerfive
dealertotal <- ifelse(dealertotal > 21 & dealer[1] %in% aces, dealertotal-10,
                      (ifelse(dealertotal > 21 & dealer[2] %in% aces, dealertotal-10,
                              (ifelse(dealertotal > 21 & dealer[3] %in% aces, dealertotal-10,
                                      (ifelse(dealertotal > 21 & dealer[4] %in% aces, dealertotal-10,
                                              (ifelse(dealertotal > 21 & dealer[5] %in% aces, dealertotal-10,dealertotal)))))))))
#Se imprime la nueva mano y el total:
dealer
dealertotal

p1
p1total
p2
p2total
p3
p3total
dealer
dealertotal

#Declarar Ganadores
ifelse(p1total > 21, "Jugador 1 se voló",
       (ifelse(p1total < dealertotal & dealertotal < 22,
               print("Jugador 1 pierde"),
               (ifelse(p1total == dealertotal,
                       print("Jugador 1 empata"),
                       print("Jugador 1 gana"))))))

ifelse(p2total > 21, "Jugador 2 se voló",
       (ifelse(p2total < dealertotal & dealertotal < 22,
               print("Jugador 2 pierde"),
               (ifelse(p2total == dealertotal,
                       print("Jugador 2 empata"),
                       print("Jugador 2 gana"))))))

ifelse(p3total > 21, "Jugador 3 se voló",
       (ifelse(p3total < dealertotal & dealertotal < 22,
               print("Jugador 3 pierde"),
               (ifelse(p3total == dealertotal,
                       print("Jugador 3 empata"),
                       print("Jugador 3 gana"))))))

