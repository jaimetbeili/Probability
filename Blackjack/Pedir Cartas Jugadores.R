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
ifelse(p1board < dealerone + 10, x <- x+1, x)

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
ifelse(p1.1 < dealerone + 10, x <- x+1, x)

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
p1.2 <- p1board + p1four
p1.2 <- ifelse(p1.2 > 21 & p1[1] %in% aces, p1.2-10,
               (ifelse(p1.2 > 21 & p1[2] %in% aces, p1.2-10,
                       (ifelse(p1.2 > 21 & p1[3] %in% aces, p1.2-10, 
                               (ifelse(p1.2 > 21 & p1[4] %in% aces, p1.2-10, p1.2)))))))
ifelse(p1.2 < dealerone + 10, print("Pide"), print("No pide"))
ifelse(p1.2 < dealerone + 10, p1 <- c(p1,board[11]),p1.2)
ifelse(p1.2 < dealerone + 10, x <- x+1, x)

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