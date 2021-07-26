#Primera carta
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
ifelse(dealerboard > 16, x,
       (ifelse(dealerboard > p1total & dealerboard > p2total & dealerboard > p3total, x,
               x <- x+1)))

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
ifelse(dealer.1 > 16, x,
       (ifelse(dealer.1 > p1total & dealer.1 > p2total & dealer.1 > p3total, x,
               x <- x+1)))

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