#Jugador 1
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







p1total <- p1one + p1two + p1three + p1four + p1five
p1total <- ifelse(p1total > 21 & p1[1] %in% aces, p1total-10,
                  (ifelse(p1total > 21 & p1[2] %in% aces, p1total-10,
                          (ifelse(p1total > 21 & p1[3] %in% aces, p1total-10,
                                  (ifelse(p1total > 21 & p1[4] %in% aces, p1total-10,
                                          (ifelse(p1total > 21 & p1[5] %in% aces, p1total-10,p1total)))))))))







p1total

#Jugador 2
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







p2total <- p2one + p2two + p2three + p2four + p2five
p2total <- ifelse(p2total > 21 & p2[1] %in% aces, p2total-10,
                  (ifelse(p2total > 21 & p2[2] %in% aces, p2total-10,
                          (ifelse(p2total > 21 & p2[3] %in% aces, p2total-10,
                                  (ifelse(p2total > 21 & p2[4] %in% aces, p2total-10,
                                          (ifelse(p2total > 21 & p2[5] %in% aces, p2total-10,p2total)))))))))







p2total

#Jugador 3
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







p3total <- p3one + p3two + p3three + p3four + p3five
p3total <- ifelse(p3total > 21 & p3[1] %in% aces, p3total-10,
                  (ifelse(p3total > 21 & p3[2] %in% aces, p3total-10,
                          (ifelse(p3total > 21 & p3[3] %in% aces, p3total-10,
                                  (ifelse(p3total > 21 & p3[4] %in% aces, p3total-10,
                                          (ifelse(p3total > 21 & p3[5] %in% aces, p3total-10,p3total)))))))))







p3total

#Dealer
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







dealertotal <- dealerone + dealertwo + dealerthree + dealerfour + dealerfive
dealertotal <- ifelse(dealertotal > 21 & dealer[1] %in% aces, dealertotal-10,
                      (ifelse(dealertotal > 21 & dealer[2] %in% aces, dealertotal-10,
                              (ifelse(dealertotal > 21 & dealer[3] %in% aces, dealertotal-10,
                                      (ifelse(dealertotal > 21 & dealer[4] %in% aces, dealertotal-10,
                                              (ifelse(dealertotal > 21 & dealer[5] %in% aces, dealertotal-10,dealertotal)))))))))







dealertotal

