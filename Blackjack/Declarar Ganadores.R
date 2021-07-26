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

