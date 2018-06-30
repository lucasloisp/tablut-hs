
casilla :: Tablero -> (Int, Int) -> Casilla
casilla (f1, f2, f3, f4, f5, f6, f7, f8, f9) (x, y) = 
    indiceFila y (
        case x of 0 -> f1 -- va del (0,0) al (8,8) como un sistema de coordenadas estándar, switch de java
                  1 -> f2
                  2 -> f3
                  3 -> f4
                  4 -> f5
                  5 -> f6
                  6 -> f7
                  7 -> f8
                  8 -> f9
                  _ -> (Vacia, Vacia, Vacia, Vacia, Vacia, Vacia, Vacia, Vacia, Vacia))
        where
            indiceFila 0 (a, _, _, _, _, _, _, _, _) = a
            indiceFila 1 (_, a, _, _, _, _, _, _, _) = a
            indiceFila 2 (_, _, a, _, _, _, _, _, _) = a
            indiceFila 3 (_, _, _, a, _, _, _, _, _) = a
            indiceFila 4 (_, _, _, _, a, _, _, _, _) = a
            indiceFila 5 (_, _, _, _, _, a, _, _, _) = a
            indiceFila 6 (_, _, _, _, _, _, a, _, _) = a
            indiceFila 7 (_, _, _, _, _, _, _, a, _) = a
            indiceFila 8 (_, _, _, _, _, _, _, _, a) = a
