import Data.List
import Data.Maybe
import System.Random (randomRIO)

-- Tipos de datos
data TablutPlayer = ShieldPlayer | SwordPlayer deriving (Eq, Show, Enum)
data Direccion = Vertical | Horizontal deriving (Eq, Show)
data TablutAction = Mover Direccion Coord2D Int deriving (Eq)
data Casilla = Vacia | PeonEspada | PeonEscudo | Rey deriving (Eq)

type Fila = (Casilla, Casilla, Casilla, Casilla, Casilla, Casilla, Casilla, Casilla, Casilla)
type Tablero = (Fila, Fila, Fila, Fila, Fila, Fila, Fila, Fila, Fila)
type Coord2D = (Int, Int)
data TablutGame = TablutGame TablutPlayer Tablero Int


-- |Permite el acceso por índice a los elementos de una 9-upla
getNUpla :: (a, a, a, a, a, a, a, a, a) -> Int -> Maybe a
getNUpla (a, _, _, _, _, _, _, _, _) 0 = Just a
getNUpla (_, a, _, _, _, _, _, _, _) 1 = Just a
getNUpla (_, _, a, _, _, _, _, _, _) 2 = Just a
getNUpla (_, _, _, a, _, _, _, _, _) 3 = Just a
getNUpla (_, _, _, _, a, _, _, _, _) 4 = Just a
getNUpla (_, _, _, _, _, a, _, _, _) 5 = Just a
getNUpla (_, _, _, _, _, _, a, _, _) 6 = Just a
getNUpla (_, _, _, _, _, _, _, a, _) 7 = Just a
getNUpla (_, _, _, _, _, _, _, _, a) 8 = Just a
getNUpla _ _= Nothing

-- |Permite cambiar por índice el valor a un elemento de una 9-upla
setNUpla :: (a, a, a, a, a, a, a, a, a) -> Int -> a -> Maybe (a, a, a, a, a, a, a, a, a)
setNUpla (_, e1, e2, e3, e4, e5, e6, e7, e8) 0 e' = Just (e', e1, e2, e3, e4, e5, e6, e7, e8)
setNUpla (e0, _, e2, e3, e4, e5, e6, e7, e8) 1 e' = Just (e0, e', e2, e3, e4, e5, e6, e7, e8)
setNUpla (e0, e1, _, e3, e4, e5, e6, e7, e8) 2 e' = Just (e0, e1, e', e3, e4, e5, e6, e7, e8)
setNUpla (e0, e1, e2, _, e4, e5, e6, e7, e8) 3 e' = Just (e0, e1, e2, e', e4, e5, e6, e7, e8)
setNUpla (e0, e1, e2, e3, _, e5, e6, e7, e8) 4 e' = Just (e0, e1, e2, e3, e', e5, e6, e7, e8)
setNUpla (e0, e1, e2, e3, e4, _, e6, e7, e8) 5 e' = Just (e0, e1, e2, e3, e4, e', e6, e7, e8)
setNUpla (e0, e1, e2, e3, e4, e5, _, e7, e8) 6 e' = Just (e0, e1, e2, e3, e4, e5, e', e7, e8)
setNUpla (e0, e1, e2, e3, e4, e5, e6, _, e8) 7 e' = Just (e0, e1, e2, e3, e4, e5, e6, e', e8)
setNUpla (e0, e1, e2, e3, e4, e5, e6, e7, _) 8 e' = Just (e0, e1, e2, e3, e4, e5, e6, e7, e')
setNUpla _ _ _ = Nothing


-- Funciones de acceso al tablero según coordenadas fila/columna
{- El operador <- es una "asignación" `Maybe a` en una binding `a`
 -   Si es `Nothing` se termina la función y se devuelve Nothing
 -   Si es `Just a` el nombre toma el valor contenido en él -}
-- |Permite acceder al valor de una casilla del tablero dados los índices de fila-columna
getCasilla :: Tablero -> Int -> Int -> Maybe Casilla
getCasilla tablero i j = do
    fila <- getNUpla tablero i
    getNUpla fila j

-- |Permite cambiar el valor de una casilla del tablero dados los índices fila-columna
setCasilla :: Tablero -> Int -> Int -> Casilla -> Maybe Tablero
setCasilla tablero i j cas = do
    fila <- getNUpla tablero i -- La fila de indice i (0 a 8)
    fila' <- setNUpla fila j cas -- Arma una nueva fila con la nueva casilla
    setNUpla tablero i fila' -- Cola la nueva fila en el tablero


-- Funciones asistentes
-- |La lista de piezas pertenecientes a cada jugador.
piezas :: TablutPlayer -> [Casilla]
piezas SwordPlayer = [PeonEspada]
piezas ShieldPlayer = [Rey, PeonEscudo]

-- |Verifica si la ficha en una casilla (determinada por fila-columna) es del jugador dado
esDeJugador :: Tablero -> Coord2D -> TablutPlayer -> Bool
esDeJugador t (i,j) p =
    case getCasilla t i j
    of Nothing -> False
       Just a -> a `elem` piezas p

-- |Calcula las nuevas coordenadas que da una acción
coordsPostAction :: TablutAction -> Coord2D
coordsPostAction (Mover Horizontal (i, j) d) = (i, j+d)
coordsPostAction (Mover Vertical (i, j) d) = (i+d, j)

-- |El jugador oponente al dado
oponente :: TablutPlayer -> TablutPlayer
oponente ShieldPlayer = SwordPlayer
oponente SwordPlayer = ShieldPlayer

-- |Computa los movimientos posibles para cada jugador
actions :: TablutGame -> [(TablutPlayer, [TablutAction])]
actions (TablutGame ShieldPlayer t _) =
    [(SwordPlayer, []) , (ShieldPlayer, acciones t ShieldPlayer)]
actions (TablutGame SwordPlayer t _) =
    [(ShieldPlayer, []) , (SwordPlayer, acciones t SwordPlayer)]

-- |Computa los movimientos posibles para un jugador determinado
acciones :: Tablero -> TablutPlayer -> [TablutAction]
acciones t p = do
    i <- [0..8]
    j <- [0..8]
    if esDeJugador t (i, j) p
        then movArriba i j ++ movAbajo i j ++ movIzq i j ++ movDer i j
        else []
    where
        movArriba i j = takeWhile llevaAVacia $ map (Mover Vertical (i, j)) [1..]
        movAbajo i j = takeWhile llevaAVacia $ map (Mover Vertical (i, j)) [(-1),(-2)..]
        movDer i j = takeWhile llevaAVacia $ map (Mover Horizontal (i, j)) [1..]
        movIzq i j = takeWhile llevaAVacia $ map (Mover Horizontal (i, j)) [(-1),(-2)..]

        llevaAVacia act =
            let (fila, col) = coordsPostAction act
            in getCasilla t fila col == Just Vacia

-- |Aplica una acción al tablero dando el nuevo estado de juego
next :: TablutGame -> (TablutPlayer, TablutAction) -> TablutGame
next (TablutGame ShieldPlayer _ nj) (SwordPlayer, _) = error "Este no es el jugador activo"
next (TablutGame SwordPlayer _ nj) (ShieldPlayer, _) = error "Este no es el jugador activo"
next (TablutGame p t nj) (_, m@(Mover _ coord _)) =
    if esDeJugador t coord p
        then
            let t' = realizarMov m t
                coord' = coordsPostAction m
            in TablutGame (oponente p) (quitarCapturados t' coord' p) (nj+1)
        else error "No tienes una ficha en esa posición"
{-|
  Modifica al tablero quitando las piezas capturadas.
  Toma la posición de la última pieza movida ya que solo ella puede estar 'atacando'
-}
quitarCapturados :: Tablero -> Coord2D -> TablutPlayer -> Tablero
quitarCapturados t (i, j) p = foldl capturar t pos
    where
        pos = [ ((i+1, j), (i+2, j))
              , ((i-1, j), (i-2, j))
              , ((i, j+1), (i, j+2))
              , ((i, j-1), (i, j-2)) ]
        peon SwordPlayer = PeonEspada
        peon ShieldPlayer = PeonEscudo
        capturar t ((i, j), (i', j')) = fromMaybe t $ do
            victima <- getCasilla t i j
            ally <- getCasilla t i' j'
            if victima == peon (oponente p) && ally == peon p
                then setCasilla t i j Vacia
                else Nothing

-- |Realiza un movimiento en el tablero, aplicando la acción. Da error si el movimiento es imposible (fuera de rango)
realizarMov :: TablutAction -> Tablero -> Tablero
realizarMov (Mover Horizontal (i, j) d) t = fromJust $ do
    c <- getCasilla t i j
    t' <- setCasilla t i j Vacia
    setCasilla t' i (j+d) c
realizarMov (Mover Vertical (i, j) d) t = fromJust $ do
    c <- getCasilla t i j
    t' <- setCasilla t i j Vacia
    setCasilla t' (i+d) j c


{-|
  Si el juego termió retorna el resultado de juego para cada jugador.
  1 si el jugador ganó, -1 si perdió y 0 si se empató
  Si el juego no está terminado, se debe retornar una lista vacía.
-}
result :: TablutGame -> [(TablutPlayer, Int)]
result (TablutGame _ t nj)
    | nj > 100 = [(ShieldPlayer, 1), (SwordPlayer, 1)] -- Si se realizaron más de 100 jugadas es un empate
    | otherwise =
        let (i, j) = buscarRey t -- i,j son las coordenadas del Rey (en este momento).
        in if (i `elem` [0, 8]) || (j `elem` [0, 8]) --Si está en cualquier borde, el shieldPlayer gana
            then [(ShieldPlayer, 1), (SwordPlayer, -1)]
            else if reyRodeado t (i, j)
                then [(ShieldPlayer, -1), (SwordPlayer, 1)]
                else []

-- |Busca al rey en el tablero y devuelve su posición.
buscarRey :: Tablero -> Coord2D
buscarRey t = head $ do
    i <- [0..8]
    j <- [0..8]
    case getCasilla t i j
        of Just Rey -> return (i, j)
           _ -> []

-- |La función dado el tablero y la posición del rey, devuelve true si el Rey esta rodeado por arriba, abajo, izquierda y
-- derecha por peones espada.
reyRodeado :: Tablero -> Coord2D -> Bool
reyRodeado t (i, j) = fromMaybe False $ do
    w <- getCasilla t i (j - 1)
    a <- getCasilla t (i-1) j
    s <- getCasilla t i (j + 1)
    d <- getCasilla t (i+1) j
    return ((w, a, s, d) == (PeonEspada, PeonEspada, PeonEspada, PeonEspada))


-- | La funcion dado un tablero, devuelve el jugador activo.
activePlayer :: TablutGame -> Maybe TablutPlayer
activePlayer g@(TablutGame p _ _) = if null (result g) then Just p else Nothing


-- |El tablero inicial del juego.
beginning :: TablutGame
beginning = TablutGame ShieldPlayer t 0 -- (f1, f2, f3, f4, f5, f6, f7, f8, f9)
    where
        t = ((PeonEspada, Vacia, Vacia, PeonEspada, PeonEspada, PeonEspada, Vacia, Vacia, PeonEspada)
            ,(Vacia, Vacia, Vacia, Vacia, PeonEspada, Vacia, Vacia, Vacia, Vacia)
            ,(Vacia, Vacia, Vacia, Vacia, PeonEscudo, Vacia, Vacia, Vacia, Vacia)
            ,(PeonEspada, Vacia, Vacia, Vacia, PeonEscudo, Vacia, Vacia, Vacia, PeonEspada)
            ,(PeonEspada, PeonEspada, PeonEscudo, PeonEscudo, Rey, PeonEscudo, PeonEscudo, PeonEspada, PeonEspada)
            ,(PeonEspada, Vacia, Vacia, Vacia, PeonEscudo, Vacia, Vacia, Vacia, PeonEspada)
            ,(Vacia, Vacia, Vacia, Vacia, PeonEscudo, Vacia, Vacia, Vacia, Vacia)
            ,(Vacia, Vacia, Vacia, Vacia, PeonEspada, Vacia, Vacia, Vacia, Vacia)
            ,(PeonEspada, Vacia, Vacia, PeonEspada, PeonEspada, PeonEspada, Vacia, Vacia, PeonEspada))


-- Ø Shield ± Sword  ¥ King
instance Show Casilla where
    show Vacia = " "
    show PeonEspada = "±"
    show Rey = "¥"
    show PeonEscudo = "Ø"

instance Show TablutGame where
    show = showBoard

-- |Representa el estado de juego, tablero y jugador actual.
showBoard :: TablutGame -> String
showBoard (TablutGame j t nj) = show j ++ " (se hicieron " ++ show nj ++ " jugadas)" ++ showTablero t

-- |Representa el tablero, con sus fichas y coordenadas
showTablero :: Tablero -> String
showTablero (f1, f2, f3, f4, f5, f6, f7, f8, f9) =
    "\n\\ A B C D E F G H I\n" ++ intercalate "\n" (zipWith (\f n -> show n ++ showFila f) fls [0..8])
    -- intercalate es: concat intersperse
    -- intersperse intercala x con cada elemento sucesivo de la lista
    where fls = [f1, f2, f3, f4, f5, f6, f7, f8, f9]
          showFila (a, b, c, d, e, f, g, h, i) =
            '|':show a ++ '|':show b ++ '|':show c ++ '|':show d ++ '|':show e ++ '|':show f ++ '|':show g ++ '|':show h ++ '|':show i++"|"

instance Show TablutAction where
    show (Mover dd (i, j) d) = algebraica i j ++ "/" ++
        case dd of Vertical -> algebraica (i + d) j
                   Horizontal -> algebraica i (j + d)
        where algebraica i j = letra j:show i
              letra i = "abcdefghi" !! i

-- |Interpreta la representación de un movimiento como una acción de juego
readAction :: String -> TablutAction
readAction l = Mover dir (i, j) dd
    where (fro, _:to) = span (/= '/') l -- parte la representación fro/to
          (i, j) = readAlgebraica fro
          (i', j') = readAlgebraica to
          readAlgebraica (c:n) = (read n, fromJust $ elemIndex c "abcdefghi")
          dir = if j' /= j then Horizontal else Vertical
          dd = case dir of Horizontal -> j' - j
                           Vertical -> i' - i


-- DE LA CATEDRA
type TablutAgent = TablutGame -> IO (Maybe TablutAction)

{- La función ´runMatch´ corre la partida completa a partir del estado de juego dado, usando los dos
agentes dados. Retorna una tupla con los puntajes (score) finales del juego. -}

runMatch :: (TablutAgent, TablutAgent) -> TablutGame -> IO [(TablutPlayer, Int)]
runMatch ags@(ag1, ag2) g = do
   putStrLn (showBoard g)
   case activePlayer g of
      Nothing -> return $ result g
      Just p -> do
         let ag = [ag1, ag2] !! fromJust ( elemIndex p [ShieldPlayer, SwordPlayer])
         move <- ag g
         runMatch ags (next g (p, fromJust move))

{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.
-}
runOnConsole :: IO [(TablutPlayer, Int)]
runOnConsole = runMatch (consoleAgent ShieldPlayer, consoleAgent SwordPlayer) beginning

{- El agente de consola ´consoleAgent´ muestra el estado de juego y los movimientos disponibles por
consola, y espera una acción por entrada de texto.
-}
consoleAgent :: TablutPlayer -> TablutAgent
consoleAgent player state = do
   let moves = fromJust $ lookup player (actions state)
   if null moves then do
      putStrLn "No moves!"
      getLine
      return Nothing
   else do
      putStrLn ("Select one move:" ++ concat [" "++ show m | m <- moves])
      line <- getLine
      let input = readAction line
      if input `elem` moves then return (Just input) else do
         putStrLn "Invalid move!"
         consoleAgent player state

main = runOnConsole
