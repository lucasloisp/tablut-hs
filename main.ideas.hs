import Data.List
import Data.Maybe
import System.Random (randomRIO)

-- Tipos de datos
data TablutPlayer = ShieldPlayer | SwordPlayer deriving (Eq, Show, Enum)
data Direccion = Vertical | Horizontal deriving (Eq, Show)
data TablutAction = Mover Direccion (Int, Int) Int deriving (Eq)
data Casilla = Vacia | PeonEspada | PeonEscudo | Rey deriving (Eq)

type Fila = (Casilla, Casilla, Casilla, Casilla, Casilla, Casilla, Casilla, Casilla, Casilla)
type Tablero = (Fila, Fila, Fila, Fila, Fila, Fila, Fila, Fila, Fila)
data TablutGame = TablutGame TablutPlayer Tablero

tablero :: TablutGame -> Tablero
tablero (TablutGame _ t) = t


-- Funciones asistenes para trabajar con tuplas de 9 elementos
getNUpla :: (a, a, a, a, a, a, a, a, a) -> Int -> Maybe a
getNUpla (a, _, _, _, _, _, _, _, _) 0= Just a
getNUpla (_, a, _, _, _, _, _, _, _) 1= Just a
getNUpla (_, _, a, _, _, _, _, _, _) 2= Just a
getNUpla (_, _, _, a, _, _, _, _, _) 3= Just a
getNUpla (_, _, _, _, a, _, _, _, _) 4= Just a
getNUpla (_, _, _, _, _, a, _, _, _) 5= Just a
getNUpla (_, _, _, _, _, _, a, _, _) 6= Just a
getNUpla (_, _, _, _, _, _, _, a, _) 7= Just a
getNUpla (_, _, _, _, _, _, _, _, a) 8= Just a
getNUpla _ _= Nothing 

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


-- Funciones de acceso al tablero, abrstracción
{-
    La flecha <- es una "asignación" de Maybe a en una "variable" a
        Si es `Nothing` se termina la función y se devuelve nothing
        Si es `Just a` la "variable" toma el valor a SIN EL JUST
-}
getDeTablero :: Tablero -> Int -> Int -> Maybe Casilla
getDeTablero tablero i j = do
    filaActual <- getNUpla tablero i
    getNUpla filaActual j

setEnTablero :: Tablero -> Int -> Int -> Casilla -> Maybe Tablero
setEnTablero tablero i j cas = do
    filaActual <- getNUpla tablero i -- La fila de indice i (0 a 8) de tableroshow 
    nuevaFila <- setNUpla filaActual j cas -- Arma una nueva fila con la nueva casilla
    setNUpla tablero i nuevaFila -- Cola la nueva fila en el tablero


-- Funciones asistentes
piezas :: TablutPlayer -> [Casilla]
piezas SwordPlayer = [PeonEspada]
piezas ShieldPlayer = [Rey, PeonEscudo]

jugadorEs :: Tablero -> (Int, Int) -> TablutPlayer -> Bool
jugadorEs t (i,j) p = 
    case getDeTablero t i j 
    of Nothing -> False
       Just a -> a `elem` piezas p

-- Computa los movimientos posibles       
actions :: TablutGame -> [(TablutPlayer, [TablutAction])]
actions (TablutGame ShieldPlayer t) = 
    [(SwordPlayer, []) , (ShieldPlayer, acciones t ShieldPlayer)]
actions (TablutGame SwordPlayer t) = 
    [(ShieldPlayer, []) , (SwordPlayer, acciones t SwordPlayer)]

-- TODO: no computar movimientos donde se saltan piezas
acciones :: Tablero -> TablutPlayer -> [TablutAction]
acciones t p = do
    i <- [0..8]
    j <- [0..8]
    c <- maybeToList $ getDeTablero t i j
    if c `elem` piezas p
        then movsVert i j ++ movsHoriz i j
        else []
    where
        movsVert i j = do
            i' <- delete i [0..8]
            c' <- maybeToList $ getDeTablero t i' j
            if not (any (/=Vacia) (piezasEnLineaVert i i' j)) && c' == Vacia
                then return $  Mover Vertical (i, j) (i' - i)
                else []
            --null ([c'' | c]
        movsHoriz i j = do
            j' <- delete j [0..8]
            c' <- maybeToList $ getDeTablero t i j'
            if not (any (/=Vacia) (piezasEnLineaHoriz j j' i)) && c' == Vacia
                then return $  Mover Horizontal (i, j) (j' - j)
                else []
        piezasEnLineaHoriz a b k = [c | j'' <- entre a b, c <- maybeToList (getDeTablero t k j'')]
        piezasEnLineaVert a b k = [c | i'' <- entre a b, c <- maybeToList (getDeTablero t i'' k)]
        entre a b = [p | p <- [0..8], p `elem` ([(a+1)..b]++[b..(a-1)]) ]
    

-- Aplica una acción al tablero dando el nuevo estado de juego
-- TODO: quitar piezas atacadas
next :: TablutGame -> (TablutPlayer, TablutAction) -> TablutGame
next (TablutGame ShieldPlayer _) (SwordPlayer, _) = error "Este no es el jugador activo"
next (TablutGame SwordPlayer _) (ShieldPlayer, _) = error "Este no es el jugador activo"
next (TablutGame p t) (p', m@(Mover _ coord _)) = 
    if jugadorEs t coord p 
        then TablutGame (oponente p) (realizarMov m t) 
        else error "No tienes una ficha en esa posición"

realizarMov :: TablutAction -> Tablero -> Tablero
realizarMov (Mover Horizontal (i, j) d) t = fromJust $ do
    c <- getDeTablero t i j
    t' <- setEnTablero t i j Vacia
    setEnTablero t' i (j+d) c 
realizarMov (Mover Vertical (i, j) d) t = fromJust $ do
    c <- getDeTablero t i j
    t' <- setEnTablero t i j Vacia
    setEnTablero t' (i+d) j c 


buscarRey :: Tablero -> (Int, Int)
buscarRey t = head $ do
    i <- [0..8]
    j <- [0..8]
    c <-maybeToList $ getDeTablero t i j
    if c == Rey
        then return (i, j)
        else []

reyRodeado :: Tablero -> (Int, Int) -> Bool
reyRodeado t (i, j) = fromJust $ do
    w <- getDeTablero t i (j - 1)
    a <- getDeTablero t (i-1) j
    s <- getDeTablero t i (j + 1)
    d <- getDeTablero t (i+1) (j - 1)
    return ((w, a, s, d) == (PeonEspada, PeonEspada, PeonEspada, PeonEspada))

{- Si el juego está terminado retorna el resultado de juego para cada jugador. 
 - 1 si el jugador ganó, -1 si perdió y 0 si se empató
 - Si el juego no está terminado, se debe retornar una lista vacía. -}
result :: TablutGame -> [(TablutPlayer, Int)]
result (TablutGame _ t) =
    let (i, j) = buscarRey t -- I,J serán las coordenadas del Rey (en este momento).
    in if (i `elem` [0, 8]) || (j `elem` [0, 8]) --Si está en cualquier borde, el shieldPlayer gana
        then [(ShieldPlayer, 1), (SwordPlayer, -1)]
        else if reyRodeado t (i, j)
            then [(ShieldPlayer, -1), (SwordPlayer, 1)]
            else []


activePlayer :: TablutGame -> Maybe TablutPlayer
activePlayer g@(TablutGame p _) = if null (result g) then Just p else Nothing

oponente :: TablutPlayer -> TablutPlayer
oponente ShieldPlayer = SwordPlayer
oponente SwordPlayer = ShieldPlayer
    

beginning :: TablutGame
beginning = TablutGame ShieldPlayer t -- (f1, f2, f3, f4, f5, f6, f7, f8, f9)
    where
        t = ((Vacia, Vacia, Vacia, PeonEspada, PeonEspada, PeonEspada, Vacia, Vacia, Vacia)
            ,(Vacia, Vacia, Vacia, Vacia, PeonEspada, Vacia, Vacia, Vacia, Vacia)
            ,(Vacia, Vacia, Vacia, Vacia, PeonEscudo, Vacia, Vacia, Vacia, Vacia)
            ,(PeonEspada, Vacia, Vacia, Vacia, PeonEscudo, Vacia, Vacia, Vacia, PeonEspada)
            ,(PeonEspada, PeonEspada, PeonEscudo, PeonEscudo, Rey, PeonEscudo, PeonEscudo, PeonEspada, PeonEspada)
            ,(PeonEspada, Vacia, Vacia, Vacia, PeonEscudo, Vacia, Vacia, Vacia, PeonEspada)
            ,(Vacia, Vacia, Vacia, Vacia, PeonEscudo, Vacia, Vacia, Vacia, Vacia)
            ,(Vacia, Vacia, Vacia, Vacia, PeonEspada, Vacia, Vacia, Vacia, Vacia)
            ,(Vacia, Vacia, Vacia, PeonEspada, PeonEspada, PeonEspada, Vacia, Vacia, Vacia))


-- Ø Shield ± Sword  ¥ King
instance Show Casilla where
    show Vacia = " "
    show PeonEspada = "±"
    show Rey = "¥" 
    show PeonEscudo = "Ø"

instance Show TablutGame where
    show = showBoard

showBoard :: TablutGame -> String
showBoard (TablutGame j t) = show j ++ "\n\\ A B C D E F G H I\n" ++ showTablero t

showTablero :: Tablero -> String 
showTablero (f1, f2, f3, f4, f5, f6, f7, f8, f9) = 
    intercalate "\n" $ zipWith (\f n -> show n ++ showFila f) fls [0..8]
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
              letra i = "abcdefhgi" !! i

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
   case (activePlayer g) of
      Nothing -> return $ result g
      Just p -> do
         let ag = [ag1, ag2] !! (fromJust $ elemIndex p [ShieldPlayer, SwordPlayer])
         move <- ag g
         runMatch ags (next g (p, fromJust move))

{- La función ´runOnConsole´ ejecuta toda la partida a partir del estado inicial usando dos agentes
de consola.
-}
runOnConsole :: IO [(TablutPlayer, Int)]
runOnConsole = do
    runMatch (randomAgent ShieldPlayer, randomAgent SwordPlayer) beginning

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

randomAgent :: TablutPlayer -> TablutAgent
randomAgent player state = do
    let moves = fromJust $ lookup player (actions state)
    if null moves then do
       putStrLn "No moves!"
       return Nothing
    else do
       i <- randomRIO (0, length moves - 1)
       return (Just (moves !! i))

