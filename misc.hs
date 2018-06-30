import Data.Maybe

data TablutPlayer = ShieldPlayer | SwordPlayer deriving (Eq, Show, Enum)
data Casilla = Vacia | PeonEspada | PeonEscudo | Rey deriving (Eq)

-- data Direccion = Vertical | Horizontal
-- data TablutAction = Mover Direccion (Int, Int) Int

type Fila = (Casilla, Casilla, Casilla, Casilla, Casilla, Casilla, Casilla, Casilla, Casilla)
type Tablero = (Fila, Fila, Fila, Fila, Fila, Fila, Fila, Fila, Fila)

-- Ø Shield ± Sword  ¥ King 
instance Show Casilla where
    show Vacia = "_"
    show PeonEspada = "±"
    show Rey = "¥" 
    show PeonEscudo = "Ø"

beginning :: Tablero
beginning = t
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


{-***********************************************************************************-}
getDeTablero :: Tablero -> Int -> Int -> Maybe Casilla
getDeTablero tablero i j = do
    filaActual <- getNUpla tablero i
    getNUpla filaActual j

setEnTablero :: Tablero -> Int -> Int -> Casilla -> Maybe Tablero
setEnTablero tablero i j cas = do
    filaActual <- getNUpla tablero i -- La fila de indice i (0 a 8) de tableroshow 
    nuevaFila <- setNUpla filaActual j cas -- Arma una nueva fila con la nueva casilla
    setNUpla tablero i nuevaFila -- Cola la nueva fila en el tablero

piezas :: TablutPlayer -> [Casilla]
piezas SwordPlayer = [PeonEspada]
piezas ShieldPlayer = [Rey, PeonEscudo]

{-***********************************************************************************-}
campoDelJugador :: Tablero -> TablutPlayer -> [((Int, Int), Casilla)]
campoDelJugador t j = do
    fil <- [0..8]
    col <- [0..8]
    c <- maybeToList $ getDeTablero t fil col 
    if c `elem` piezas j
        then return ((fil, col), c)
        else []