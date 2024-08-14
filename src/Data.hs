module Data where

import Data.List (delete)

data Naipe = Ouros | Espadas | Copas | Paus deriving (Eq, Show)

data Numero = Quatro | Cinco | Seis | Sete | Dama | Valete | Rei | As | Dois | Tres deriving (Eq, Show, Enum, Ord)

data Carta = Carta Numero Naipe deriving (Eq, Show)

type Baralho = [Carta]

criarBaralho :: Baralho
criarBaralho = [Carta num naipe | num <- [Quatro .. Tres], naipe <- [Ouros, Espadas, Copas, Paus]]

data EstadoJogo = EstadoJogo { 
  baralho :: Baralho, 
  cartasJogador :: [Carta], 
  cartasMaquina :: [Carta], 
  manilha :: Numero, 
  pontosJogador :: Int, 
  pontosMaquina :: Int 
} deriving (Show)

embaralharBaralho :: Baralho -> IO Baralho
embaralharBaralho [] = return []
embaralharBaralho baralho = do
    indice <- randomRIO (0, length baralho - 1)
    let carta = baralho !! indice
    resto <- embaralharBaralho (delete carta baralho)
    return (carta : resto)

determinarManilha :: Carta -> Numero
determinarManilha (Carta numero _) = case numero of
    Tres   -> Quatro
    Dois   -> Tres
    As     -> Dois
    Rei    -> As
    Valete -> Rei
    Dama   -> Valete
    Sete   -> Dama
    Seis   -> Sete
    Cinco  -> Seis
    Quatro -> Cinco

compararCartas :: Carta -> Carta -> Numero -> Ordering
compararCartas (Carta num1 naipe1) (Carta num2 naipe2) manilha
    | num1 == manilha && num2 /= manilha = GT
    | num2 == manilha && num1 /= manilha = LT
    | otherwise = compare num1 num2

data Acao = Jogar Carta | PedirTruco deriving (Eq, Show)

calcularPlacar :: Acao -> Acao -> Numero -> (Int, Int)
calcularPlacar (Jogar carta1) (Jogar carta2) manilha =
    case compararCartas carta1 carta2 manilha of
        GT -> (1, 0)
        LT -> (0, 1)
        EQ -> (0, 0)
calcularPlacar (PedirTruco) _ _ = (0, 3)
calcularPlacar _ (PedirTruco) _ = (3, 0)

