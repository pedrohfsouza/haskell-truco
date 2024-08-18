module Data where

import Data.List (delete)
--import System.Random (randomRIO)
import Test.QuickCheck (Arbitrary, arbitrary, elements)

data Naipe = Ouros | Espadas | Copas | Paus deriving (Eq, Show)

data Numero = Quatro | Cinco | Seis | Sete | Dama | Valete | Rei | As | Dois | Tres deriving (Eq, Show, Enum, Ord)

data Carta = Carta Numero Naipe deriving (Eq, Show)

type Baralho = [Carta]

type Placar = (Int, Int)

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

-- Precisamos ajustar essa função
--embaralharBaralho :: Baralho -> IO Baralho
--embaralharBaralho [] = return []
--embaralharBaralho baralho = do
--    indice <- randomRIO (0, length baralho - 1)
--    let carta = baralho !! indice
--    resto <- embaralharBaralho (delete carta baralho)
--    return (carta : resto)

printCarta :: Carta -> String
printCarta (Carta As Espadas) = "🂡"
printCarta (Carta As Copas) = "🂱"
printCarta (Carta As Ouros) = "🃁"
printCarta (Carta As Paus) = "🃑"
printCarta (Carta Dois Espadas) = "🂢"
printCarta (Carta Dois Copas) = "🂲"
printCarta (Carta Dois Ouros) = "🃂"
printCarta (Carta Dois Paus) = "🃒"
printCarta (Carta Tres Espadas) = "🂣"
printCarta (Carta Tres Copas) = "🂳"
printCarta (Carta Tres Ouros) = "🃃"
printCarta (Carta Tres Paus) = "🃓"
printCarta (Carta Quatro Espadas) = "🂤"
printCarta (Carta Quatro Copas) = "🂴"
printCarta (Carta Quatro Ouros) = "🃄"
printCarta (Carta Quatro Paus) = "🃔"
printCarta (Carta Cinco Espadas) = "🂥"
printCarta (Carta Cinco Copas) = "🂵"
printCarta (Carta Cinco Ouros) = "🃅"
printCarta (Carta Cinco Paus) = "🃕"
printCarta (Carta Seis Espadas) = "🂦"
printCarta (Carta Seis Copas) = "🂶"
printCarta (Carta Seis Ouros) = "🃆"
printCarta (Carta Seis Paus) = "🃖"
printCarta (Carta Sete Espadas) = "🂧"
printCarta (Carta Sete Copas) = "🂷"
printCarta (Carta Sete Ouros) = "🃇"
printCarta (Carta Sete Paus) = "🃗"
printCarta (Carta Dama Espadas) = "🂫"
printCarta (Carta Dama Copas) = "🂻"
printCarta (Carta Dama Ouros) = "🃋"
printCarta (Carta Dama Paus) = "🃛"
printCarta (Carta Valete Espadas) = "🂮"
printCarta (Carta Valete Copas) = "🂾"
printCarta (Carta Valete Ouros) = "🃎"
printCarta (Carta Valete Paus) = "🃞"
printCarta (Carta Rei Espadas) = "🂭"
printCarta (Carta Rei Copas) = "🂽"
printCarta (Carta Rei Ouros) = "🃍"
printCarta (Carta Rei Paus) = "🃝"

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

valor :: Carta -> Int
valor (Carta num _) = case num of
    Quatro -> 1
    Cinco  -> 2
    Seis   -> 3
    Sete   -> 4
    Dama   -> 5
    Valete -> 6
    Rei    -> 7
    As     -> 8
    Dois   -> 9
    Tres   -> 10
    
ehManilha :: Carta -> Numero -> Bool
ehManilha (Carta num _) manilhaNum = num == manilhaNum

compareNaipe :: Carta -> Carta -> Ordering
compareNaipe (Carta _ naipe1) (Carta _ naipe2) = compareNaipeRanking naipe1 naipe2

compareNaipeRanking :: Naipe -> Naipe -> Ordering
compareNaipeRanking naipe1 naipe2
    | naipe1 == naipe2 = EQ
    | naipe1 == Ouros  = GT
    | naipe2 == Ouros  = LT
    | naipe1 == Espadas = GT
    | naipe2 == Espadas = LT
    | naipe1 == Copas  = GT
    | naipe2 == Copas  = LT
    | naipe1 == Paus   = GT
    | naipe2 == Paus   = LT

compararCartas :: Carta -> Carta -> Numero -> Ordering
compararCartas c1 c2 manilha
    | ehManilha c1 manilha && ehManilha c2 manilha = compareNaipe c1 c2
    | ehManilha c1 manilha = GT
    | ehManilha c2 manilha = LT
    | valor c1 > valor c2  = GT
    | valor c1 < valor c2  = LT
    | otherwise            = EQ


data Acao = Jogar Carta | PedirTruco deriving (Eq, Show)

calcularPlacar :: Acao -> Acao -> Numero -> Placar
calcularPlacar (Jogar carta1) (Jogar carta2) manilha =
    case compararCartas carta1 carta2 manilha of
        GT -> (1, 0)
        LT -> (0, 1)
        EQ -> (0, 0)
calcularPlacar PedirTruco _ _ = (0, 3)
calcularPlacar _ PedirTruco _ = (3, 0)

ehFimDeJogo :: Placar -> Bool
ehFimDeJogo (x,y) = x >= 12 || y >= 12

-- Instâncias para o QuickCheck
instance Arbitrary Naipe where
    arbitrary = elements [Ouros, Espadas, Copas, Paus]

instance Arbitrary Numero where
    arbitrary = elements [Quatro, Cinco, Seis, Sete, Dama, Valete, Rei, As, Dois, Tres]

instance Arbitrary Carta where
    arbitrary = do
        numero <- arbitrary
        naipe <- arbitrary
        return $ Carta numero naipe
