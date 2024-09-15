module Main (main) where

import Data

import Control.Monad
import System.IO (hFlush, stdout)




main :: IO ()
main = do
    -- Inicializar o estado do jogo
    let estadoInicial = EstadoJogo {
            baralho = criarBaralho,
            cartasJogador = take 3 criarBaralho,  -- Distribuir cartas iniciais
            cartasMaquina = take 3 (drop 3 criarBaralho),
            manilha = determinarManilha (criarBaralho !! 6), -- Definir manilha com uma carta qualquer do baralho
            pontosJogador = 0,
            pontosMaquina = 0
        }

    -- Rodar o jogo usando a monad State
    jogoTruco estadoInicial
