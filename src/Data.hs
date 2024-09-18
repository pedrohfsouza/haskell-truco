module Data where

import Data.List (delete)
import System.IO
import Data.List (unfoldr)
import System.Random (randomRIO, newStdGen, StdGen)
import System.Random.Shuffle (shuffleM)

data Naipe = Ouros | Espadas | Copas | Paus deriving (Eq, Show)

data Numero = Quatro | Cinco | Seis | Sete | Dama | Valete | Rei | As | Dois | Tres deriving (Eq, Show, Enum, Ord)

data Carta = Carta Numero Naipe deriving (Eq, Show)

type Baralho = [Carta]
type Placar = (Int, Int)

data EstadoJogo = EstadoJogo {
  baralho :: Baralho,
  cartasJogador :: [Carta],
  cartasMaquina :: [Carta],
  manilha :: Numero,
  pontosJogador :: Int,
  pontosMaquina :: Int,
  pontosTotaisJogador :: Int,
  pontosTotaisMaquina :: Int
} deriving (Show)

criarBaralho :: Baralho
criarBaralho = [Carta num naipe | num <- [Quatro .. Tres], naipe <- [Ouros, Espadas, Copas, Paus]]

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

printCarta :: Carta -> String
printCarta (Carta As Espadas)    = "As Espadas"
printCarta (Carta As Copas)      = "As Copas"
printCarta (Carta As Ouros)      = "As Ouros"
printCarta (Carta As Paus)       = "As Paus"
printCarta (Carta Dois Espadas)  = "Dois Espadas"
printCarta (Carta Dois Copas)    = "Dois Copas"
printCarta (Carta Dois Ouros)    = "Dois Ouros"
printCarta (Carta Dois Paus)     = "Dois Paus"
printCarta (Carta Tres Espadas)  = "Tres Espadas"
printCarta (Carta Tres Copas)    = "Tres Copas"
printCarta (Carta Tres Ouros)    = "Tres Ouros"
printCarta (Carta Tres Paus)     = "Tres Paus"
printCarta (Carta Quatro Espadas) = "Quatro Espadas"
printCarta (Carta Quatro Copas)   = "Quatro Copas"
printCarta (Carta Quatro Ouros)   = "Quatro Ouros"
printCarta (Carta Quatro Paus)    = "Quatro Paus"
printCarta (Carta Cinco Espadas)  = "Cinco Espadas"
printCarta (Carta Cinco Copas)    = "Cinco Copas"
printCarta (Carta Cinco Ouros)    = "Cinco Ouros"
printCarta (Carta Cinco Paus)     = "Cinco Paus"
printCarta (Carta Seis Espadas)   = "Seis Espadas"
printCarta (Carta Seis Copas)     = "Seis Copas"
printCarta (Carta Seis Ouros)     = "Seis Ouros"
printCarta (Carta Seis Paus)      = "Seis Paus"
printCarta (Carta Sete Espadas)   = "Sete Espadas"
printCarta (Carta Sete Copas)     = "Sete Copas"
printCarta (Carta Sete Ouros)     = "Sete Ouros"
printCarta (Carta Sete Paus)      = "Sete Paus"
printCarta (Carta Dama Espadas)   = "Dama Espadas"
printCarta (Carta Dama Copas)     = "Dama Copas"
printCarta (Carta Dama Ouros)     = "Dama Ouros"
printCarta (Carta Dama Paus)      = "Dama Paus"
printCarta (Carta Valete Espadas) = "Valete Espadas"
printCarta (Carta Valete Copas)   = "Valete Copas"
printCarta (Carta Valete Ouros)   = "Valete Ouros"
printCarta (Carta Valete Paus)    = "Valete Paus"
printCarta (Carta Rei Espadas)    = "Rei Espadas"
printCarta (Carta Rei Copas)      = "Rei Copas"
printCarta (Carta Rei Ouros)      = "Rei Ouros"
printCarta (Carta Rei Paus)       = "Rei Paus"

compararCartas :: Carta -> Carta -> Numero -> Ordering
compararCartas (Carta num1 _) (Carta num2 _) manilha
    | ehManilha num1 manilha && ehManilha num2 manilha = compare (valorManilha num1) (valorManilha num2)
    | ehManilha num1 manilha = GT
    | ehManilha num2 manilha = LT
    | otherwise              = compare num1 num2

ehManilha :: Numero -> Numero -> Bool
ehManilha numero manilha = numero == manilha

valorManilha :: Numero -> Int
valorManilha num = case num of
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


-- Função para determinar o vencedor de uma rodada
calcularPlacar :: Carta -> Carta -> Numero -> Placar
calcularPlacar carta1 carta2 manilha =
    case compararCartas carta1 carta2 manilha of
        GT -> (1, 0)
        LT -> (0, 1)
        EQ -> (1, 1)

ehFimDeJogo :: EstadoJogo -> Bool
ehFimDeJogo estado = pontosTotaisJogador estado >= 12 || pontosTotaisMaquina estado >= 12

-- Função para exibir as cartas do jogador
mostrarMaoJogador :: [Carta] -> IO ()
mostrarMaoJogador cartas = do
    putStrLn "Suas cartas:"
    mapM_ (\(i, carta) -> putStrLn (show i ++ ": " ++ printCarta carta)) (zip [1..] cartas)

-- Função para exibir as cartas da máquina
mostrarMaoMaquina :: [Carta] -> IO ()
mostrarMaoMaquina cartas = do
    putStrLn "Cartas da máquina:"
    mapM_ (putStrLn . printCarta) cartas


newtype State s a = State (s -> (a, s))

instance Functor (State s) where
  fmap f (State g) = State (\s -> let (a, s') = g s in (f a, s'))

instance Applicative (State s) where
  pure x = State (\s -> (x, s))
  State sab <*> State sa = State (\s -> let (f, s1) = sab s
                                            (a, s2) = sa s1
                                        in (f a, s2))
instance Monad (State s) where
  State sa >>= f = State (\s -> let (a, s1) = sa s
                                    State sb = f a
                                in sb s1)

runState :: State s a -> s -> (a, s)
runState (State f) = f

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put $ f s

exec :: State s a -> s -> s
exec sa s = snd $ runState sa s

embaralharBaralho :: Baralho -> IO Baralho
embaralharBaralho baralho = shuffleM baralho
                       
distribuirCartas :: Baralho -> ([Carta], [Carta], Baralho)
distribuirCartas baralho =
    let (cartasJogador, resto1) = splitAt 3 baralho
        (cartasMaquina, resto2) = splitAt 3 resto1
    in (cartasJogador, cartasMaquina, resto2)

reiniciarRodada :: EstadoJogo -> IO EstadoJogo
reiniciarRodada estado = do
    -- Embaralha e distribui cartas
    baralhoNovo <- embaralharBaralho criarBaralho
    let (cartasJogador, cartasMaquina, baralhoRestante) = distribuirCartas baralhoNovo

    -- Passa as informacoes atualizadas para o estado
    return estado {
        baralho = baralhoRestante,
        cartasJogador = cartasJogador,
        cartasMaquina = cartasMaquina,
        pontosJogador = 0,
        pontosMaquina = 0
    }

calcularRodada :: EstadoJogo -> IO EstadoJogo
calcularRodada estado = 
    if pontosMaquina estado >= 3
        then do
            putStrLn "O Vencedor da rodada foi a machina"
            let novoEstado = estado {
                pontosTotaisMaquina = pontosTotaisMaquina estado + 1
            }
            return novoEstado
        else do
            putStrLn "O Vencedor da rodada foi a jogador"
            let novoEstado = estado {
                pontosTotaisJogador = pontosTotaisJogador estado + 1
            }
            return novoEstado

jogoTruco :: EstadoJogo -> IO ()
jogoTruco estado = do
    if ehFimDeJogo estado
        then putStrLn "Fim de jogo!"
        else if null (cartasJogador estado) && null (cartasMaquina estado)
            then do 
                putStrLn ""
                novoEstado <- calcularRodada estado
                novoEstadoRodada <- reiniciarRodada novoEstado
                putStrLn $ "O placar e: Jogador " ++ show (pontosTotaisJogador novoEstadoRodada) ++ " x " ++ show (pontosTotaisMaquina novoEstadoRodada) ++ " Machina"
                putStrLn ""
                putStrLn "Comecando nova rodada"
                jogoTruco novoEstadoRodada
            else do
                putStrLn $ "A manilha é: " ++ show (manilha estado)
                cartaJogador <- escolherCarta (cartasJogador estado)
                let cartaMaquina = head (cartasMaquina estado)
                putStrLn $ "A máquina jogou: " ++ printCarta cartaMaquina
                let (novoEstado, _) = runState (executarRodada cartaJogador) estado
                putStrLn $ "Pontos do Jogador: " ++ show (pontosJogador novoEstado)
                putStrLn $ "Pontos da Máquina: " ++ show (pontosMaquina novoEstado)
                
                --if pontosJogador novoEstado >= 3
                --    then do
                --        putStrLn "Você ganhou a rodada! Deseja começar uma nova rodada ou sair do jogo? (Digite 'nova' para nova rodada ou 'sair' para sair)"
                --        opcao <- getLine
                --        case opcao of
                --            "nova" -> do
                --                novoEstadoRodada <- reiniciarRodada novoEstado
                --                jogoTruco novoEstadoRodada
                --            "sair" -> putStrLn "Saindo do jogo. Até mais!"
                --            _      -> do
                --                putStrLn "Opção inválida. Tente novamente."
                --                jogoTruco novoEstado
                jogoTruco novoEstado

-- Função para permitir que o jogador escolha uma carta para jogar
escolherCarta :: [Carta] -> IO Carta
escolherCarta cartas = do
    mostrarMaoJogador cartas
    putStrLn "Escolha uma carta pelo número correspondente:"
    escolha <- getLine
    let idx = read escolha :: Int
    if idx >= 1 && idx <= length cartas
        then return (cartas !! (idx - 1))
        else do
            putStrLn "Escolha inválida, tente novamente."
            escolherCarta cartas

-- Função principal que executa o jogo, alternando entre State e IO
executarRodada :: Carta -> State EstadoJogo EstadoJogo
executarRodada cartaJogador = do
    estado <- get
    let cartaMaquina = head (cartasMaquina estado)
    
    -- Comparar cartas
    let manilhaAtual = manilha estado
    let (pontosJogadorAtual, pontosMaquinaAtual) = calcularPlacar cartaJogador cartaMaquina manilhaAtual
    
    -- Atualizar estado
    let novoEstado = estado {
        cartasJogador = delete cartaJogador (cartasJogador estado), 
        cartasMaquina = tail (cartasMaquina estado),                 
        pontosJogador = pontosJogador estado + pontosJogadorAtual,
        pontosMaquina = pontosMaquina estado + pontosMaquinaAtual
    }
    
    put novoEstado
    return novoEstado
