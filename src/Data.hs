module Data where
    
import Data.List (delete)
import System.IO

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
  pontosMaquina :: Int
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
printCarta (Carta As Espadas)    = "🂡"
printCarta (Carta As Copas)      = "🂱"
printCarta (Carta As Ouros)      = "🃁"
printCarta (Carta As Paus)       = "🃑"
printCarta (Carta Dois Espadas)  = "🂢"
printCarta (Carta Dois Copas)    = "🂲"
printCarta (Carta Dois Ouros)    = "🃂"
printCarta (Carta Dois Paus)     = "🃒"
printCarta (Carta Tres Espadas)  = "🂣"
printCarta (Carta Tres Copas)    = "🂳"
printCarta (Carta Tres Ouros)    = "🃃"
printCarta (Carta Tres Paus)     = "🃓"
printCarta (Carta Quatro Espadas) = "🂤"
printCarta (Carta Quatro Copas)   = "🂴"
printCarta (Carta Quatro Ouros)   = "🃄"
printCarta (Carta Quatro Paus)    = "🃔"
printCarta (Carta Cinco Espadas)  = "🂥"
printCarta (Carta Cinco Copas)    = "🂵"
printCarta (Carta Cinco Ouros)    = "🃅"
printCarta (Carta Cinco Paus)     = "🃕"
printCarta (Carta Seis Espadas)   = "🂦"
printCarta (Carta Seis Copas)     = "🂶"
printCarta (Carta Seis Ouros)     = "🃆"
printCarta (Carta Seis Paus)      = "🃖"
printCarta (Carta Sete Espadas)   = "🂧"
printCarta (Carta Sete Copas)     = "🂷"
printCarta (Carta Sete Ouros)     = "🃇"
printCarta (Carta Sete Paus)      = "🃗"
printCarta (Carta Dama Espadas)   = "🂫"
printCarta (Carta Dama Copas)     = "🂻"
printCarta (Carta Dama Ouros)     = "🃋"
printCarta (Carta Dama Paus)      = "🃛"
printCarta (Carta Valete Espadas) = "🂮"
printCarta (Carta Valete Copas)   = "🂾"
printCarta (Carta Valete Ouros)   = "🃎"
printCarta (Carta Valete Paus)    = "🃞"
printCarta (Carta Rei Espadas)    = "🂭"
printCarta (Carta Rei Copas)      = "🂽"
printCarta (Carta Rei Ouros)      = "🃍"
printCarta (Carta Rei Paus)       = "🃝"

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
        EQ -> (0, 0)

ehFimDeJogo :: EstadoJogo -> Bool
ehFimDeJogo estado = pontosJogador estado >= 12 || pontosMaquina estado >= 12

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

-- Função para rodar uma rodada e mostrar as cartas ao jogador e da máquina
jogoTruco :: EstadoJogo -> IO ()
jogoTruco estado = do
    -- Verificar se o jogo terminou
    if ehFimDeJogo estado
        then putStrLn "Fim de jogo!"
        else do

          -- Mostrar a manilha atual
            putStrLn $ "A manilha é: " ++ show (manilha estado)
            
            -- Jogador escolhe uma carta
            cartaJogador <- escolherCarta (cartasJogador estado)
            
            -- Mostrar a carta da máquina no terminal
            let cartaMaquina = head (cartasMaquina estado)
            putStrLn $ "A máquina jogou: " ++ printCarta cartaMaquina
            
            -- Executar uma rodada e obter o novo estado
            let (novoEstado, _) = runState (executarRodada cartaJogador) estado
            
            -- Exibir placar
            putStrLn $ "Pontos do Jogador: " ++ show (pontosJogador novoEstado)
            putStrLn $ "Pontos da Máquina: " ++ show (pontosMaquina novoEstado)
            
            -- Continuar o jogo
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

-- Função principal que executa o jogo, alternando entre `State` e `IO`
executarRodada :: Carta -> State EstadoJogo EstadoJogo
executarRodada cartaJogador = do
    estado <- get
    let cartaMaquina = head (cartasMaquina estado)
    
    -- Comparar cartas
    let manilhaAtual = manilha estado
    let (pontosJogadorAtual, pontosMaquinaAtual) = calcularPlacar cartaJogador cartaMaquina manilhaAtual
    
    -- Atualizar estado
    let novoEstado = estado {
        cartasJogador = delete cartaJogador (cartasJogador estado),  -- Remover a carta jogada
        cartasMaquina = tail (cartasMaquina estado),                 -- Remover a carta jogada pela máquina
        pontosJogador = pontosJogador estado + pontosJogadorAtual,
        pontosMaquina = pontosMaquina estado + pontosMaquinaAtual
    }
    
    put novoEstado
    return novoEstado


