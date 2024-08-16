import Test.QuickCheck
import Data 

-- Testa a função determinarManilha
prop_determinarManilha :: Carta -> Bool
prop_determinarManilha (Carta numero _) =
  case numero of
    Tres   -> determinarManilha (Carta numero Espadas) == Quatro
    Dois   -> determinarManilha (Carta numero Espadas) == Tres
    As     -> determinarManilha (Carta numero Espadas) == Dois
    Rei    -> determinarManilha (Carta numero Espadas) == As
    Valete -> determinarManilha (Carta numero Espadas) == Rei
    Dama   -> determinarManilha (Carta numero Espadas) == Valete
    Sete   -> determinarManilha (Carta numero Espadas) == Dama
    Seis   -> determinarManilha (Carta numero Espadas) == Sete
    Cinco  -> determinarManilha (Carta numero Espadas) == Seis
    Quatro -> determinarManilha (Carta numero Espadas) == Cinco

-- Testa a função compararCartas
prop_compararCartas :: Carta -> Carta -> Bool
prop_compararCartas c1 c2 =
  compararCartas c1 c2 manilha == compare (valor c1) (valor c2)
  where
    manilha = determinarManilha c1

-- Testa a função calcularPlacar
prop_calcularPlacar :: Carta -> Carta -> Bool
prop_calcularPlacar c1 c2 =
  case compararCartas c1 c2 manilha of
    GT -> calcularPlacar (Jogar c1) (Jogar c2) manilha == (1, 0)
    LT -> calcularPlacar (Jogar c1) (Jogar c2) manilha == (0, 1)
    EQ -> calcularPlacar (Jogar c1) (Jogar c2) manilha == (0, 0)
  where
    manilha = determinarManilha c1

main :: IO ()
main = do
  quickCheck prop_determinarManilha
  quickCheck prop_compararCartas
  quickCheck prop_calcularPlacar
