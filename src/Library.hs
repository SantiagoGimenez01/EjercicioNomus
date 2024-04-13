module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--Definimos atributos del nomu
data Nomu = UnNomu {
    alas :: Bool,
    cantBrazos :: Number,
    cantOjos :: Number,
    colorPiel :: ColoresPiel,
    cantVida :: Number,
    cantFuerza :: Number
} deriving(Show)

--Asignamos posibles colores de piel
data ColoresPiel = Negro | Blanco | Gris | Azul | Rojo | Amarillo | Verde deriving(Show)
data CategoriasNomus = Pichi | Comun | Fuerte | HighEnd deriving(Show) 

puedeVer :: Nomu -> Bool
puedeVer nomu = cantOjos nomu > 0 

categoriaNomu :: Nomu -> CategoriasNomus
categoriaNomu nomu 
    | cantFuerza nomu > 10000 = HighEnd
    | cantFuerza nomu > 3000 = Fuerte
    | cantFuerza nomu > 1000 = Comun
    | otherwise = Pichi

pepe :: Nomu
pepe = UnNomu True 2 3 Azul 100 5000