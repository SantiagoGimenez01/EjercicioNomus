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
    cantFuerza :: Number,
    poderes :: [Poder]
} deriving(Show)

data Poder = UnPoder{
    curacionPorUso :: Number,
    danioPorUso :: Number,
    rangoDeAtaque :: Number,
    probDanioCritico :: Number
}deriving(Show)

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
pepe = UnNomu True 2 3 Azul 100 5000 [superRegeneracion, superFuerza, fuego]

superRegeneracion :: Poder
superRegeneracion = UnPoder 2000 0 10 0

superFuerza :: Poder
superFuerza = UnPoder 500 2000 80 70

fuego :: Poder
fuego = UnPoder 0 2500 200 90

teletransportacion :: Poder
teletransportacion = UnPoder 1000 0 500 0

saberProbDanioUltPoder :: Nomu -> Number
saberProbDanioUltPoder nomu = probDanioCritico (last (poderes nomu))

esCuerpoACuerpo :: Poder -> Bool
esCuerpoACuerpo poder = rangoDeAtaque poder < 100

soloCuracion :: Poder -> Bool
soloCuracion poder = danioPorUso poder == 0 && curacionPorUso poder > 0


