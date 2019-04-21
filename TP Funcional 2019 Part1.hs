import Text.Show.Functions

data Auto = Auto{ nombre::String, nivelDeNafta::Int, velocidad::Int, enamorade::String, truco::(Auto -> Auto) } deriving Show

rochaMcQueen = Auto{nombre = "RochaMcQueen" , nivelDeNafta = 300 , velocidad = 0 , enamorade = "Ronco" , truco = (deReversa)}
biankerr = Auto{nombre = "Biankerr" , nivelDeNafta = 500 , velocidad = 20 , enamorade = "Tinch" , truco = (impresionar)}
gushtav = Auto{nombre = "Gushtav" , nivelDeNafta = 200 , velocidad = 130 , enamorade = "PetiLaLinda" , truco = (nitro)}
rodra = Auto{nombre = "Rodra" , nivelDeNafta = 0 , velocidad = 50 , enamorade = "Taisa" , truco = (finjirAmor "Petra")}

--funcion "deReversa" "completa":
--deReversa :: Auto -> Int -> Auto
--deReversa auto distancia = subirNafta(auto, distancia/5)

deReversa :: Auto -> Auto
deReversa auto = subirNafta auto 200

impresionar :: Auto -> Auto
impresionar auto = subirVelocidad auto (velocidad auto)

nitro :: Auto -> Auto
nitro = incrementarVelocidadEn 15

finjirAmor :: String -> Auto -> Auto
finjirAmor nueveEnamorade auto = auto{ enamorade = nueveEnamorade }

incrementarVelocidad :: Auto -> Auto
incrementarVelocidad auto 
                          | cantidadVocales (enamorade auto) > 4 = subirVelocidad auto 30
                          | cantidadVocales (enamorade auto) >= 3 = subirVelocidad auto 20
                          | cantidadVocales (enamorade auto) >= 1 = subirVelocidad auto 15
                          | otherwise = auto

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco auto = (tieneNafta auto) && (velocidad auto < 100)

comboLoco :: Auto -> Auto
comboLoco = deReversa.nitro

queTrucazo :: String -> Auto -> Auto
queTrucazo nueveAmor = incrementarVelocidad.(finjirAmor nueveAmor)

turbo :: Auto -> Auto
turbo auto = vaciarNafta (incrementarVelocidadEn (nivelDeNafta auto * 10) auto)

-- Aux Functions

subirNafta :: Auto -> Int -> Auto
subirNafta auto cant = auto{ nivelDeNafta = nivelDeNafta auto + cant }

subirVelocidad :: Auto -> Int -> Auto
subirVelocidad auto speed = auto{ velocidad = velocidad auto + speed }

incrementarVelocidadEn :: Int -> Auto -> Auto
incrementarVelocidadEn cant auto = auto{ velocidad = velocidad auto + cant }

cantidadVocales :: String -> Int
cantidadVocales = length.vocales

vocales :: String -> String
vocales = filter esVocal

esVocal :: Char -> Bool
esVocal = flip elem "aeiouAEIOU"

tieneNafta :: Auto -> Bool
tieneNafta auto = nivelDeNafta auto > 0

vaciarNafta :: Auto -> Auto
vaciarNafta auto = auto{ nivelDeNafta = 0 }