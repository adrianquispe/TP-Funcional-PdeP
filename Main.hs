import Text.Show.Functions

data Auto = Auto{ nombre::String, nivelDeNafta::Int, velocidad::Int, enamorade::String, truco::(Auto -> Auto) } deriving Show
data Carrera = Carrera { cantidadVueltas::Int, longitudPista::Int, integrantesPublico::[String], participantes::[String], trampa::(Auto->Auto)} deriving Show

rochaMcQueen = Auto{nombre = "RochaMcQueen" , nivelDeNafta = 300 , velocidad = 0 , enamorade = "Ronco" , truco = (deReversa)}
biankerr = Auto{nombre = "Biankerr" , nivelDeNafta = 500 , velocidad = 20 , enamorade = "Tinch" , truco = (impresionar)}
gushtav = Auto{nombre = "Gushtav" , nivelDeNafta = 200 , velocidad = 130 , enamorade = "PetiLaLinda" , truco = (nitro)}
rodra = Auto{nombre = "Rodra" , nivelDeNafta = 0 , velocidad = 50 , enamorade = "Taisa" , truco = (fingirAmor "Petra")}

--funcion "deReversa" "completa":
--deReversa :: Auto -> Int -> Auto
--deReversa auto distancia = subirNafta(auto, distancia/5)

deReversa :: Auto -> Auto
deReversa auto = subirNafta auto 200

impresionar :: Auto -> Auto
impresionar auto = incrementarVelocidadEn auto (velocidad auto)

nitro :: Auto -> Auto
nitro = flip incrementarVelocidadEn 15

fingirAmor :: String -> Auto -> Auto
fingirAmor nueveEnamorade auto = auto{ enamorade = nueveEnamorade }

incrementarVelocidad :: Auto -> Auto
incrementarVelocidad auto 
                          | cantidadVocales (enamorade auto) > 4 = incrementarVelocidadEn auto 30
                          | cantidadVocales (enamorade auto) >= 3 = incrementarVelocidadEn auto 20
                          | cantidadVocales (enamorade auto) >= 1 = incrementarVelocidadEn auto 15
                          | otherwise = auto

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco auto = (tieneNafta auto) && (velocidadNecesaria auto)

comboLoco :: Auto -> Auto
comboLoco = deReversa.nitro

queTrucazo :: String -> Auto -> Auto
queTrucazo nueveAmor = incrementarVelocidad.(fingirAmor nueveAmor)

turbo :: Auto -> Auto
turbo = vaciarNafta.aumentarVelocidadPorNivelDeNafta

-- Aux Functions

subirNafta :: Auto -> Int -> Auto
subirNafta auto cant = auto{ nivelDeNafta = nivelDeNafta auto + cant }

incrementarVelocidadEn :: Auto -> Int -> Auto
incrementarVelocidadEn auto cant = auto{ velocidad = velocidad auto + cant }

aumentarVelocidadPorNivelDeNafta :: Auto -> Auto
aumentarVelocidadPorNivelDeNafta auto = incrementarVelocidadEn auto (nivelDeNafta auto * 10)

cantidadVocales :: String -> Int
cantidadVocales = length.vocales

vocales :: String -> String
vocales = filter esVocal

esVocal :: Char -> Bool
esVocal = flip elem "aeiouAEIOU"

tieneNafta :: Auto -> Bool
tieneNafta auto = nivelDeNafta auto > 0

velocidadNecesaria :: Auto -> Bool
velocidadNecesaria auto = velocidad auto <100

vaciarNafta :: Auto -> Auto
vaciarNafta auto = auto{ nivelDeNafta = 0 }

