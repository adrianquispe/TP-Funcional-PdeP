module Kars where
import Text.Show.Functions

data Auto = Auto{ nombre::String, nivelDeNafta::Int, velocidad::Int, enamorade::String, truco::(Auto -> Auto) } deriving Show
data Carrera = Carrera { cantidadVueltas::Int, longitudPista::Float, integrantesPublico::[String], participantes::[Auto], trampa::(Carrera->Carrera)} deriving Show

rochaMcQueen = Auto{nombre = "RochaMcQueen" , nivelDeNafta = 300 , velocidad = 0 , enamorade = "Ronco" , truco = (deReversa)}
biankerr = Auto{nombre = "Biankerr" , nivelDeNafta = 500 , velocidad = 20 , enamorade = "Tinch" , truco = (impresionar)}
gushtav = Auto{nombre = "Gushtav" , nivelDeNafta = 200 , velocidad = 130 , enamorade = "PetiLaLinda" , truco = (nitro)}
rodra = Auto{nombre = "Rodra" , nivelDeNafta = 0 , velocidad = 50 , enamorade = "Taisa" , truco = (fingirAmor "Petra")}

potreroFunes = Carrera { cantidadVueltas = 3, longitudPista = 5.0, integrantesPublico = ["Ronco","Tinch","Dodain"], participantes = [rochaMcQueen,biankerr,gushtav,rodra], trampa = (sacarAlPistero) }

inutilidad::Auto->Auto
inutilidad auto = auto

deReversa :: Auto -> Auto
deReversa auto = subirNafta auto (div (velocidad auto) 5)

impresionar :: Auto -> Auto
impresionar auto = variarVelocidadEn auto (velocidad auto)

nitro :: Auto -> Auto
nitro = flip variarVelocidadEn 15

fingirAmor :: String -> Auto -> Auto
fingirAmor nueveEnamorade auto = auto{ enamorade = nueveEnamorade }

incrementarVelocidad :: Auto -> Auto
incrementarVelocidad auto 
                          | cantidadVocales (enamorade auto) > 4 = variarVelocidadEn auto 30
                          | cantidadVocales (enamorade auto) >= 3 = variarVelocidadEn auto 20
                          | cantidadVocales (enamorade auto) >= 1 = variarVelocidadEn auto 15
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

variarVelocidadEn :: Auto -> Int -> Auto
variarVelocidadEn auto cant = auto{ velocidad = velocidad auto + cant }

aumentarVelocidadPorNivelDeNafta :: Auto -> Auto
aumentarVelocidadPorNivelDeNafta auto = variarVelocidadEn auto (nivelDeNafta auto * 10)

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

--Trampas--

sacarAlPistero::Carrera->Carrera
sacarAlPistero carrera = carrera{
     participantes =  sacarAQuienVaPrimero (participantes carrera)
}
sacarAQuienVaPrimero :: [Auto] -> [Auto]
sacarAQuienVaPrimero [] = []
sacarAQuienVaPrimero (primero:resto) = resto

lluvia::Carrera->Carrera
lluvia carrera = carrera{
     participantes = map (reducirVelocidadEn (-10)) (participantes carrera)
} --En este ejercicio hay que consultar a algun ayudante/profesor que hacer con el primer corredor ya que su velocidad queda en -10
  --Resuelto
reducirVelocidadEn :: Int -> Auto -> Auto
reducirVelocidadEn cant auto
                            | cant > velocidad auto = variarVelocidadEn auto (velocidad auto)
                            | otherwise = variarVelocidadEn auto cant

neutralizarTrucos::Carrera->Carrera
neutralizarTrucos carrera = carrera{
     participantes = map (modificarTruco inutilidad) (participantes carrera)
}

modificarTruco::(Auto->Auto)->Auto->Auto
modificarTruco newTruco auto = auto{
     truco = newTruco
}


pocaReserva::Carrera->Carrera
pocaReserva carrera = carrera{
 participantes = filter naftaSuficiente (participantes carrera)
}

naftaSuficiente :: Auto -> Bool
naftaSuficiente = (>30).nivelDeNafta

--conPocaNafta listaAutos = filter ((nivelDeNafta) > 30) (listaAutos)

podio::Carrera->Carrera
podio carrera = carrera{
     participantes = (take 3 (participantes carrera))
}


restarCombustible :: Carrera -> Carrera
restarCombustible carrera = carrera {participantes = map (aplicarFormula carrera) (participantes carrera)}

aplicarFormula :: Carrera -> Auto -> Auto
aplicarFormula carrera auto = modificarCombustible auto (formula (velocidad auto) (longitudPista carrera))

modificarCombustible :: Auto -> Int -> Auto
modificarCombustible auto valorFormula
                                      | valorFormula > nivelDeNafta auto = vaciarNafta auto
                                      | otherwise = auto {nivelDeNafta = nivelDeNafta auto - valorFormula}

formula :: Int -> Float -> Int
formula speed largoPista = ceiling (largoPista * fromIntegral (div speed 10))

enamoradeEnPublico :: Carrera -> Carrera
enamoradeEnPublico carrera = carrera {participantes = realizarTrucoParaEnamorade (integrantesPublico carrera) (participantes carrera)}

realizarTrucoParaEnamorade :: [String] -> [Auto] -> [Auto]
realizarTrucoParaEnamorade publico = map (haceTrucoSiEstaEnamoradeEnPublico publico) 

haceTrucoSiEstaEnamoradeEnPublico :: [String] -> Auto -> Auto
haceTrucoSiEstaEnamoradeEnPublico publico auto
                                              | elem (enamorade auto) publico = (truco auto) auto
                                              | otherwise = auto

sufrirTrampa :: Carrera -> Carrera
sufrirTrampa carrera = (trampa carrera) carrera

darVuelta :: Carrera -> Carrera
darVuelta = sufrirTrampa.enamoradeEnPublico.restarCombustible

correrCarrera :: Carrera -> Carrera
correrCarrera carrera = (!!) (iterate (darVuelta) carrera) (cantidadVueltas carrera)

quienGana :: Carrera -> Auto
quienGana carrera = elMasVeloz ((participantes.correrCarrera) carrera)

elMasVeloz :: [Auto] -> Auto
elMasVeloz = foldl1 esMasVelozQue

esMasVelozQue :: Auto -> Auto -> Auto
esMasVelozQue unAuto otroAuto
                             | velocidad unAuto >= velocidad otroAuto = unAuto
                             | otherwise = otroAuto

elGranTruco :: [(Auto->Auto)] -> Auto -> Auto
<<<<<<< HEAD
elGranTruco [truco] = truco
elGranTruco (unTruco:otrosTrucos) = (elGranTruco (otrosTrucos)).unTruco
=======
elGranTruco [x] auto = x auto
elGranTruco (x:xs) auto = (elGranTruco(xs).x) auto

--aux test
devolverParticipantes :: Carrera -> [String]
devolverParticipantes carrera = map nombre (participantes carrera) 
>>>>>>> db73f5178eaef27bf0712fa80eec9b64038f2a0f
