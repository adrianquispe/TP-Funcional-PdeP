import Text.Show.Functions

data Auto = Auto{ nombre::String, nivelDeNafta::Int, velocidad::Int, enamorade::String, truco::(Auto -> Auto) } deriving Show
data Carrera = Carrera { cantidadVueltas::Int, longitudPista::Int, integrantesPublico::[String], participantes::[Auto], trampa::(Carrera->Carrera)} deriving Show

rochaMcQueen = Auto{nombre = "RochaMcQueen" , nivelDeNafta = 300 , velocidad = 0 , enamorade = "Ronco" , truco = (deReversa)}
biankerr = Auto{nombre = "Biankerr" , nivelDeNafta = 500 , velocidad = 20 , enamorade = "Tinch" , truco = (impresionar)}
gushtav = Auto{nombre = "Gushtav" , nivelDeNafta = 200 , velocidad = 130 , enamorade = "PetiLaLinda" , truco = (nitro)}
rodra = Auto{nombre = "Rodra" , nivelDeNafta = 0 , velocidad = 50 , enamorade = "Taisa" , truco = (fingirAmor "Petra")}

potreroFunes = Carrera { cantidadVueltas = 3, longitudPista = 5, integrantesPublico = ["Ronco","Tinch","Dodain"], participantes = [rochaMcQueen,biankerr,gushtav,rodra], trampa = (sacarAlPistero) }

--funcion "deReversa" "completa":
--deReversa :: Auto -> Int -> Auto
--deReversa auto distancia = subirNafta(auto, distancia/5)

inutilidad::Auto->Auto
inutilidad auto = auto

deReversa :: Auto -> Auto
deReversa auto = subirNafta auto (div (velocidad auto) (5))

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

--Trampas--

sacarAlPistero::Carrera->Carrera
sacarAlPistero carrera = carrera{
	participantes = (tail (participantes carrera))
}

lluvia::Carrera->Carrera
lluvia carrera = carrera{
	participantes = map (flip incrementarVelocidadEn (-10)) (participantes carrera)
} --En este ejercicio hay que consultar a algun ayudante/profesor que hacer con el primer corredor ya que su velocidad queda en -10

neutralizarTrucos::Carrera->Carrera
neutralizarTrucos carrera = carrera{
	participantes = map (flip modificarTruco (inutilidad)) (participantes carrera)
}

modificarTruco::Auto->(Auto->Auto)->Auto
modificarTruco auto newTruco = auto{
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


