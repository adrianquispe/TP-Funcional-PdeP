import Kars
import Test.Hspec

import Test.Hspec.QuickCheck

import Test.QuickCheck hiding (property)
import Test.HUnit

main = hspec $ do
  describe "Tests con números:" $ do
     it "nivelDeNafta es 300" $ do
        (nivelDeNafta.truco rochaMcQueen) rochaMcQueen `shouldBe` 300

  describe "Se aplica sacarAlPistero" $ do         
     it "Cantidad de participantes despues de sacarAlPistero deben ser 3" $ do
        (length.participantes.sacarAlPistero) potreroFunes `shouldBe` 3

  describe "Se aplica sacarAlPistero" $ do
     it "rochaMcQueen no participa en potreroFunes" $ do
        (devolverParticipantes.sacarAlPistero) potreroFunes `shouldNotContain` ["RochaMcQueen"]

  describe "Se aplica pocaReserva" $ do
    it "Cantidad participantes despues de pocaReserva deber ser 3" $do
        (length.participantes.pocaReserva) potreroFunes `shouldBe` 3

  describe "Se aplica pocaReserva" $ do
    it "rodra ya no deberia estar entre los participantes luego de aplicar pocaReserva" $ do
        (devolverParticipantes.pocaReserva) potreroFunes  `shouldNotContain` ["Rodra"]

  describe "Se aplica podio" $ do
    it "Cantidad participantes despues de podio deben ser 3" $ do
        (length.participantes.podio) potreroFunes `shouldBe` 3

  describe "Se aplica lluvia" $ do
    it "Velocidad del ultimoParticipante debe ser 40" $ do
        velocidad ((last.participantes.lluvia) potreroFunes) `shouldBe` 40

  describe "Se da una vuelta" $ do
    it "Se consulta nivelDeNafta del primer participante que debe ser 490" $ do
        nivelDeNafta ((head.participantes.darVuelta) potreroFunes) `shouldBe` 490

  describe "Se da una vuelta" $ do
    it "Se consulta velocidad del primer participante luego de dar una vuelta debe ser 40" $ do
        velocidad ((head.participantes.darVuelta) potreroFunes) `shouldBe` 40

  describe "Se da dos vueltas" $ do
    it "Se consulta la cantidad de participantes que deberian ser dos" $ do
        (length.participantes.darVuelta.darVuelta) potreroFunes `shouldBe` 2

  describe "Se da dos vueltas" $ do
    it "Se consulta el nivelDeNafta del primer participante que debe ser 70" $ do
        nivelDeNafta ((head.participantes.darVuelta.darVuelta) potreroFunes) `shouldBe` 70

  describe "Se corre la carrera" $ do
    it "rodra deber ser el unico participante luego de correr la carrera" $ do
        (devolverParticipantes.correrCarrera) potreroFunes `shouldBe` ["Rodra"]

  describe "Ganador de la carrera" $ do
    it "El Ganador debe ser rodra" $ do
        (nombre.quienGana) potreroFunes `shouldBe` "Rodra"

  describe "Se aplica elGranTruco con nitro, deReversa e impresionar" $do
    it "La velocidad de rodra debe ser 130" $do
        (velocidad.(elGranTruco [nitro,deReversa,impresionar])) rodra `shouldBe` 130

  describe "Se aplica elGranTruco con nitro, deReversa e impresionar" $do
    it "El nivelDeNafta de rodra debe ser 13" $do
        (nivelDeNafta.(elGranTruco [nitro,deReversa,impresionar])) rodra `shouldBe` 13
