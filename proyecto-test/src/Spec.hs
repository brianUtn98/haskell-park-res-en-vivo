module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Atracciones" $ do
    describe "montaña rusa" $ do
      it "montaña rusa rapida" $ do
        emocion (montañaRusa 60 20 ana) `shouldBe` 49
      it "montaña rusa lenta disminuye emocion" $ do
        emocion (montañaRusa 40 20 ana) `shouldBe` 19
      it "montaña rusa lenta disminuye satisfaccion" $ do
        satisfaccion (montañaRusa 40 20 ana) `shouldBe` 9
    describe "caida libre" $ do
      it "caida libre aumenta la emocion" $ do
        emocion (caidaLibre 20 ana) `shouldBe` 24
    describe "mundo maya" $ do
      it "mundo maya aumenta la emocion" $ do
        emocion (mundoMaya ana) `shouldBe` 22
      it "mundo maya aumenta la cultura" $ do
        cultura (mundoMaya ana) `shouldBe` 72
    describe "show de magia" $ do
      it "show de magia aumenta satisfaccion a persona con cultura mayor a 50" $ do
        satisfaccion (showDeMagia ana) `shouldBe` 30
      it "show de magia aumenta la emocion a persona con cultura menor o igual a 50" $ do
        emocion (showDeMagia juan) `shouldBe` 60

  describe "Visitar" $ do
    describe "visitar" $ do
      it "emocion de persona que visita una atraccion" $ do
        emocion (visitar [montañaRusa 60 20] ana) `shouldBe` emocion (montañaRusa 60 20 ana) 
      it "emocion de persona que visita muchas atracciones" $ do
        emocion (visitar todas ana) `shouldBe` emocion ((\persona -> (montañaRusa 100 50).showDeMagia $ persona) . showDeMagia . mundoMaya . caidaLibre 50 . montañaRusa 20 20 $ ana)
      it "emocion de persona que visita ninguna atraccion" $ do
        emocion (visitar [] ana) `shouldBe` emocion ana

  describe "Atraccion inventada" $ do
    describe "atraccion inventada" $ do
      it "atraccion inventada es como ir a un show de magia y luego a una montaña rusa de 50 metros a 100km/h" $ do
        emocion (atraccionInventada ana) `shouldBe` emocion (montañaRusa 100 50 . showDeMagia $ ana)
  
  describe "estan felices" $ do
    describe "estan felices" $ do
      it "personas que estan felices" $ do
        estanFelices [ana,juan] `shouldBe` True
      it "no todas estan felices" $ do
        estanFelices [ana,juan,grinch] `shouldBe` False
      it "estan felices una lista vacia" $ do
        estanFelices [] `shouldBe` True
  
  describe "esta contenta" $ do
    describe "esta contenta" $ do
      it "persona esta contenta al visitar una atraccion" $ do
        estaContenta [montañaRusa 100 100] ana `shouldBe` True
      it "persona esta contenta al visitar todas las atracciones" $ do
        estaContenta todas grinch `shouldBe` True
      it "persona no esta contenta al visitar una atraccion" $ do
        estaContenta [mundoMaya] leopoldo `shouldBe` False
      it "persona no esta contenta al visitar varias atracciones" $ do
        estaContenta [montañaRusa 20 20,caidaLibre 10,mundoMaya] leopoldo `shouldBe` False

  describe "personas infinitas" $ do
    describe "personas infinitas" $ do
      it "se puede procesar lista infinita por lazy evaluation" $ do
        nombre (h (estaContenta todas) personasInfinitas) `shouldBe` "Ana"

