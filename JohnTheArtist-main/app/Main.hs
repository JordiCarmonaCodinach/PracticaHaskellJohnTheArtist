module Main where

import Artist
import UdGraphic
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Proves:"
  
  putStrLn "-1 separa (Avança 3 :#: Gira 4 :#: Avança 7 :#: Para):"
  print (separa (Avança 3 :#: Gira 4 :#: Avança 7 :#: Para))
  
  putStrLn "\n-2 ajunta [Avança 3, Gira 4, Avança 7]:"
  print (ajunta [Avança 3, Gira 4, Avança 7])
  
  putStrLn "\n-3.1 prop_equivalent (Para :#: Avança 10) (Avança 10):"
  quickCheck (prop_equivalent (Para :#: Avança 10) (Avança 10))
  
  putStrLn "\n-3.2 quickCheck prop_separa_ajunta:"
  quickCheck prop_separa_ajunta
  
  putStrLn "\n-3.3 quickCheck prop_separa:"
  quickCheck prop_separa
  
  putStrLn "\n-4 print (copia 3 (Avança 10 :#: Gira 120)):"
  print (copia 3 (Avança 10 :#: Gira 120))
  
  putStrLn "\n-5 print (pentagon 50):"
  print (pentagon 50)
  
  putStrLn "\n-6.1 print (poligon 50 5 72):"
  print (poligon 50 5 72)
  
  putStrLn "\n-6.2 quickCheck prop_poligon_pentagon:"
  quickCheck prop_poligon_pentagon
  
  putStrLn "\n-7 print (espiral 30 4 5 30):"
  print (espiral 30 4 5 30)
  
  putStrLn "\n-8.1 print (execute (Avança 30 :#: Para :#: Gira 10 :#: Avança 20)):"
  print (execute (Avança 30 :#: Para :#: Gira 10 :#: Avança 20))
  
  putStrLn "\n-8.2 print (execute (Avança 30 :#: Para :#: Gira 10 :#: Avança 20 :#: Gira (-15) :#: Para :#: Avança 10 :#: Para :#: Para)):"
  print (execute (Avança 30 :#: Para :#: Gira 10 :#: Avança 20 :#: Gira (-15) :#: Para :#: Avança 10 :#: Para :#: Para))
  
  putStrLn "\n-9 print (optimitza (Avança 10 :#: Para :#: Avança 20 :#: Gira 35 :#: Avança 0 :#: Gira 15 :#: Gira (-50))):"
  print (optimitza (Avança 10 :#: Para :#: Avança 20 :#: Gira 35 :#: Avança 0 :#: Gira 15 :#: Gira (-50)))
  
  putStrLn "\n-10 print (triangle 1):"
  print (triangle 1)
  
  putStrLn "\n-11 print (fulla 1):"
  print (fulla 1)
  
  putStrLn "\n-12 print (hilbert 1):"
  print (hilbert 1)
  
  putStrLn "\n-13 print (fletxa 1):"
  print (fletxa 1)
  
  putStrLn "\n14- print (branca 1):"
  print (branca 1)
  
  putStrLn "Escull la figura a representar, pots triar entre:\n 5. Pentagon\n 6. Poligon\n 7. Espiral\n 10. Triangle\n 11. Fulla\n 12. Hilbert\n 13. Fletxa\n 14. Branca\n"
  input <- getLine
  let number = read input :: Int
  
  case number of
    5 -> do
      putStrLn "Introdueix la distancia del pentagon: "
      input' <- getLine
      let radius = read input' :: Float
      display (pentagon radius)
    
    6 -> do
      putStrLn "Introdueix la distancia dels costats del poligon: "
      input' <- getLine
      let radius = read input' :: Float
      putStrLn "Introdueix el nombre de costats del poligon:"
      input'' <- getLine
      let sides = read input'' :: Int
      putStrLn "Introdueix l'angle entre costats del poligon:"
      input''' <- getLine
      let angle = read input''' :: Float
      display (poligon radius sides angle)
    
    7 -> do
      putStrLn "Introdueix la distancia del primer segmente de l'espiral:"
      input' <- getLine
      let initialRadius = read input' :: Float
      putStrLn "Introdueix el nombre de segments de linia de l'espiral:"
      input'' <- getLine
      let turns = read input'' :: Int
      putStrLn "Introdueix el factor d'escala de l'espiral:"
      input''' <- getLine
      let scale = read input''' :: Float
      putStrLn "Introdueix l'angle de gir de cada segment de l'espiral:"
      input'''' <- getLine
      let angle = read input'''' :: Float
      display (espiral initialRadius turns scale angle)
    
    10 -> do
      putStrLn "Introdueix el nivell del triangle:"
      input' <- getLine
      let size = read input' :: Int
      display (triangle size)
    
    11 -> do
      putStrLn "Introdueix el nivell de la fulla:"
      input' <- getLine
      let size = read input' :: Int
      display (fulla size)
    
    12 -> do
      putStrLn "Introdueix el nivell de la corvatura de Hilbert:"
      input' <- getLine
      let order = read input' :: Int
      display (hilbert order)
    
    13 -> do
      putStrLn "Introdueix el nivell de la fletxa:"
      input' <- getLine
      let size = read input' :: Int
      display (fletxa size)
    
    14 -> do
      putStrLn "Introdueix el nivell de la branca:"
      input' <- getLine
      let size = read input' :: Int
      display (branca size)
    
    _ -> putStrLn "Opció no vàlida"

