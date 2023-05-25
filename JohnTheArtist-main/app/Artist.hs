module Artist where

import UdGraphic

import Test.QuickCheck

import Debug.Trace



-- Problema 1



-- Separa una comanda composta en una llista de comandes simples.

separa :: Comanda -> [Comanda]

separa (Avança d) = [Avança d]

separa (Gira a) = [Gira a]

separa (p :#: q) = separa p ++ separa q

separa Para = []



-- Problema 2



-- Ajunta una llista de comandes simples en una comanda composta.

ajunta :: [Comanda] -> Comanda

ajunta []     = Para

ajunta [c]    = c :#: ajunta []

ajunta (c:cs) = c :#: ajunta cs



-- funcio que ajunta però no afegeix una comanda Para al final.

ajunta' :: [Comanda] -> Comanda 

ajunta' [c]    = c

ajunta' (c:cs) = c :#: ajunta' cs



-- Problema 3



-- Retorna true si les dues comandes entrades per parametre son equivalents, altrament retorna false.

prop_equivalent :: Comanda -> Comanda -> Bool

prop_equivalent c1 c2 = separa c1 == separa c2



-- Retorna true si la comanda entrada per paràmetre és equivalent a la mateixa comanda

-- aplicant-li la funció separa, i després ajunta, altrament retorna false.

prop_separa_ajunta :: Comanda -> Bool

prop_separa_ajunta c = prop_equivalent c (ajunta (separa c))



-- Retorna true si totes les comandes que retorna la funcio "separa" son comandes simples,

-- es a dir, que la llista no contingui comandes compostes, ni la comanda "Para" entre elles.

prop_separa :: Comanda -> Bool

prop_separa c = all esComanda (separa c)

 where

    esComanda (Avança _) = True

    esComanda (Gira _) = True

    esComanda _ = False



-- Problema 4



-- Copia una comanda tantes vegades com s'indica al primer parametre.

copia :: Int -> Comanda -> Comanda

copia n c = ajunta (replicate n c)



-- Problema 5



-- Donada la mida d'un costat retorna una comanda per dibuixar un pentagon.

pentagon :: Distancia -> Comanda

pentagon d = copia 5 (Avança d :#: Gira 72)



-- Problema 6



-- Donats la mida de costat, el nombre de costats, i l'angle entre costats,

-- retorna una comanda per dibuixar un poligon regular amb les mides indicades.

poligon :: Distancia -> Int -> Angle -> Comanda

poligon d n a = copia n (Avança d :#: Gira a)



-- Donada la mida d'un costat, retorna true si la comanda que genera el pentagon es equivalent

-- a la comanda que genera un poligon amb els parametres d'un pentagon.

prop_poligon_pentagon :: Distancia -> Bool

prop_poligon_pentagon d = prop_equivalent c1 c2

  where 

    c1 = poligon d 5 72

    c2 = pentagon d



-- Problema 7



-- Donats la mida del primer segment, nombre de segments a dibuixar, distància que s'augmenta 

-- per cada segment successiu, i l'angle de l'espiral, retorna una comanda per dibuixar 

-- una espiral amb les mides indicades.

espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda

espiral _ 0 _ _ = Para

espiral distancia n pas angle = Avança distancia :#: Gira angle :#: espiral (distancia + pas) (n - 1) pas angle



-- Problema 9



-- Donada una comanda, retorna la mateixa comanda optimitzada(simplificada al màxim).

optimitza :: Comanda -> Comanda

optimitza comanda = if comandaOptimitzada == comanda

                    then comandaOptimitzada

                    else optimitza comandaOptimitzada

  where

    comandaOptimitzada = ajunta'(separa(simplifica comanda))

                    

-- Funcio que donada una comanda, retorna una comanda equivalent simplificada. Simplifica ocurrencies

-- del mateix tipus de comanda consecutives per una sola d'aquest tipus, si s'anulen els seus valors,

-- retorna la comanda neutra "Para". Crida a simplificaAux per simplificar comandes simples.

simplifica :: Comanda -> Comanda

simplifica (Avança 0) = Para

simplifica (Gira 0) = Para

simplifica (Avança d1 :#: Avança d2 :#: cs) = simplifica (Avança (d1 + d2) :#: cs)

simplifica (Gira a1 :#: Gira a2 :#: cs) = simplifica (Gira (a1 + a2) :#: cs)

simplifica (c1 :#: c2) = simplificaAux (simplifica c1) (simplifica c2)

simplifica c = c



-- Donades dues comandes simples, si son del mateix tipus retorna una comanda d'aquest tipus 

-- amb els valors sumats. Si son de tipus diferents, retorna una comanda composta per aquestes.

simplificaAux :: Comanda -> Comanda -> Comanda

simplificaAux (Avança d1) (Avança d2) = Avança (d1 + d2)

simplificaAux (Gira a1) (Gira a2) = Gira (a1 + a2)

simplificaAux c1 c2 = c1 :#: c2





-- Problema 10



-- Donat el nivell del fractal(recursivitat) retorna una comanda per dibuixar la figura triangle.

-- (aquesta funció no s'utilitza, funcional)

triangle' :: Int -> Comanda

triangle' n = reescriptura "f" n

  where

    reescriptura :: String -> Int -> Comanda

    reescriptura c 0 = tradueix c

    reescriptura c n = reescriptura (reemplaça c) (n - 1)

    reemplaça :: String -> String

    reemplaça [] = []

    reemplaça ('f' : rest) = "f+f-f-f+f" ++ reemplaça rest

    reemplaça (c : rest) = c : reemplaça rest

    tradueix :: String -> Comanda

    tradueix [] = Para

    tradueix ('f' : rest) = Avança 10 :#: tradueix rest

    tradueix ('+' : rest) = Gira 90 :#: tradueix rest

    tradueix ('-' : rest) = Gira (-90) :#: tradueix rest



-- Donat el nivell del fractal(recursivitat) retorna una comanda per dibuixar la figura triangle,

-- utilitzant la funció fTriangle. (aquesta és la que s'utilitza)

triangle :: Int -> Comanda

triangle n = fTriangle n



-- Funció que donat el nivell de recursivitat del fractal, utilitza la gramàtica determinada per generar

-- la figura triangle. A partir de la gramàtica es substitueix cada element per la seva comanda

-- corresponent, utilitzant la recursivitat per implementar les sustitucions de la "f" i expandir-la

-- segons la seva definició gramatical.

fTriangle :: Int -> Comanda

fTriangle 0 = CanviaColor blau :#: Avança 10

fTriangle n = fTriangle(n - 1) :#: Gira 90 :#: fTriangle(n - 1) :#: Gira (-90) :#: fTriangle(n - 1) :#: Gira (-90) :#: fTriangle(n - 1) :#: Gira 90 :#: fTriangle(n - 1)



-- Problema 11



-- Donat el nivell del fractal(recursivitat) retorna una comanda per dibuixar la figura fulla.

-- (aquesta funció no s'utilitza, no funcional)

fulla' :: Int -> Comanda

fulla' n = reescriptura "f" n

  where

    reescriptura :: String -> Int -> Comanda

    reescriptura c 0 = tradueix c

    reescriptura c n = reescriptura (reemplaça c) (n - 1)

    

    reemplaça :: String -> String

    reemplaça [] = []

    reemplaça ('f' : rest) = "g[-f][+f][gf]" ++ reemplaça rest

    reemplaça ('g' : rest) = "gg" ++ reemplaça rest

    reemplaça (c : rest) = c : reemplaça rest

    tradueix :: String -> Comanda

    tradueix [] = Para

    tradueix ('f' : rest) = Avança 10 :#: tradueix rest

    tradueix ('g' : rest) = Avança 10 :#: tradueix rest

    tradueix ('[' : rest) = Branca (tradueix rest)

    tradueix (']' : rest) = tradueix rest

    tradueix ('+' : rest) = Gira 45 :#: tradueix rest

    tradueix ('-' : rest) = Gira (-45) :#: tradueix rest



-- Donat el nivell del fractal(recursivitat) retorna una comanda per dibuixar la figura fulla,

-- utilitzant la funció fFulla. (aquesta és la que s'utilitza)

fulla :: Int -> Comanda

fulla n = fFulla n



-- Funció que donat el nivell de recursivitat del fractal, utilitza la gramàtica determinada per generar

-- la figura fulla mitjançant una comanda i la retorna. A partir de la gramàtica es substitueix cada element

-- per la seva comanda corresponent, utilitzant la recursivitat per implementar la substitució de "f" i 

-- expandir-la segons la seva definició gramatical. S'implementa la comanda corresponent codificada 

-- per "-" i "+" i "g". Per implementar la comanda que sustitueix a "g" s'utilitza la funció gFulla.

fFulla :: Int -> Comanda

fFulla 0 = CanviaColor verd :#: Avança 10   -- Si nivell 0(darrer nivell) canvia el color del llapis

fFulla n = gFulla (n - 1) :#: Branca (Gira (-45) :#: fFulla (n - 1)) :#: Branca (Gira 45 :#: fFulla (n - 1)) :#: Branca (gFulla (n - 1) :#: fFulla (n - 1))



-- Donat el nivell de recursivitat del fractal, retorna una comanda que implementa l'element de la gramàtica "g".

-- Utilitzant la recursivitat si es crida gFulla per un nivell de fractal > 0.

gFulla :: Int -> Comanda

gFulla 0 = CanviaColor marro :#: Avança 10      -- Si nivell 0(darrer nivell) canvia el color del llapis

gFulla n = gFulla (n - 1) :#: gFulla (n - 1) 

  

-- Problema 12



-- Donat el nivell del fractal(recursivitat) retorna una comanda per dibuixar la figura fulla.

-- (aquesta funció no s'utilitza, no funcional)

hilbert' :: Int -> Comanda

hilbert' n = reescriptura "l" n

  where

    reescriptura :: String -> Int -> Comanda

    reescriptura c 0 = tradueix c

    reescriptura c n = reescriptura (reemplaça c) (n - 1)



    reemplaça :: String -> String

    reemplaça [] = []

    reemplaça ('l' : rest) = "+rf-lfl-fr+" ++ reemplaça rest

    reemplaça ('r' : rest) = "-lf+rfr+fl-" ++ reemplaça rest

    reemplaça (c : rest) = c : reemplaça rest



    tradueix :: String -> Comanda

    tradueix [] = Para

    tradueix ('f' : rest) = Avança 100 :#: tradueix rest

    tradueix ('l' : rest) = Avança 10 :#: tradueix rest

    tradueix ('r' : rest) = Avança 10 :#: tradueix rest

    tradueix ('+' : rest) = Gira 90 :#: tradueix rest

    tradueix ('-' : rest) = Gira (-90) :#: tradueix rest



-- Donat el nivell del fractal(recursivitat) retorna una comanda per dibuixar la figura hilbert,

-- utilitzant la funció lHilbert. (aquesta és la que s'utilitza)

hilbert :: Int -> Comanda

hilbert n = lHilbert n 



-- Funció que donat el nivell de recursivitat del fractal, utilitza la gramàtica determinada per generar

-- la figura hilbert mitjançant una comanda i la retorna. A partir de la gramàtica es substitueix cada element

-- per la seva comanda corresponent, utilitzant la recursivitat per implementar la substitució de "l"i "r" i 

-- expandir-les segons la seva definició gramatical. S'implementa la comanda corresponent codificada 

-- per "-", "+", "l", "r" i "f". Per implementar la comanda que sustitueix la "l" s'utilitza la funció lHilbert,

-- per "r" s'utilitza la funció rHilbert i per "f" la comanda fHilbert.

lHilbert :: Int -> Comanda

lHilbert 0 = CanviaColor vermell :#: Avança 10

lHilbert n = Gira 90 :#: rHilbert (n - 1) :#: fHilbert :#: Gira (-90) :#: lHilbert (n - 1) :#: fHilbert :#: lHilbert (n - 1) :#: Gira (-90) :#: fHilbert :#: rHilbert (n - 1) :#: Gira 90



-- Donat el nivell de recursivitat del fractal, retorna una comanda que implementa l'element de la gramàtica "r".

-- Utilitzant la recursivitat per implementar la substitució de l'element "r", i utilitzant les altres funcions o 

-- la comanda per les substitucions de "l" i "f".

rHilbert :: Int -> Comanda  

rHilbert 0 = CanviaColor blau :#: Avança 10    -- Darrer nivell de recusivitat

rHilbert n = Gira (-90) :#: lHilbert (n - 1) :#: fHilbert :#: Gira 90 :#: rHilbert (n - 1) :#: fHilbert :#: rHilbert (n - 1) :#: Gira 90 :#: fHilbert :#: lHilbert (n - 1) :#: Gira (-90)



-- Comanda composta per CanviaColor a verd i Avança 10

fHilbert :: Comanda   

fHilbert = CanviaColor verd :#: Avança 10



-- Problema 13



-- Donat el nivell del fractal(recursivitat) retorna una comanda per dibuixar la figura fletxa.

-- (aquesta funció no s'utilitza, no funcional)

fletxa :: Int -> Comanda

fletxa n = fFletxa n

fletxa' :: Int -> Comanda

fletxa' n = reescriptura "f" n

  where

    reescriptura :: String -> Int -> Comanda

    reescriptura c 0 = tradueix c

    reescriptura c n = reescriptura (reemplaça c) (n - 1)

    reemplaça :: String -> String

    reemplaça [] = []

    reemplaça ('f' : rest) = "g+f+g" ++ reemplaça rest

    reemplaça ('g' : rest) = "f-g-f" ++ reemplaça rest

    reemplaça (c : rest) = c : reemplaça rest

    tradueix :: String -> Comanda

    tradueix [] = Para

    tradueix ('f' : rest) = Avança 10 :#: tradueix rest

    tradueix ('g' : rest) = Avança 10 :#: tradueix rest

    tradueix ('+' : rest) = Gira 60 :#: tradueix rest

    tradueix ('-' : rest) = Gira (-60) :#: tradueix rest



-- Donat el nivell del fractal(recursivitat) retorna una comanda per dibuixar la figura fletxa,

-- utilitzant la funció gFletxa per implementar la substitució de "g" i es substitueix de manera

--  recursiva les "f". També substitueix "+" per la comanda corresponent (aquesta és la que s'utilitza)

fFletxa :: Int -> Comanda

fFletxa 0 = CanviaColor blau :#: Avança 10    -- Darrer nivell de recusivitat

fFletxa n = gFletxa (n - 1) :#: Gira 60 :#: fFletxa (n - 1) :#: Gira 60 :#: gFletxa (n - 1)



-- Funció que donat el nivell de recursivitat del fractal, utilitza la gramàtica determinada per generar

-- la figura fletxa mitjançant una comanda i la retorna. A partir de la gramàtica es substitueix cada element

-- per la seva comanda corresponent, utilitzant la recursivitat per implementar la substitució de "g" i 

-- expandir-la segons la seva definició gramatical. S'implementa la comanda corresponent codificada 

-- per "-" i "+" i "f". Per implementar la comanda que sustitueix a "f" s'utilitza la funció fFletxa.

gFletxa :: Int -> Comanda

gFletxa 0 = CanviaColor vermell :#: Avança 10    -- Darrer nivell de recusivitat

gFletxa n = fFletxa (n - 1) :#: Gira (-60) :#: gFletxa (n - 1) :#: Gira (-60) :#: fFletxa (n - 1)



-- Problema 14



-- Donat el nivell del fractal(recursivitat) retorna una comanda per dibuixar la figura fletxa.

-- (aquesta funció no s'utilitza, no funcional)

branca' :: Int -> Comanda

branca' n = reescriptura "g" n

  where

    reescriptura :: String -> Int -> Comanda

    reescriptura c 0 = tradueix c

    reescriptura c n = reescriptura (reemplaça c) (n - 1)

    reemplaça :: String -> String

    reemplaça [] = []

    reemplaça ('g' : rest) = "f-[[g]+g]+f[+fg]-g" ++ reemplaça rest

    reemplaça ('f' : rest) = "ff" ++ reemplaça rest

    reemplaça (c : rest) = c : reemplaça rest

    tradueix :: String -> Comanda

    tradueix [] = Para

    tradueix ('f' : rest) = Avança 10 :#: tradueix rest

    tradueix ('g' : rest) = Avança 10 :#: tradueix rest

    tradueix ('[' : rest) = Branca (tradueix rest)

    tradueix (']' : rest) = tradueix rest

    tradueix ('+' : rest) = Gira 22.5 :#: tradueix rest

    tradueix ('-' : rest) = Gira (-22.5) :#: tradueix rest



-- Donat el nivell del fractal(recursivitat) retorna una comanda per dibuixar la figura branca,

-- utilitzant la funció gBranca. (aquesta és la que s'utilitza)

branca :: Int -> Comanda

branca n = gBranca n



-- Funció que donat el nivell de recursivitat del fractal, utilitza la gramàtica determinada per generar

-- la figura branca mitjançant una comanda i la retorna. A partir de la gramàtica es substitueix cada element

-- per la seva comanda corresponent, utilitzant la recursivitat per implementar la substitució de "g" i 

-- expandir-la segons la seva definició gramatical. S'implementa la comanda corresponent codificada 

-- per "-" i "+" i "f". Per implementar la comanda que sustitueix a "f" s'utilitza la funció fBranca.

gBranca :: Int -> Comanda

gBranca 0 = CanviaColor verd :#: Avança 10    -- Darrer nivell de recusivitat

gBranca n = fBranca (n-1) :#: Gira (-22.5) :#: Branca (Branca (gBranca (n-1)) :#: Gira 22.5 :#: gBranca (n-1)) :#: Gira 22.5 :#: fBranca (n-1) :#: Branca (Gira 22.5 :#: fBranca (n-1) :#: gBranca (n-1)) :#: Gira (-22.5) :#: gBranca (n-1) 



-- Donat el nivell de recursivitat del fractal, retorna una comanda que implementa l'element de la gramàtica "f".

-- Utilitzant la recursivitat si es crida fBranca per un nivell de fractal > 0.

fBranca :: Int -> Comanda

fBranca 0 = CanviaColor marro :#: Avança 10

fBranca n = fBranca (n-1) :#: fBranca (n-1)


-- Opcional Nova Figura amb Branques | Falguera

-- Donat el nivell del fractal(recursivitat) retorna una comanda per dibuixar la figura Falguera,
-- utilitzant la funció rFalguera.
falguera :: Int -> Comanda
falguera n = rFalguera n

-- Funció que donat el nivell de recursivitat del fractal, utilitza la gramàtica determinada per generar
-- la figura Falguera mitjançant una comanda i la retorna. A partir de la gramàtica es substitueix cada element
-- per la seva comanda corresponent, utilitzant la recursivitat per implementar la substitució de "l"i "r" i 
-- expandir-les segons la seva definició gramatical. S'implementa la comanda corresponent codificada 
-- per "-", "+", "l" i "f". Per implementar la comanda que sustitueix la "l" s'utilitza la funció lFalguera i per "f" la comanda fFalguera.
lFalguera :: Int -> Comanda
lFalguera 0 = CanviaColor verd :#: Avança 10
lFalguera n = lFalguera (n - 1) :#: Branca (Gira (-25.7) :#: fFalguera :#: fFalguera :#: fFalguera) :#: Branca (Gira 25.7 :#: fFalguera :#: fFalguera :#: fFalguera) :#: fFalguera :#: lFalguera (n -1)

-- Donat el nivell de recursivitat del fractal, retorna una comanda que implementa l'element de la gramàtica "r".
-- Utilitzant la recursivitat per implementar la substitució de l'element "r", i utilitzant les altres funcions o 
-- la comanda per les substitucions de "l" i "f".
rFalguera :: Int -> Comanda
rFalguera 0 = CanviaColor verd :#: Avança 10
rFalguera n = rFalguera (n - 1) :#: fFalguera :#: lFalguera (n - 1) :#: Branca (Gira 25.7 :#: rFalguera (n - 1)) :#: Branca ( Gira (-25.7) :#: rFalguera (n - 1))

-- Donat el nivell de recursivitat del fractal, retorna una comanda que implementa l'element de la gramàtica "f".
-- Utilitzant la recursivitat si es crida fBranca per un nivell de fractal > 0.
fFalguera :: Comanda
fFalguera = CanviaColor marro :#: Avança 10


-- Opcional Nova Figura amb Branques | Arbust

-- Donat el nivell del fractal(recursivitat) retorna una comanda per dibuixar la figura Arbust,
-- utilitzant les funcions rArbust, lArbust i fArbust (3 cops).
arbust :: Int -> Comanda
arbust n = rArbust n :#: lArbust n :#: fArbust :#: fArbust :#: fArbust

-- Donat el nivell de recursivitat del fractal, retorna una comanda que implementa l'element de la gramàtica "r".
-- Utilitzant la recursivitat per implementar la substitució de l'element "r", i utilitzant les altres funcions o 
-- la comanda per les substitucions de "q", "r" i "s".
rArbust :: Int -> Comanda
rArbust 0 = CanviaColor marro :#: Avança 10
rArbust n = Branca (Gira 20 :#: Gira 20 :#: Gira 20 :#: qArbust (n - 1)) :#: Branca (Gira (-20) :#: Gira (-20) :#: Gira (-20) :#: qArbust(n - 1)) :#: sArbust(n - 1) :#: rArbust(n - 1) 

-- Donat el nivell de recursivitat del fractal, retorna una comanda que implementa l'element de la gramàtica "l".
-- Utilitzant la recursivitat per implementar la substitució de l'element "f".
lArbust :: Int -> Comanda
lArbust 0 = CanviaColor marro :#: Avança 10
lArbust n = Branca (Gira (-20) :#: fArbust :#: fArbust :#: fArbust) :#: Branca (Gira 20 :#: fArbust :#: fArbust :#: fArbust) :#: fArbust

-- Donat el nivell de recursivitat del fractal, retorna una comanda que implementa l'element de la gramàtica "q".
-- Utilitzant la recursivitat per implementar la substitució de l'element "l", "q" i "t".
qArbust :: Int -> Comanda
qArbust 0 = CanviaColor marro :#: Avança 10
qArbust n = Gira 20 :#: tArbust (n - 1) :#: Branca (Gira (-20) :#: qArbust(n - 1)) :#: lArbust(n - 1)

-- Donat el nivell de recursivitat del fractal, retorna una comanda que implementa l'element de la gramàtica "t".
-- Utilitzant la recursivitat per implementar la substitució de l'element "l", "q" i "t".
tArbust :: Int -> Comanda
tArbust 0 = CanviaColor marro :#: Avança 10
tArbust n = Gira (-20) :#: qArbust (n - 1) :#: Branca (Gira 20 :#: tArbust(n - 1)) :#: lArbust(n - 1)

-- Donat el nivell de recursivitat del fractal, retorna una comanda que implementa l'element de la gramàtica "s".
-- Utilitzant la recursivitat per implementar la substitució de l'element "s", i "l".
sArbust :: Int -> Comanda
sArbust 0 = CanviaColor marro :#: Avança 10
sArbust n = sArbust(n - 1) :#: lArbust(n - 1)

-- Donat el nivell de recursivitat del fractal, retorna una comanda que implementa l'element de la gramàtica "f".
-- Utilitzant la recursivitat si es crida fBranca per un nivell de fractal > 0.
fArbust :: Comanda
fArbust = CanviaColor marro :#: Avança 10
