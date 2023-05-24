module UdGraphic (
    Comanda(..),
    Distancia,
    Angle,
    execute, display, blanc, vermell, verd, blau, negre, marro
    )
    where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
import Data.IORef
import Data.List
import Control.Monad( liftM, liftM2, liftM3 )
import System.Random
import Test.QuickCheck

infixr 5 :#:

-- Punts

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)

sizeToPoint :: Size -> Pnt
sizeToPoint (Size x y) = Pnt (fromIntegral x) (fromIntegral y)

-- Colors
data Llapis = Color' GL.GLfloat GL.GLfloat GL.GLfloat
            | Transparent
            deriving (Eq, Ord, Show)


pencilToRGB :: Llapis -> GL.Color3 GL.GLfloat
pencilToRGB (Color' r g b)  =  GL.Color3 r g b
pencilToRGB Transparent  =  error "pencilToRGB: transparent"



blanc, negre, vermell, verd, blau :: Llapis
blanc   = Color' 1.0 1.0 1.0
negre   = Color' 0.0 0.0 0.0
vermell = Color' 1.0 0.0 0.0
verd    = Color' 0.0 1.0 0.0
blau    = Color' 0.0 0.0 1.0
marro   = Color' 0.6 0.3 0.0


-- Lines
data Ln = Ln Llapis Pnt Pnt
  deriving (Eq,Ord,Show)


-- Window parameters
theCanvas :: Pnt
theCanvas  =  Pnt 800 800

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = pencilToRGB blanc


-- Main drawing and window functions

display :: Comanda -> IO ()
display c = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "pencilcil Graphics"
  displayCallback $= draw c
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
  --actionOnWindowClose $= ContinueExectuion
  draw c
  mainLoop

draw :: Comanda -> IO ()
draw c = do clear [ColorBuffer]
            loadIdentity
            background
            toGraphic $ rescale $ execute c
            swapBuffers


toGraphic :: [Ln] -> IO ()
toGraphic lines  = sequence_ (map f lines)
  where
  f (Ln pencil startP endP)  =
    GL.color (pencilToRGB pencil) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)


background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]

toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)


-- Definició de les comandes per moure el llapis
type Angle     = Float
type Distancia = Float
data Comanda   = Avança Distancia
               | Gira Angle
               | Comanda :#: Comanda
               | Para
               | CanviaColor Llapis
               | Branca Comanda
    

--Sobrecaregem la funció Show per el tipus Comanda
instance Show Comanda where
  show (Avança d)       = "Avança " ++ show d
  show (Gira a)         = "Gira " ++ show a
  show (p :#: q)        = show p ++ " :#: " ++ show q
  show Para             = "Para"
  show (CanviaColor l)  = "Color " ++ show l
  show (Branca c)       = "Branca (" ++ show c ++ ")"


-- Sobrecaregem la funció Eq per al tipus Comanda
instance Eq Comanda where
  (Avança d1) == (Avança d2) = d1 == d2
  (Gira a1) == (Gira a2) = a1 == a2
  (p1 :#: q1) == (p2 :#: q2) = p1 == p2 && q1 == q2
  Para == Para = True
  (CanviaColor l1) == (CanviaColor l2) = l1 == l2 
  (Branca c1) == (Branca c2) = c1 == c2
  _ == _ = False


-- Problema 8
-- Pas de comandes a lines a pintar per GL graphics

execute :: Comanda -> [Ln]
execute comanda = result
  -- es crida a execute' per poder fer una funcio recursiva que retorni
  -- una tupla amb variables necessaries per a la recursivitat
  where
    (result, (llapis,angle,pnt)) = execute' comanda (negre, 0, Pnt 0 0)

-- Donats una Comanda i una tupla de variables retorna la linia a pintar i la propia tupla
execute' :: Comanda -> (Llapis,Angle,Pnt) -> ([Ln], (Llapis,Angle,Pnt))

-- Si la comanda es Avança: calcula el nou punt i retorna la linia i tupla actualitzada amb el nou punt
execute' (Avança d) (llapis, angle, pnt@(Pnt x y)) = ([Ln llapis pnt endpnt], (llapis, angle, endpnt))
  where
    endpnt = Pnt (x + d * cos (toRadians angle)) (y - d * sin (toRadians angle))

-- Si la comanda es Gira: no es retorna cap linia per dibuixar, només s'actualiza l'angle de la tupla
execute' (Gira a) (llapis, angle, pnt) = ([], (llapis, (angle + a), pnt))

-- Si la comanda es Para: no es retorna cap linia per dibuixar i es torna la tupla sense modificar
execute' Para (llapis,angle,pnt) = ([], (llapis,angle,pnt))

-- Si la comanda es CanviaColor: no es retorna cap linia per dibuixar, s'actualitza el color de la tupla
execute' (CanviaColor llapis) (_, angle, pnt) =  ([],(llapis, angle, pnt))

-- Si la comanda es Branca: es retorna la linia a dibuixar que surt d'executar la comanda associada a la branca,
-- i la tupla d'entrada sense actualitzar el seu estat
execute' (Branca c) (llapis,angle,pnt) = (ln, (llapis,angle,pnt))
  where
    (ln, (llapisBranca,angleBranca,pntBranca)) = execute' c (llapis,angle,pnt)

-- Si la comanda es un conjunt de dues comandes "c" i "d": retorna la linia que surt de concatenar les linies
-- resultants de les dues comandes "c" i després "d", i retorna la tupla amb l'estat actualitzat per la segona comanda
execute' (c :#: d) (llapis,angle,pnt) = (lnC ++ lnD, (dllapis,dangle,dpnt))
  where
    (lnC, (cllapis,cangle,cpnt)) = execute' c (llapis,angle,pnt)
    (lnD, (dllapis,dangle,dpnt)) = execute' d (cllapis,cangle,cpnt)

-- Converteix un angle en graus a radians
toRadians :: Angle -> Angle
toRadians angle = angle * pi / 180


-- Rescales all points in a list of lines
--  from an arbitrary scale
--  to (-1.-1) - (1.1)

rescale :: [Ln] -> [Ln]
rescale lines | points == [] = []
              | otherwise    = map f lines
  where
  f (Ln pencil p q)  =  Ln pencil (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pencil p q <- lines, r <- [p, q] ]
  hi              =  foldr1 lub points
  lo              =  foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar (0.55)
  p0              =  (hi + lo) * scalar (0.5)
  swap (Pnt x y)  =  Pnt y x


-- Generators for QuickCheck

instance Arbitrary Llapis where
    arbitrary  =  sized pencil
        where
          pencil n  =  elements [negre,vermell,verd,blau,blanc,Transparent]


instance Arbitrary Comanda where
    arbitrary  =  sized cmd
        where
          cmd n  |  n <= 0     =  oneof [liftM (Avança . abs) arbitrary,
                                         liftM Gira arbitrary ]
                 |  otherwise  =  liftM2 (:#:) (cmd (n `div` 2)) (cmd (n `div`2))

                 
