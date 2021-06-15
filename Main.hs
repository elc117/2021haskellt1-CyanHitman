import Text.Printf
   
type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)

greenPalette :: Int -> [(Int,Int,Int)]
greenPalette 8 = [(0, 80+i*10, 0) | i <- [0..8] ]

rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(0,0,205),(139,0,0),(139,0,139)]

genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [((m*(h+gap), 250), w, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)
        gap = 45


genCircleInLine :: Int -> [Circle]
genCircleInLine n  = [((p*gp+r, gp+5*p), r) | p <- [0..fromIntegral (n-1)]]
  where r = 10
        gp = 50

svgRect :: Rect -> String -> String 
svgRect ((rx,ry),w,h) style = 
  printf "<rect x='%.2f' y='%.2f' width='%.2f' height='%.2f' style='%s' />\n" rx ry w h style

svgCircle :: Circle -> String -> String 
svgCircle ((cx,cy),r) fill = 
  printf "<circle  cx='%.2f' cy='%.2f' r='%.2f' fill='%s' />\n" cx cy r fill

svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

svgEnd :: String
svgEnd = "</svg>"

svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

main :: IO ()
main = do
  writeFile "figuras.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgRetangles ++ svgCirculos ++ svgEnd
        svgRetangles = svgElements svgRect rects (map svgStyle palette)
        rects = genRectsInLine nrects
        nrects = 6

        svgCirculos = svgElements svgCircle circulo (map svgStyle palette)
        circulo = genCircleInLine ncircs
        ncircs = 9

        
        palette = rgbPalette nrects
        (w,h) = (1440,720)