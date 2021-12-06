data Square = BlackSquare Bool | CompSquare Square Square Square Square deriving (Eq, Show)

allBlack :: Double -> Square
allBlack 1.0 = BlackSquare True
allBlack 2.0 = clockwise (allBlack( 1.0 )) (allBlack( 1.0 )) (allBlack( 1.0 )) (allBlack( 1.0 ))
allBlack n = clockwise (allBlack( sqrt n)) (allBlack( sqrt n)) (allBlack( sqrt n)) (allBlack( sqrt n)) 

allWhite :: Double -> Square
allWhite 1.0 = BlackSquare False
allWhite 2.0 = clockwise (allWhite( 1.0 )) (allWhite( 1.0)) (allWhite( 1.0 )) (allWhite( 1.0 )) 
allWhite n = clockwise (allWhite( sqrt n)) (allWhite( sqrt n)) (allWhite( sqrt n)) (allWhite( sqrt n)) 

clockwise :: Square->Square->Square->Square -> Square
clockwise w x y z = CompSquare w x y z

anticlockwise :: Square->Square->Square->Square -> Square
anticlockwise w x y z = CompSquare w z y x

-- ndiff :: Square -> Square
