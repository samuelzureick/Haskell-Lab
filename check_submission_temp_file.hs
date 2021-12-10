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

ndiff :: Square -> Square
ndiff n
 | n == allWhite 1 = BlackSquare False
 | n == allBlack 1 = BlackSquare False
 | n == allWhite 2 = allWhite 2
 | n == allBlack 2 = allWhite 2
 | otherwise = allBlack 2
-- This file is used to test your submission
-- it checks whether your submission will work with the automated testing script
-- but it does not provide many examples.
-- More testing examples will be provided on Blackboard
-- It needs to be pasted after your submission, which is what the script check_submission.hs does
-- before compiling and running

-- testing exercise 1

t1a = clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1) == anticlockwise (allBlack 1) (allWhite 1) (allWhite 1) (allBlack 1)

t1b = clockwise (allBlack 2) (anticlockwise (allBlack 1) (allWhite 1) (allWhite 1) (allBlack 1)) (allWhite 2) (allWhite 2) == anticlockwise (allBlack 2) (allWhite 2) (allWhite 2) (clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1))

t1c = clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1) /= anticlockwise (allWhite 1) (allWhite 1) (allWhite 1) (allBlack 1)

-- texting exercise 2

t2ai = (clockwise(clockwise(clockwise(allBlack 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allWhite 1))(allWhite 2)(allBlack 2))(clockwise(allBlack 2)(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allWhite 1))(clockwise(allBlack 1)(allBlack 1)(allWhite 1)(allWhite 1)))(allWhite 4)(clockwise(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allBlack 1)(allWhite 1)(allWhite 1)(allBlack 1))(allBlack 2)))

t2ao = (clockwise(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2)(allBlack 2))(clockwise(allBlack 2)(allWhite 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(allBlack 4)(clockwise(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allWhite 2)))


t2a = (ndiff t2ai) == t2ao


t2bi = (clockwise(clockwise(clockwise(allBlack 2)(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allWhite 1)(allBlack 1))(allBlack 2))(clockwise(clockwise(allBlack 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allWhite 1)(allWhite 1))(allWhite 2)(allWhite 2))(clockwise(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allWhite 1))(allWhite 2)(allWhite 2))(clockwise(allBlack 2)(allWhite 2)(allWhite 2)(clockwise(allBlack 1)(allWhite 1)(allWhite 1)(allBlack 1))))(clockwise(clockwise(clockwise(allBlack 1)(allBlack 1)(allWhite 1)(allWhite 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allWhite 1))(allWhite 2)(allWhite 2))(clockwise(allBlack 2)(allBlack 2)(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allWhite 1)))(clockwise(clockwise(allWhite 1)(allWhite 1)(allWhite 1)(allBlack 1))(allBlack 2)(clockwise(allWhite 1)(allBlack 1)(allBlack 1)(allWhite 1))(allWhite 2))(clockwise(clockwise(allWhite 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allBlack 1)(allBlack 1)(allWhite 1)(allWhite 1))(allWhite 2)(allWhite 2)))(clockwise(clockwise(allWhite 2)(allWhite 2)(clockwise(allWhite 1)(allWhite 1)(allBlack 1)(allBlack 1))(allWhite 2))(clockwise(allWhite 2)(clockwise(allWhite 1)(allBlack 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allBlack 1)(allBlack 1)(allWhite 1))(allWhite 2))(clockwise(allWhite 2)(clockwise(allWhite 1)(allBlack 1)(allBlack 1)(allWhite 1))(allBlack 2)(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allBlack 1)))(clockwise(allWhite 2)(allBlack 2)(allBlack 2)(clockwise(allWhite 1)(allBlack 1)(allBlack 1)(allBlack 1))))(clockwise(clockwise(clockwise(allBlack 1)(allWhite 1)(allWhite 1)(allBlack 1))(allWhite 2)(allWhite 2)(clockwise(allBlack 1)(allWhite 1)(allWhite 1)(allBlack 1)))(clockwise(allWhite 2)(allWhite 2)(clockwise(allWhite 1)(allWhite 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allWhite 1)(allBlack 1)(allWhite 1)))(clockwise(clockwise(allWhite 1)(allBlack 1)(allBlack 1)(allWhite 1))(clockwise(allBlack 1)(allWhite 1)(allWhite 1)(allBlack 1))(allBlack 2)(allBlack 2))(clockwise(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allBlack 1))(allWhite 2)(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allBlack 1))(allBlack 2))))

t2bo = (clockwise(clockwise(clockwise(allWhite 2)(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2)(allBlack 2))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2)(allBlack 2))(clockwise(allBlack 2)(allBlack 2)(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))))(clockwise(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2)(allBlack 2))(clockwise(allBlack 2)(allWhite 2)(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2)(allBlack 2)))(clockwise(clockwise(allWhite 2)(allWhite 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2))(clockwise(allWhite 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2))(clockwise(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(allBlack 2)(allBlack 2)(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))))(clockwise(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allWhite 2)(allWhite 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(allWhite 2)(allWhite 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2)(allBlack 2))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allBlack 2))))

t2b = (ndiff t2bi) == t2bo

main =     print("Tests running...")
        >> print(if t1a then "Simple check for ex 1 passed!" else "Simple check for ex1 FAILED!")
        >> print(if t1c then "Inequality check for ex 1 passed!" else "Inequality check for ex1 FAILED!")
        >> print(if t1b then "Larger check for ex 1 passed!" else "Larger check for ex1 FAILED!")
        >> print(if t2a then "Small check for ex 2 passed!" else "Small check for ex2 FAILED!")
        >> print(if t2b then "Medium check for ex 2 passed!" else "Medium check for ex2 FAILED!")
--
