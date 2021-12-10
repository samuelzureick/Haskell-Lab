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

--

ex2_1_1_i = allWhite 1

ex2_1_1_o = allWhite 1

ex2_1_2_i = allBlack 1

ex2_1_2_o = allWhite 1

ex2_1_3_i = clockwise (allWhite 1) (allWhite 1) (allWhite 1) (allWhite 1)

ex2_1_3_o = clockwise (allWhite 1) (allWhite 1) (allWhite 1) (allWhite 1)

ex2_1_4_i = clockwise (allBlack 1) (allBlack 1) (allBlack 1) (allBlack 1)

ex2_1_4_o = clockwise (allWhite 1) (allWhite 1) (allWhite 1) (allWhite 1)

ex2_1_5_i = clockwise (allWhite 1) (allBlack 1) (allWhite 1) (allWhite 1)

ex2_1_5_o = clockwise (allBlack 1) (allBlack 1) (allBlack 1) (allBlack 1)

ex2_1_6_i = clockwise (allBlack 1) (allWhite 1) (allBlack 1) (allWhite 1)

ex2_1_6_o = clockwise (allBlack 1) (allBlack 1) (allBlack 1) (allBlack 1)

ex2_1_7_i = clockwise (allWhite 1) (allBlack 1) (allBlack 1) (allBlack 1)

ex2_1_7_o = clockwise (allBlack 1) (allBlack 1) (allBlack 1) (allBlack 1)

-----

t1_1 = ex2_1_1_o == (ndiff ex2_1_1_i)
t1_2 = ex2_1_2_o == (ndiff ex2_1_2_i)
t1_3 = ex2_1_3_o == (ndiff ex2_1_3_i)
t1_4 = ex2_1_4_o == (ndiff ex2_1_4_i)
t1_5 = ex2_1_5_o == (ndiff ex2_1_5_i)
t1_6 = ex2_1_6_o == (ndiff ex2_1_6_i)
t1_7 = ex2_1_7_o == (ndiff ex2_1_7_i)

ifF b s = if b then "" else s

t1 = t1_1 && t1_2 && t1_3 && t1_4 && t1_5 && t1_6 && t1_7

t1f = (ifF t1_1 "ex2_1_1_i, ") ++ (ifF t1_2 "ex2_1_2_i, ")  ++ (ifF t1_3 "ex2_1_3_i, ")
    ++ (ifF t1_4 "ex2_1_4_i, ") ++ (ifF t1_5 "ex2_1_5_i, ") ++ (ifF t1_6 "ex2_1_6_i, ") ++ (ifF t1_7 "ex2_1_7_i, ")
--


-----

ex2_2_1_i = clockwise (allWhite 2)
                      (clockwise (allWhite 1) (allBlack 1) (allWhite 1) (allWhite 1))
                      (allWhite 2)
                      (allWhite 2)

--

ex2_2_1_o = (clockwise(allWhite 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allWhite 2)(allWhite 2))

--

ex2_2_2_i = clockwise (allWhite 2)
                      (clockwise (allBlack 1) (allWhite 1) (allWhite 1) (allWhite 1))
                      (allWhite 2)
                      (allWhite 2)
---

ex2_2_2_o = (clockwise(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allWhite 2)(allWhite 2))

---

ex2_2_3_i = clockwise (allWhite 2)
                      (clockwise (allBlack 1) (allWhite 1) (allWhite 1) (allWhite 1))
                      (allWhite 2)
                      (clockwise (allWhite 1) (allWhite 1) (allWhite 1) (allBlack 1))
---

ex2_2_3_o = (clockwise(allBlack 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allWhite 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))

---
ex2_2_4_i = clockwise (clockwise (allWhite 1) (allWhite 1) (allBlack 1) (allWhite 1))
                      (clockwise (allWhite 1) (allWhite 1) (allBlack 1) (allBlack 1))
                      (allBlack 2)
                      (clockwise (allWhite 1) (allBlack 1) (allBlack 1) (allWhite 1))
---

ex2_2_4_o = (clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(allWhite 2)(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))

--

t2_1 = ex2_2_1_o == (ndiff ex2_2_1_i)
t2_2 = ex2_2_2_o == (ndiff ex2_2_2_i)
t2_3 = ex2_2_3_o == (ndiff ex2_2_3_i)
t2_4 = ex2_2_4_o == (ndiff ex2_2_4_i)

t2 = t2_1 && t2_2 && t2_3 && t2_4

t2f = (ifF t2_1 "ex2_2_1_i, ") ++ (ifF t2_2 "ex2_2_2_i, ")  ++ (ifF t2_3 "ex2_2_3_i, ")   ++ (ifF t2_4 "ex2_2_4_i, ")

--

ex2_3_i = (clockwise(clockwise(clockwise(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allWhite 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allWhite 1)(allWhite 1)))(clockwise(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allWhite 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allWhite 1)(allWhite 1)))(clockwise(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allBlack 1)(allWhite 1)(allWhite 1)(allWhite 1))(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allBlack 1)(allWhite 1)(allWhite 1)(allWhite 1))(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allBlack 1))))(clockwise(clockwise(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allWhite 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allWhite 1)(allWhite 1)))(clockwise(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allWhite 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allWhite 1)(allWhite 1)))(clockwise(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allBlack 1)(allWhite 1)(allWhite 1)(allWhite 1))(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allBlack 1)(allWhite 1)(allWhite 1)(allWhite 1))(clockwise(allBlack 1)(allWhite 1)(allBlack 1)(allBlack 1))))(clockwise(clockwise(clockwise(allWhite 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1)))(clockwise(clockwise(allWhite 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1)))(clockwise(clockwise(allWhite 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allWhite 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allWhite 1)(allWhite 1)(allBlack 1)))(clockwise(clockwise(allWhite 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allWhite 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allWhite 1)(allWhite 1)(allBlack 1))))(clockwise(clockwise(clockwise(allWhite 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1)))(clockwise(clockwise(allWhite 1)(allWhite 1)(allBlack 1)(allWhite 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allBlack 1)(allWhite 1)(allBlack 1)))(clockwise(clockwise(allWhite 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allWhite 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allWhite 1)(allWhite 1)(allBlack 1)))(clockwise(clockwise(allWhite 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allWhite 1))(clockwise(allWhite 1)(allWhite 1)(allWhite 1)(allBlack 1))(clockwise(allWhite 1)(allWhite 1)(allWhite 1)(allBlack 1)))))

ex2_3_o = (clockwise(clockwise(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))))(clockwise(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))))(clockwise(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))))(clockwise(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))(clockwise(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1))(clockwise(allBlack 1)(allBlack 1)(allBlack 1)(allBlack 1)))))

t3 = ex2_3_o == (ndiff ex2_3_i)

t3f = ifF t3 "ex2_3_i."

fails = t1f ++ t2f ++ t3f

--- the section below runs some diagnostic tests which check for alternative interpretations
--- of the lab script which were not intended but for which I do not want to penalize students.
--- if one of the diagnostic tests passes and many normal tests fail, you should contact me to manually review your code.
--- most students should simply ignore this part of the file. In fact, if it causes errors,
--- you can just delete it! Look out for the comment below signalling the end of the diagnostic section.

d2a = clockwise (allWhite 2)
                (clockwise (allWhite 1) (allWhite 1) (allWhite 1) (allBlack 1))
                (allWhite 2)
                (allWhite 2)
d2b = clockwise (clockwise (allWhite 1) (allBlack 1) (allBlack 1) (allWhite 1))
                (clockwise (allBlack 1) (allBlack 1) (allBlack 1) (allBlack 1))
                (clockwise (allWhite 1) (allBlack 1) (allWhite 1) (allWhite 1))
                (clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1))

diag1 = allBlack 4 == clockwise (allBlack 1) (allBlack 1) (allBlack 1) (allBlack 1)
diag2 = d2b == (ndiff d2a)

diagmessage = if diag1 then
                 (if diag2 then "\n NOTE: If several tests above fail, please contact me (Joe) to manually review your code as a special circumstance may have arisen" else "\n NOTE: If several tests above fail, please contact me (Joe) to manually review your code as a special circumstance may have arisen.\n However, one thing you could try is using your implementation of clockwise to construct the output of ndiff rather than using your quadtree constructor directly,\n in the case when you need to combine for subtrees.\n The reason is that it looks like your implementation of clockwise optimises the tree structure.\n Email me if you are not sure.")
                           else ""

-- END OF DIAGNOSTIC SECTION

main = putStrLn("Running tests related to exercise 2, please run your own tests too, as the inputs used for marking may be different") >>
       putStrLn("(However, in most cases the output below is a good guide to the first three marks of ex 2)") >>
       putStr("Running tests on 1 by 1 and 2 by 2 quadtrees...") >>
       putStr(if t1 then "passed!\n" else "FAILED!\n") >>
       putStr("Running tests on small quadtrees...............") >>
       putStr(if t2 then "passed!\n" else "FAILED!\n") >>
       putStr("Running test on a uniform, medium quadtree.....") >>
       putStr(if t3 then "passed!\n" else "FAILED!\n") >>
       putStr(if not(t1 && t2 && t3) then ("For the failing cases above, the inputs to look at are:\n" ++ fails) else "\n") >>
       putStrLn(diagmessage)
