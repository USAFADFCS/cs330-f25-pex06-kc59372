-- pex6.hs 
-- unKnot Haskell
-- name: Kaci McBrayer
{- DOCUMENTATION: I used some test cases provided by C2C Gavin Smith in the CS330 Teams page. I also 
   used https://www.haskelltutorials.com/guides/haskell-lists-ultimate-guide.html to understand how 
   to pattern match the list in each function. I referenced https://hoogle.haskell.org/?hoogle=init
   to search different built in functions like init. Finally, I discussed how to implement a Type II 
   knot checker and remover with C1C Zarif Bin Mohd Zamrin at a high level, specifially the idea to 
   use helper functions to find under and over matches later in the list.-}

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "not a knot"
   | typeIknotExists tripCode = unKnot (removetypeIKnot tripCode)
   | typeIwrapExists tripCode = unKnot (removetypeIWrap tripCode)
   | typeIIknotExists tripCode = unKnot (removetypeIIKnot tripCode)
   -- Sorry I couldn't figure out typeII wraps :(
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

typeIknotExists :: [(Char, Char)] -> Bool
typeIknotExists [] = False
typeIknotExists [_] = False
typeIknotExists ((c1,t1):(c2,t2):restofTripCode)
   | c1 == c2 = True
   | otherwise = typeIknotExists ((c2,t2):restofTripCode)

removetypeIKnot :: [(Char, Char)] -> [(Char, Char)]
removetypeIKnot [] = []
removetypeIKnot [_] = []
removetypeIKnot ((c1,t1):(c2,t2):restofTripCode)
    | c1 == c2  = restofTripCode 
    | otherwise = (c1,t1) : removetypeIKnot ((c2,t2):restofTripCode)

typeIwrapExists :: [(Char, Char)] -> Bool
typeIwrapExists [] = False
typeIwrapExists [_] = False
typeIwrapExists tripCode = fst (head tripCode) == fst (last tripCode)

removetypeIWrap :: [(Char, Char)] -> [(Char, Char)]
removetypeIWrap [] = []
removetypeIWrap [_] = []
removetypeIWrap tripCode = tail (init tripCode)

typeIIknotExists :: [(Char, Char)] -> Bool
typeIIknotExists [] = False
typeIIknotExists [_] = False
typeIIknotExists ((c1,'o'):(c2,'o'):restofTripCode) 
    | underpairExists c1 c2 restofTripCode = True
    | otherwise = typeIIknotExists ((c2,'o'):restofTripCode)
typeIIknotExists ((c1,'u'):(c2,'u'):restofTripCode) 
    | overpairExists c1 c2 restofTripCode = True
    | otherwise = typeIIknotExists ((c2,'u'):restofTripCode)
typeIIknotExists (_:restofTripCode) = typeIIknotExists restofTripCode

underpairExists :: Char -> Char -> [(Char, Char)] -> Bool
underpairExists _ _ [] = False
underpairExists _ _ [_] = False
underpairExists c1 c2 ((x1,'u'):(x2,'u'):restofTripCode)
    | (c1 == x1 && c2 == x2) = True
    | (c1 == x2 && c2 == x1) = True
    | otherwise = underpairExists c1 c2 ((x2,'u'):restofTripCode)
underpairExists c1 c2 (_:restofTripCode) = underpairExists c1 c2 restofTripCode

overpairExists :: Char -> Char -> [(Char, Char)] -> Bool
overpairExists _ _ [] = False
overpairExists _ _ [_] = False
overpairExists c1 c2 ((x1,'o'):(x2,'o'):restofTripCode)
    | (c1 == x1 && c2 == x2) = True
    | (c1 == x2 && c2 == x1) = True
    | otherwise = overpairExists c1 c2 ((x2,'o'):restofTripCode)
overpairExists c1 c2 (_:restofTripCode) = overpairExists c1 c2 restofTripCode

removetypeIIKnot :: [(Char, Char)] -> [(Char, Char)]
removetypeIIKnot [] = []
removetypeIIKnot [x] = [x]
removetypeIIKnot ((c1,'o'):(c2,'o'):restoftripCode) 
    | underpairExists c1 c2 restoftripCode = removeunderPair c1 c2 restoftripCode
    | otherwise = (c1,'o') : removetypeIIKnot ((c2,'o'):restoftripCode)
removetypeIIKnot ((c1,'u'):(c2,'u'):restoftripCode) 
    | overpairExists c1 c2 restoftripCode = removeoverPair c1 c2 restoftripCode
    | otherwise = (c1,'u') : removetypeIIKnot ((c2,'u'):restoftripCode)
removetypeIIKnot (x:restoftripCode) = x : removetypeIIKnot restoftripCode

removeunderPair :: Char -> Char -> [(Char, Char)] -> [(Char, Char)]
removeunderPair _ _ [] = []
removeunderPair _ _ [x] = [x]
removeunderPair c1 c2 ((x1,'u'):(x2,'u'):restoftripCode)
    | (c1 == x1 && c2 == x2) = restoftripCode 
    | (c1 == x2 && c2 == x1) = restoftripCode 
    | otherwise = (x1,'u') : removeunderPair c1 c2 ((x2,'u'):restoftripCode)
removeunderPair c1 c2 (x:restoftripCode) = x : removeunderPair c1 c2 restoftripCode

removeoverPair :: Char -> Char -> [(Char, Char)] -> [(Char, Char)]
removeoverPair _ _ [] = []
removeoverPair _ _ [x] = [x]
removeoverPair c1 c2 ((x1,'o'):(x2,'o'):restoftripCode)
    | (c1 == x1 && c2 == x2) = restoftripCode 
    | (c1 == x2 && c2 == x1) = restoftripCode 
    | otherwise = (x1,'o') : removeoverPair c1 c2 ((x2,'o'):restoftripCode)
removeoverPair c1 c2 (x:restoftripCode) = x : removeoverPair c1 c2 restoftripCode

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("test case t01 - tripcode: " )
   print(t01)
   print("result: " ++ unKnot t01)  -- "not a knot"

   let t02 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
   print("test case t02 - tripcode: " ) 
   print(t02)
   print("result: " ++ unKnot t02) -- "not a knot"
 
   let t03 = [('a','u'),('b','u'),('a','o'),('b','o')]
   print("test case t03 - tripcode: " )
   print(t03)
   print("result: " ++ unKnot t03) -- "not a knot"

   let t04 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("test case t04 - tripcode: " )
   print(t04)
   print("result: " ++ unKnot t04)  -- "not a knot"

   let t05 = [('a','o'),('q','u'), ('a','u')]
   print("test case t05 - tripcode: " )
   print(t05)
   print("result: " ++ unKnot t05) -- [('q','u')]

   let t06 = [('a','o'),('a','u'), ('q','u')]
   print("test case t06 - tripcode: " )
   print(t06)
   print("result: " ++ unKnot t06) -- [('q','u')]

   let t07 = [('a','o'),('b','o'), ('a','u'), ('b','u'),('q','u')]
   print("test case t07 - tripcode: " )
   print(t07)
   print("result: " ++ unKnot t07) -- [('q','u')]

   let t08 = [('b','o'), ('a','u'), ('b','u'),('q','u'), ('a','o')]
   print("test case t08 - tripcode: " )
   print(t08)
   print("result: " ++ unKnot t08) -- [('q','u')]
 
   let t09 = [('a','u'),('b','o'), ('a','o'), ('q','u'),('b','u'),('c','o'),('c','u')]
   print("test case t09 - tripcode: " )
   print(t09)
   print("result: " ++ unKnot t09) -- [('a','u'),('b','o'), ('a','o'), ('q','u'),('b','u')]
 
   let t10 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('q','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("test case t10 - tripcode: " )
   print(t10)
   print("result: " ++ unKnot t10) -- [('q','u')]