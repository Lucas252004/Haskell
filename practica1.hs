
-- Ejercicio 3 --
sum2 :: (Num a) => [a] -> a
sum2 = foldr (\x rec -> x + rec) 0 

elem2 :: (Eq a) => a -> [a] -> Bool
elem2 n = foldr (\x rec -> x == n || rec) False

concatenar :: [a] -> [a] -> [a]
concatenar xs ys = foldr (\x rec -> x : rec) ys xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f = foldr (\x rec -> if f x then x : rec else rec) []

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\x rec -> f x : rec) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

-- recordar el tipo de foldl
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- aca la funcion es step, c es la tupla y a el valor actual la tupla es = (a, b)
sumasParciales :: Num a => [a] -> [a]
sumasParciales = snd . foldl step (0, [])
  where
    step (acc, lista) x = (acc + x, lista ++ [acc + x]) -- en cada paso actualizo el valor
-- Otra manera de hacerlo (como lo habia pensado)
sumasParciales2 :: Num a => [a] -> [a]
sumasParciales2 = foldr (\x rec -> (x + if null rec then 0 else head rec) : rec) []

sumaAlt :: Num a => [a] -> a
sumaAlt xs = foldr (\x rec i -> x * i + rec(i*(-1))) (const 0) xs 1

sumaAlt2 :: Num a => [a] -> a
sumaAlt2 xs = foldr (\x rec i -> x * i + rec(i*(-1))) (const 0) (reverse xs) 1
-- Ejercicio 4 --
insertar :: a -> [a] -> [[a]] -- Funcion que me devuelve las distintas formas de insertar un elemento en una lista
insertar e xs = [take n xs ++ [e] ++ drop n xs | n <- [0..length xs]]

permutaciones :: [a] -> [[a]]
permutaciones = foldr (\x rec -> concatMap (insertar x) rec) [[]]

partes :: [a] -> [[a]]
partes = foldr (\x rec -> rec ++ map (x:) rec) [[]]

sublistas :: [a] -> [[a]] -- Ver si se puede mejorar
sublistas xs = [] : [take n (drop m xs) | n <-[1..length xs], m <-[0..length xs - n]]
-- Ejercicio 5 --
-- ELEMENTOS EN POSICIONES PARES USA RECURSION PRIMITIVA, ESTO ES DEBIDO A QUE LA CONDICION DEL IF ES NULL XS Y SE ESTA USANDO LA COLA DEL MISMO
entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x rec ys -> if null ys then x : rec [] else x : head ys : rec(tail ys)) id

-- Ejercicio 6 --
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna n = recr (\x xs rec -> if x == n then xs else x : rec) []

-- NO ES FOLDR ADECUADO PARA ESTA FUNCION YA QUE CUANDO SACO UN ELEMENTO DEBO DE DEVOLVER LO QUE QUEDA DE LA LISTA
-- SI USARA FOLDR ELIMINARIA TODAS LAS APARICIONES DEL ELEMENTO EN LA LISTA

-- Ejercicio 7 --
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = foldr (\x res -> uncurry f x : res) []

armarPares :: [a] -> [b] -> [(a,b)]
armarPares = foldr (\x rec (y:ys) -> if null ys then [(x,y)] else (x,y) : rec ys) (const []) 

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\x rec (y:ys) -> f x y : rec ys) (const [])
-- Ejercicio 8 --
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat xs ys = foldr (\(x,y) rec -> zipWith (+) x y : rec) [] (zip xs ys)

trasponer :: [[Int]] -> [[Int]]
trasponer (y:ys) = foldl (\acc x -> zipWith (++) acc (map (:[]) x)) (map (:[]) y) ys
-- Ejercicio 9 --
foldNat :: (a -> a) -> a -> Integer -> a
foldNat f z 0 = z
foldNat f z n = f (foldNat f z (n - 1))

potencia :: Integer -> Integer -> Integer
potencia num = foldNat (*num) 1 
-- Ejercicio 10 --
aplicarNVeces :: (a -> a) -> Integer -> a -> a
aplicarNVeces f 0 e = e
aplicarNVeces f n e = f (aplicarNVeces f (n-1) e)

genLista :: a -> (a -> a) -> Integer -> [a]
genLista e f n = [aplicarNVeces f k e | k <-[0..n-1]]

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta desde hasta = take (fromIntegral hasta - fromIntegral desde + 1) (genLista desde (+1) hasta)
-- Ejercicio 11 --
data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a)
foldPoli :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPoli cX cCte cSuma cProd p = case p of
  X -> cX
  Cte x -> cCte x
  Suma p q -> cSuma (rec p) (rec q)
  Prod p q -> cProd (rec p) (rec q)
  where rec = foldPoli cX cCte cSuma cProd

evaluar :: Num a => a -> Polinomio a -> a
evaluar n = foldPoli n id (+) (*)

-- Ejercicio 12 --
data AB a = Nil | Bin (AB a) a (AB a) deriving Show
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin t = case t of
  Nil -> cNil
  Bin i r d -> cBin (rec i) r (rec d)
  where rec = foldAB cNil cBin

recAB :: b -> (AB a -> AB a -> b -> a -> b -> b) -> AB a -> b
recAB cNil cBin t = case t of
  Nil -> cNil
  Bin i r d -> cBin i d (rec i) r (rec d)
  where rec = recAB cNil cBin

esNil :: AB a -> Bool
esNil t = case t of
  Nil -> True
  Bin _ _ _ -> False

altura :: AB a -> Int
altura = foldAB 0 (\ recI r recD -> 1 + max recI recD)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\recI r recD -> 1 + recI + recD)

mejorSegunNil :: (a -> a -> Bool) -> AB a -> a -> a
mejorSegunNil f Nil x = x
mejorSegunNil f (Bin i r d) x = if f x r then x else r

mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB f = recAB (error "Arbol Vacio") (\i d recI r recD -> mejorSegunNil f i (mejorSegunNil f d r))

mayoresA :: Ord a => a -> AB a -> Bool
mayoresA x = foldAB True (\recI r recD -> recI && r > x && recD)

menoresA :: Ord a => a -> AB a -> Bool
menoresA x = foldAB True (\recI r recD -> recI && r <= x && recD)

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\i d recI r recD -> recI && recD && menoresA r i && mayoresA r d)
-- Ejercicio 13 --
ramas :: AB a -> [[a]]
ramas = foldAB [] (\recI r recD -> if null recI && null recD then [[r]] else map(r:) recI ++ map(r:) recD)

cantHojas :: AB a -> Integer
cantHojas = recAB 0 (\i d recI _ recD -> (if esNil i && esNil d then 1 else 0) + recI + recD)

espejo :: AB a -> AB a
espejo = foldAB Nil (\recI r recD-> Bin recD r recI)

armarParesAB :: AB a -> AB b -> AB (a,b)
armarParesAB = foldAB (const Nil) (\recI r recD t2-> case t2 of 
                                                    Nil -> Nil 
                                                    Bin i2 r2 d2 -> Bin (recI i2) (r,r2) (recD d2))
mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura t1 t2 = cantNodos t1 == cantNodos t2 && cantNodos(armarParesAB t1 t2) == cantNodos t1
-- Ejercicio 14 --
data AIH a = Hoja a | Bi (AIH a) (AIH a)
foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH cHoja cBi t = case t of
  Hoja x -> cHoja x
  Bi i d -> cBi (rec i) (rec d)
  where rec = foldAIH cHoja cBi 

alturaAIH :: AIH a -> Integer
alturaAIH = foldAIH (const 1) (\i d -> 1 + max i d)

tamañoAIH :: AIH a -> Integer
tamañoAIH = foldAIH (const 1) (+)
-- Ejercicio 15 --
data RoseTree a = Rose a [RoseTree a]

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose r rs) = f r (map (foldRose f) rs)

hojas :: RoseTree a -> [a]
hojas = foldRose (\r rec -> if null rec then [r] else concat rec)

distancias :: RoseTree a -> [Int]
distancias t = (foldRose f t) 0
  where f _ rec i = i : concatMap (\g -> g(i+1)) rec

alturaR :: RoseTree a -> Int
alturaR = foldRose (\i rec -> if null rec then 1 else 1 + maximum rec)
-- Ejercicio 17 --
-- El valor de la expresion [ x | x <- [1..3], y <- [x..3], (x + y) `mod' 3 == 0 ] es
-- [1, 3] 
-- Ejercicio 18 --
paresQueSuman :: (Int, Int) -> Int -> Bool
paresQueSuman (x,y) n = x + y == n

paresDeNat :: [(Int, Int)]
paresDeNat = [(x, y) | n <- [0..20], x <- [0..50], y <- [0..50], paresQueSuman (x, y) n] -- Utilizo valores finitos para testear 
-- Ejercicio 19 --
-- La expresion dada en el ejercicio esta mal ya que se va a quedar generando infinitamente valores para A sin incrementar los valores para b y c
-- Una mejor expresion seria la siguiente
pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(a,b,c) | a <- [1..], b <- [1..a], let c = floor (sqrt (fromIntegral (a*a + b*b))), a*a + b*b == c*c]
-- Ejercicio 20 --
listasQueSuman :: Int -> [[Int]]
listasQueSuman n = if n == 0 then [[]] else [x:xs | x <- [1..n], xs <- listasQueSuman(n-x)]
-- La cada elemento de la lista es de la forma (x:xs) donde x toma un numero de 1 hasta n y la cola le realizamos recursion pasando como argumento n - x --
-- No es recursion estructural ya que no estamos recorriendo la estructura de una lista existente
-- sino que estamos construyendo todas las posibles estructuras que cumplen con una condicion
-- no hay un dato cuya estructura dicte la recursion; usamos un numero (n) y lo vamos reduciendo

-- Ejercicio 21 --
listasFinitas :: [[Int]]
listasFinitas = [xs | n <- [1..], xs <- listasQueSuman n]