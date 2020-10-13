module Nd_euler_dwc where
import Data.List


-- Auxiliar function
noInt x = not (x == fromInteger (round x))

-- List of the hypercubes of dimension n
hypercubes_list n = iteration n [[a] | a <- [0,1]]

-- Auxiliar function
iteration 1 xs = xs
iteration n xs = iteration (n-1) [x ++ [a] | x <- xs, a <- [0,1]]


-- Cells of given dimension.
cell 0 hs = hs
cell i hs= nub (cell (i-1) (suma_general hs))


suma h = [take i h++[ t + (h!!i)]++(drop (i+1) h)| i <- [0..(length h)-1],t <- [-0.5,0.5], not (noInt (h!!i)) ]
suma_general = concat. map suma


not_adjacent xss = [xs | xs <- xss, (elem 0.5 xs == False)]

cubes_link xss =  nub (concat [not_adjacent (suma xs)| xs <- xss])

cubes_star xss = nub (concat [adjacents (suma xs)| xs <- xss])

euler_link hs =  sum [(length (cell i cl))*(-1)^(i+1) | i <- [0..(n-1)]]
           where
                cl = cubes_link hs
                n = length (head hs)



adjacents xss = [xs | xs <- xss, not (elem 0.5 xs == False)]
-- 
adjacent x xs = [y | y <- xs, hammingDistance x y == 1]


removeItems xs ys = [x | x <- xs, not (elem x ys)]


dualHcubes n xs = removeItems (hypercubes_list n) xs


-- Determine if there is a path between two elements. 

isthereapath :: (Eq a, Eq t, Num t) => t -> [a] -> [a] -> [[a]] -> Bool
isthereapath n x y xs | x == y = True
                      | n == 0 = False
                      | otherwise = or [isthereapath (n-1) t y xs | t <- (adjacent x xs)]


-- Is dwc?
dwc :: Eq a => [[a]] -> Bool
dwc xss = and [isthereapath (length xss) xs ys xss | ys <- xss, xs <- xss]

hammingDistance :: Eq a => [a] -> [a] -> Int
hammingDistance = (sum .) . zipWith ((fromEnum .) . (/=))




counter_example_init = [[0,0,0,0],[0,0,0,1],[0,0,1,1],[0,1,1,1],[1,1,1,0],[1,1,0,0],[1,0,0,0],[1,1,1,1.0]]
counter_example 0 hs = hs
counter_example n hs = counter_example (n-1) [[x]++ys| x <-[0,1], ys <- hs]

-- n is dim-4
-- let hs = [[0,0,0,0],[0,0,0,1],[0,0,1,1],[0,1,1,1],[1,1,1,0],[1,1,0,0],[1,0,0,0],[1,1,1,1.0]]
-- let ts =[[x]++ys| x <-[0,1], ys <- hs]


test = [ (4+i,dwc (f i),dwc (dualHcubes (4+i) (f i)),euler_link (f i), euler_link (dualHcubes (4+i) (f i))) | i <- [1..2]]
     where
        f i = counter_example i counter_example_init

-- %%%%%%%%%%%%%%%%%%%%
remove xs xss = [ys| ys <- xss, xs/= ys]


intersection xss = nub (concat [intersect x y | x <- xss, y <- xss, x/=y])
-- not_boundary k hs = intersection [cell k [h] | h <- hs]

-- %%%%%%%%%%%%%%%%%%%%%%%%%%
{-
Xi(St_K v)=0 and Xi(St_dualK v)=0 => Xi(St_K e)=0

knowing that:
Xi(St_k e)=Xi(\cap_{v\in e^{(0)}} St_k v)
-}

-- Dimension of a cell
dim e = sum [1 | i <- e, i==0||i==1]


-- Determine if a cell e is a face of a cell c
face e c | dim c >= dim e = elem e (cell k [c])
         | otherwise = False
     where
        k = dim c - dim e

-- Computes the cells of a given dimension of a cell e in a configuration hs.
star k e hs = [c | c <- cell k hs, face e c]


-- Computes the Euler characteristic of the star of a cell e in a configuration hs.
euler_star e hs =  sum [(length (star k e hs)) * (-1)^k | k <- [0..n] ]
           where
                n = length e


-- List of cells of [0..0]
e_list n = concat [cell k [replicate n 0] | k <- [1..n-1]]
v_list n = cell n [replicate n 0]

-- Euler characteristic of the previous list.
euler_e_list hs = [euler_star e hs | e <- e_list n]
             where
                n = length (head hs)

-- List of hypercubes that contains a cell e as a face.
adjacent_hips e = [h | h <- hypercubes_list_general n, face e h]
              where
                n = length e
                
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs,
                             ys <- combinations (n-1) xs']          

-- All possible configurations of hypercubes that contain the cell e as a face. 
all_conf e = [c | c <- concat [combinations k hs | k <- [1..length hs]], elem (replicate n 0) c, f e c]
         where
                hs = adjacent_hips e
                n = length e
                f e c = length c <= 2^(n-dim e)-1

euler_star_all e = [euler_star e hs | hs <- all_conf e, f hs]
               where
                f hs = sum [euler_star v hs + euler_star v (dhs_gen v hs) | v <- cell n hs] == 0
                n = length e

euler_star_all_list e = [hs | hs <- all_conf e, f hs]
               where
                f hs = sum [euler_star v hs + euler_star v (dhs_gen v hs) | v <- cell n hs] == 0
                n = length e


hypercubes_list_general n = iteration n [[a] | a <- [0,1,-1]]
hs_list_gen_subset v = [h | h <- hypercubes_list_general n, face v h]
                   where
                        n = length v
dhs_gen v hs = (hs_list_gen_subset v) \\ hs

{-
confs_cond1 v = [hs | hs <- all_conf v, euler_star v hs == 0, euler_star v (dualHcubes n hs)==0]
            where
                n = length vstar
-}

---

-- ND Euler => DWC

xi_dwc_prop hs = (not (dwc hs) || not (dwc hsdual)) && (euler_link hs == 1 && euler_link hsdual ==1)
     where
         n = length (head hs)
         hsdual = dualHcubes n hs

hyperCubesSubsets n k = concat [combinations k (hypercubes_list n) | k <- [1..25]]
xiSets n k = (n ,length xss, length (hss), head xss)
        where
               hss = [hs | hs <- hyperCubesSubsets n k]
               xss = [x | x <- hss, xi_dwc_prop x]

xiSetsAll n = [xiSets n k | k <- [1..25]]
{-
test2 hs = (not (dwc hs) || not (dwc hsdual)) && (euler_link hs == 1 && euler_link hsdual ==1)
     where
         hsdual = dualHcubes hs

xiSets n = (n ,length xss, length (hss), head xss)
        where
               hss = [hs | hs <- hyperCubesSubsets, length(hs) == n ]
               xss = [x | x <- hss, test x]
               
xiSets2 n = (n ,head [x | x <- hss, test x])
        where
               hss = [hs | hs <- hyperCubesSubsets, length(hs) == n ]
               

xiSetsAll = [xiSets n | n <- [1..25]]
-}