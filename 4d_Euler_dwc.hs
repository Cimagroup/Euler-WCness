import Data.List
import Data.Matrix as M

noInt x = not (x == fromInteger (round x))

-- List of all the hypercubes
--hypercubes_list :: [[Int]]
hypercubes_list = [[a,b,c,d] | a <- [0,1], b<-[0,1], c <- [0,1], d <- [0,1]]


cubes_hypercube [a,b,c,d] = [[a+a1,b,c,d] | a1 <-[0.5,-0.5]]
                          ++ [[a,b+b1,c,d] | b1 <-[0.5,-0.5]]
                          ++ [[a,b,c+c1,d] | c1 <-[0.5,-0.5]]
                          ++ [[a,b,c,d+d1] | d1 <-[0.5,-0.5]]

faces_cube [a,b,c,d] | noInt a = [[a,b+b1,c,d] | b1 <- [0.5,-0.5]] 
                             ++ [[a,b,c+c1,d] | c1 <- [0.5,-0.5]]
                             ++ [[a,b,c,d+d1] | d1 <- [0.5,-0.5]]
                     | noInt b = [[a+a1,b,c,d] | a1 <- [0.5,-0.5]] 
                             ++ [[a,b,c+c1,d] | c1 <- [0.5,-0.5]]
                             ++ [[a,b,c,d+d1] | d1 <- [0.5,-0.5]]
                     | noInt c = [[a,b+b1,c,d] | b1 <- [0.5,-0.5]] 
                             ++ [[a+a1,b,c,d] | a1 <- [0.5,-0.5]]
                             ++ [[a,b,c,d+d1] | d1 <- [0.5,-0.5]]
                     | noInt d = [[a,b+b1,c,d] | b1 <- [0.5,-0.5]] 
                             ++ [[a,b,c+c1,d] | c1 <- [0.5,-0.5]]
                             ++ [[a+a1,b,c,d] | a1 <- [0.5,-0.5]]

list_faces_from_cube xss = concat [faces_cube xs | xs <- xss]


edges_faces [a,b,c,d] | noInt a && noInt b = [[a,b,c+c1,d] | c1<- [0.5,-0.5]]
                                     ++[[a,b,c,d+d1] | d1<- [0.5,-0.5]]
                      | noInt a && noInt c = [[a,b+b1,c,d] | b1<- [0.5,-0.5]]
                                     ++[[a,b,c,d+d1] | d1<- [0.5,-0.5]]
                      | noInt a && noInt d = [[a,b+b1,c,d] | b1<- [0.5,-0.5]]
                                     ++[[a,b,c+c1,d] | c1<- [0.5,-0.5]]
                      | noInt b && noInt c = [[a+a1,b,c,d] | a1<- [0.5,-0.5]]
                                     ++[[a,b,c,d+d1] | d1<- [0.5,-0.5]]
                      | noInt b && noInt d = [[a+a1,b,c,d] | a1<- [0.5,-0.5]]
                                     ++[[a,b,c+c1,d] | c1<- [0.5,-0.5]]
                      | noInt c && noInt d = [[a+a1,b,c,d] | a1<- [0.5,-0.5]]
                                     ++[[a,b+b1,c,d] | b1<- [0.5,-0.5]]

list_edges_from_faces xss = nub (concat [edges_faces xs| xs <- xss])

vertices_edges [a,b,c,d] | not (noInt a) = [[a+a1,b,c,d]|a1<-[0.5,-0.5]]
                         | not (noInt b) = [[a,b+b1,c,d]|b1<-[0.5,-0.5]]
                         | not (noInt c) = [[a,b,c+c1,d]|c1<-[0.5,-0.5]]
                         | not (noInt d) = [[a,b,c,d+d1]|d1<-[0.5,-0.5]]

list_vertices_from_edges xss = nub (concat [vertices_edges xs | xs <- xss])



not_adjacent xss = [xs | xs <- xss, (elem 0.5 xs == False)]


cubes_link xss =  nub (concat [not_adjacent (cubes_hypercube xs)| xs <- xss])


remove xs xss = [ys| ys <- xss, xs/= ys]



num_CubesL = length . cubes_link
num_facesL = length . nub. list_faces_from_cube . cubes_link
num_edgesL = length . list_edges_from_faces . nub. list_faces_from_cube . cubes_link
num_verticesL = length . list_vertices_from_edges .list_edges_from_faces . nub. list_faces_from_cube . cubes_link


euler_link :: [[Double]] -> Int
euler_link hs = num_verticesL hs -num_edgesL hs+num_facesL hs-num_CubesL hs

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% --
        
adjacent x xs = [y | y <- xs, hammingDistance x y == 1]

removeItems xs ys = [x | x <- xs, not (elem x ys)]


dualHcubes xs = removeItems hypercubes_list xs


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


combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs,
                             ys <- combinations (n-1) xs']


hyperCubesSubsets = concat [[x | x <- combinations n hypercubes_list, head x == [0,0,0,0]] | n <- [1..8]]
subsets n =  [x | x <- combinations n hypercubes_list, head x ==[0,0,0,0]]

-- Set of functions for the table in the paper:

pr1 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = dwc hs && (euler_link hs /=1) && not(dwc dhs) && (euler_link dhs /=1) 
                             where dhs = dualHcubes hs

pr2 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = not( dwc hs) && (euler_link hs ==1) && not(dwc dhs) && (euler_link dhs /=1) 
                             where dhs = dualHcubes hs

pr3 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = not( dwc hs) && (euler_link hs /=1) && (dwc dhs) && (euler_link dhs /=1) 
                             where dhs = dualHcubes hs

pr4 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = not( dwc hs) && (euler_link hs /=1) && not(dwc dhs) && (euler_link dhs ==1) 
                             where dhs = dualHcubes hs

pr5 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = ( dwc hs) && (euler_link hs ==1) && not(dwc dhs) && (euler_link dhs /=1) 
                             where dhs = dualHcubes hs

pr6 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = ( dwc hs) && (euler_link hs /=1) && (dwc dhs) && (euler_link dhs /=1) 
                             where dhs = dualHcubes hs

pr7 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = ( dwc hs) && (euler_link hs /=1) && not(dwc dhs) && (euler_link dhs ==1) 
                             where dhs = dualHcubes hs

pr8 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = not( dwc hs) && (euler_link hs ==1) && (dwc dhs) && (euler_link dhs /=1) 
                             where dhs = dualHcubes hs

pr9 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = not( dwc hs) && (euler_link hs ==1) && not(dwc dhs) && (euler_link dhs ==1) 
                             where dhs = dualHcubes hs

pr10 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = not( dwc hs) && (euler_link hs /=1) && (dwc dhs) && (euler_link dhs ==1) 
                             where dhs = dualHcubes hs

pr11 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = ( dwc hs) && (euler_link hs ==1) && (dwc dhs) && (euler_link dhs /=1) 
                             where dhs = dualHcubes hs

pr12 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = ( dwc hs) && (euler_link hs ==1) && not(dwc dhs) && (euler_link dhs ==1) 
                             where dhs = dualHcubes hs

pr13 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = ( dwc hs) && (euler_link hs /=1) && (dwc dhs) && (euler_link dhs ==1) 
                             where dhs = dualHcubes hs

pr14 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = not( dwc hs) && (euler_link hs ==1) && (dwc dhs) && (euler_link dhs ==1) 
                             where dhs = dualHcubes hs

pr15 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = not( dwc hs) && (euler_link hs /=1) && not(dwc dhs) && (euler_link dhs /=1) 
                             where dhs = dualHcubes hs

pr16 n =  [x | x <- subsets n, cond x]
                  where
                        cond hs = ( dwc hs) && (euler_link hs ==1) && (dwc dhs) && (euler_link dhs ==1) 
                             where dhs = dualHcubes hs