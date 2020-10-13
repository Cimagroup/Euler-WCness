# Euler-WCness

In this repository the exhaustive study of the 4D cases is provided.

- The file [4d_Euler_dwc.hs](https://github.com/Cimagroup/Euler-WCness/blob/master/4d_Euler_dwc.hs) is the code used to do the exhaustive study. It is written in Haskell.
- The file [dwc_notsChiwc.txt](https://github.com/Cimagroup/Euler-WCness/blob/master/dwc_notsChiwc.txt) is an exhaustive list of configurations of hypercubes in 4D incident to a vertex that are digitally well-composed but  not self-dual Euler. (Size of the set of hypercubes 6,7, and 8. The rest of the examples are their duals). Proof of theorem 20.
- In the file [exhaustive_list.txt](https://github.com/Cimagroup/Euler-WCness/blob/master/exhaustive_list.txt), a list with `(Int, [[Int]],Bool, Bool)` where the first element is the size of the set of hypercubes, the second is one example, the third element corresponds to the self-dual Euler well-composedness, and the last one to the digitally well-composedness.
- The file [nd_euler_dwc.hs](https://github.com/Cimagroup/Euler-WCness/blob/master/nd_euler_dwc.hs) provides a ND implementation.
- The pdf [main.pdf](https://github.com/Cimagroup/Euler-WCness/blob/master/tex/main.pdf) is a description of the experiments and code.

<img src="https://github.com/Cimagroup/Euler-WCness/blob/master/3d.png" width="300" /><img src="https://github.com/Cimagroup/Euler-WCness/blob/master/4d.png" width="400" />



Boutry N., Gonzalez-Diaz R., Jimenez MJ., Paluzo-Hildago E. (2020) Euler Well-Composedness. In: Lukić T., Barneva R., Brimkov V., Čomić L., Sladoje N. (eds) Combinatorial Image Analysis. IWCIA 2020. Lecture Notes in Computer Science, vol 12148. Springer, Cham. https://doi.org/10.1007/978-3-030-51002-2_1
