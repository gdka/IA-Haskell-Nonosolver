import qualified Data.Map as Map
import Data.Maybe
import Data.List
import System.IO
import Data.List.Split
import System.Process
import System.Environment
import System.Exit
import Text.Printf


--Predicates F(i,j,k), C(i,j,k) and P(i,j) defined in class
data Atom = F (Int, Int, Int) | C (Int, Int, Int) | P (Int, Int) deriving (Show, Ord, Eq)
data Literal = Id Atom | Not Atom deriving (Eq, Ord, Show)


--List with all possible pairs of elements between l1 and l2 where x belongs to l1 and y belongs to l2
cross l1 l2 = [(x,y) | x <- l1, y <- l2]

--Return the amount of blocks in the i row
getTamF i blocksF = length (blocksF !! i) - 1

--Return the amount of blocks in the j column
getTamC j blocksC = length (blocksC !! j) - 1

--Given a row i and a block k (of the row i) returns all posible start for one block (F(i,j,k)) for the given (i,k)
--and verifies that no two F(i,j1,k) F(i,j2,k) are True for every j1!=j2.  
positionCellF (i,k) c blocksF = oneInitial : oneSetByOne
        where notBoth (j1,j2) = [ Not $ F (i,j1,k), Not $ F (i,j2,k)]
              oneSetByOne = map notBoth [(j1,j2) | (j1,j2) <- cross [0..c] [0..c], j1>j2]
              oneInitial = [Id $ F (i,j,k) | j <- [0..c], c-j+1>=(blocksF !! i) !! k]

--Calls positionCellF for every (i,k) with i belongs to [0..f] and k belongs to [0..amount of blocks of the row i]
alfa f c blocksF = foldr (\x acc-> (positionCellF x c blocksF)++acc) [] [(i,k) | i<-[0..f], k<-[0..getTamF i blocksF]]

--Given a row i, a column j and a block k, return the conditions that have to meet if F(i,j,k) is True. 
--i.e F(i,j) -> P(i,j)..P(i,j+l) & F(i,j)-> not P(i,j-1) & F(i,j) -> not P(i,j+l+1) 
blockOn (i,j,k) c l blocksF = border1:border2:(map setP [j..(j+l-1)])
            where fit = c-j+1>=(blocksF !! i) !! k
                  border1 = if (j-1>=0 && fit) then [Not $ F (i,j,k), Not $ P (i,j-1)] else []
                  border2 = if (j+l<=c && fit) then [ Not $ F (i,j,k), Not $ P (i,j+l)] else []
                  setP p = if (fit) then [Not $ F (i,j,k), Id $ P (i,p)] else []

--Calls blockOn for every (i,j,k) with i belongs to [0..f], j belongs to [0..c]
--and k belongs to [0..amount of blocks of the row i]
beta f c blocksF = foldr (\(i,j,k,l) acc -> (blockOn (i,j,k) c l blocksF)++acc) [] list
        where list = [(i,j,k,l) | i<-[0..f], j<-[0..c], k<-[0..getTamF i blocksF], l<-[blocksF !! i !! k]]

--Given a row i, a column j and a block k, verifies the order of the blocks.  
--i.e F(i,j,k) -> the next block starts in one position between [j+l+1,c].
orderOfTheBlocks (i,j,k) c l blocksF = (Not $ F (i,j,k)):(map (\nj -> Id $ F (i,nj,k+1)) (filter (<=c) [j+l+1..c]))

--Calls orderOfTheBlocks for every (i,j,k) with i belongs to [0..f], j belongs to [0..c]
--and k belongs to [0..amount of blocks of the row i].
gamma f c  blocksF = foldr (\(i,j,k,l) acc -> (orderOfTheBlocks (i,j,k) c l blocksF):acc) [] list
        where list = [(i,j,k,l) | i<-[0..f], j<-[0..c], k<-[0..((getTamF i blocksF)-1)], l<-[blocksF !! i !! k]]

--Given a row i, a column j, verifies if the P(i,j) is True then there is a block that contains this cell.
cellOn (i,j) c blocksF = let list = [(nj,k) | nj<-[0..c], k<-[0..getTamF i blocksF], j>=nj, j<=nj-1+blocksF !! i !! k]
                 in (Not $ P (i,j)):(map (\(nj,k) -> Id $ F (i,nj,k)) list)

--Calls cellOn for every (i,j) with i belongs to [0..f], j belongs to [0..c].
delta f c blocksF = foldr (\p acc -> (cellOn p c blocksF):acc) [] (cross [0..f] [0..c])

--The following 8 functions are the same conditions but for the columns :).

positionCellC (j,k) f blocksC = oneInitial : oneSetByOne
        where notBoth (i1,i2) = [ Not $ C (i1,j,k), Not $ C (i2,j,k)]
              oneSetByOne = map notBoth [(i1,i2) | (i1,i2) <- cross [0..f] [0..f], i1>i2]
              oneInitial = [Id $ C (i,j,k) | i<-[0..f], f-i+1>=(blocksC !! j) !! k]

alfaC f c blocksC = foldr (\x acc-> (positionCellC x f blocksC)++acc) [] [(j,k) | j<-[0..c], k<-[0..getTamC j blocksC]]

blockOnC (i,j,k) f l blocksC = border1:border2:(map setP [i..(i+l-1)])
            where fit = f-i+1>=(blocksC !! j) !! k
                  border1 = if (i-1>=0 && fit) then [Not $ C (i,j,k), Not $ P (i-1,j)] else []
                  border2 = if (i+l<=f && fit) then [Not $ C (i,j,k), Not $ P (i+l,j)] else []
                  setP p = if (fit) then [Not $ C (i,j,k), Id $ P (p,j)] else []

betaC f c blocksC = foldr (\(i,j,k,l) acc -> (blockOnC (i,j,k) f l blocksC)++acc) [] list
        where list = [(i,j,k,l) | i<-[0..f], j<-[0..c], k<-[0..getTamC j blocksC], l<-[blocksC !! j !! k]]


orderOfTheBlocksC (i,j,k) f l blocksC = (Not $ C (i,j,k)):(map (\ni -> Id $ C (ni,j,k+1)) (filter (<=f) [i+l+1..f]))

gammaC f c blocksC = foldr (\(i,j,k,l) acc -> (orderOfTheBlocksC (i,j,k) f l blocksC):acc) [] list
        where list = [(i,j,k,l) | i<-[0..f], j<-[0..c], k<-[0..((getTamC j blocksC)-1)], l<-[blocksC !! j !! k]]

cellOnC (i,j) f blocksC = let list = [(ni,k) | ni<-[0..f], k<-[0..getTamC j blocksC], i>=ni, i<=ni-1+blocksC !! j !! k]
                 in (Not $ P (i,j)):(map (\(ni,k) -> Id $ C (ni,j,k)) list)

deltaC f c blocksC = foldr (\p acc -> (cellOnC p f blocksC):acc) [] (cross [0..f] [0..c])

--Construct the Minisat input file.
printCnf l hash = foldr (\x acc -> if (x == []) then acc else (getcnf x) ++ "0\n" ++ acc) [] l
    where getcnf :: [Literal] -> String
          getcnf [] = []
          getcnf (l:ls) = let num = Map.lookup (extract l) hash
                              value = (bool l)*(fromJust num)
                          in (show value)++" "++(getcnf ls)

--Map from number to Atom.
addToMap2 m hash [] = m
addToMap2 m hash (l:ls) = let num = fromJust $ Map.lookup (extract l) hash
                                in if (Map.member num m)
                                   then (addToMap2 m hash ls)
                                   else (addToMap2 (Map.insert num (extract l) m) hash ls)
--Map from Atom to number.
addToMap m [] count = (m,count)
addToMap m (l:ls) count = let atom = extract l
                          in if Map.member atom m
                             then (addToMap m ls count)
                             else addToMap (Map.insert atom count m) ls (count+1)

extract l = case l of
        Id x -> x
        Not x -> x

bool l = case l of
     Id x -> 1
     Not x -> -1


-- Concatenation of all clauses
allClauses h w rows cols = (alfa (h-1) (w-1) rows)
                            ++ (alfaC (h-1) (w-1) cols)
                            ++ (beta (h-1) (w-1) rows)
                            ++ (betaC (h-1) (w-1) cols)
                            ++ (gamma (h-1) (w-1) rows)
                            ++ (gammaC (h-1) (w-1) cols)
                            ++ (delta (h-1) (w-1) rows)
                            ++ (deltaC (h-1) (w-1) cols)


-- Argument handling
parseArgs :: [String] -> IO (String, String, String, String, Int)
parseArgs []          = do
    content <- getContents
    return (content, defaultCnfFile, defaultResFile, defaultPbmFile, 1)
parseArgs ("-non":non:xs) = do
    inputContent <- readFile non
    (_, cnf, res, pbm, x) <- parseArgs xs
    return (inputContent, cnf, res, pbm, x)
parseArgs ("-cnf":cnf:xs) = do
    (inputContent, _, res, pbm, x) <- parseArgs xs
    return (inputContent, cnf, res, pbm, x)
parseArgs ("-res":res:xs) = do
    (inputContent, cnf, _, pbm, x) <- parseArgs xs
    return (inputContent, cnf, res, pbm, x)
parseArgs ("-pbm":pbm:xs) = do
    (inputContent, cnf, res, _, x) <- parseArgs xs
    return (inputContent, cnf, res, pbm, x)
parseArgs ("-x":x:xs) = do
    (inputContent, cnf, res, pbm, _) <- parseArgs xs
    return (inputContent, cnf, res, pbm, read x :: Int)
parseArgs ["-h"]      = usage   >> exitSuccess
parseArgs ["-v"]      = version >> exitSuccess
parseArgs _           = usage >> exitFailure


usage   = putStrLn "Usage: nonosolver [-v] [-h] [-non file.non] [-cnf file.cnf] [-res file.res] [-pbm file.pbm] [-x size]\nNonosolver will read a .non file from stdin if no -non flag is used."
version = putStrLn "IA I (2018) MAGIA nonosolver 0.3"
defaultPbmFile = "nonogram.pbm"
defaultCnfFile = "nonogram.cnf"
defaultResFile = "nonogram.res"

-- Format .non handling
readRule :: String -> [Int]
readRule rule = map read (splitOn "," rule) :: [Int]

readRules :: Int -> [String] -> ([[Int]], [String])
readRules 0 rest    = ([], rest)
readRules n (x:xs)  = ((readRule x):rules, rest)
    where (rules, rest) = readRules (n - 1) xs

readNonogram :: Int -> Int -> Maybe [[Int]] -> Maybe [[Int]] -> [String] -> (Int, Int, Maybe [[Int]], Maybe [[Int]])
readNonogram h w rows cols []                   = (h, w, rows, cols)
readNonogram 0 w rows cols ("height":h:rest)    = readNonogram (read h :: Int) w rows cols rest
readNonogram h 0 rows cols ("width":w:rest)     = readNonogram h (read w :: Int) rows cols rest
readNonogram h w Nothing cols ("rows":nono)     = readNonogram h w (Just rows) cols rest
    where (rows, rest) = readRules h nono
readNonogram h w rows Nothing ("columns":nono)  = readNonogram h w rows (Just cols) rest
    where (cols, rest) = readRules w nono

-- Format .cnf handling
printfCNFHeader :: Int -> Int -> String
printfCNFHeader n m = printf "p cnf %d %d\n" n m

printCNFFile n m clauses hash = printfCNFHeader n m ++ printCnf clauses hash

-- Format .pbm handling
printPBMHeader :: Int -> Int -> String
printPBMHeader h w = printf "P1\n%d %d\n" w h

printfPBMFile h w matrix = printPBMHeader h w ++ matrix


-- Main
main = do
    getEnv "PATH" >>= \path -> setEnv "PATH" (path ++ ":.") -- This allows for minisat to be installed locally in the same folder
    (nonogram, cnfFile, resFile, pbmFile, multiplier) <- getArgs >>= parseArgs
    let (h, w, Just rows, Just cols) = readNonogram 0 0 Nothing Nothing (words nonogram)
        clauses = allClauses h w rows cols
        lenClause = length $ filter (/= []) clauses
        (hash, count) = addToMap (Map.empty) (concat clauses) 1
        reverseHash = addToMap2 (Map.empty) hash (concat clauses)
    writeFile cnfFile $ printCNFFile (count - 1) (lenClause) clauses hash
    system $ "minisat " ++ cnfFile ++ " " ++ resFile
    sat <- readFile resFile
    let listVar = map read (drop 1 $ words sat) :: [Int]
        findS n (l:ls) = case (n == l, -n == l) of
                    (True,_) -> intercalate " " $ replicate multiplier "1"
                    (_,True) -> intercalate " " $ replicate multiplier "0"
                    otherwise -> findS n ls
        getBit x = let num = fromJust $ Map.lookup (P x) hash
                    in findS num listVar
        getRow i = intercalate "\n" $ replicate multiplier $ foldr (\j acc-> getBit (i, j) ++ " " ++ acc) [] [0..(w - 1)]
        matrix = foldr (\i acc -> getRow i ++ "\n" ++ acc) [] [0..(h - 1)]

    writeFile pbmFile $ printfPBMFile (multiplier * h) (multiplier * w) matrix
    return ()

