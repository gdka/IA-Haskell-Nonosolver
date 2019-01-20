-- rows(columns) are numbered from 0 to r-1 (c-1)

import Data.Set (Set, fromList, findIndex, size)
import Data.Maybe
import Data.List hiding (findIndex)
import System.IO
import Data.List.Split
import System.Process
import System.Environment
import System.Exit
import Text.Printf

data Atom = R Int Int Int | C Int Int Int | P Int Int deriving (Show, Ord, Eq)

data Literal = Pos Atom | Neg Atom deriving (Eq, Ord, Show)

type Clause = [Literal]

type CNF = [Clause]

data Nonogram = Nonogram Int Int [[Int]] [[Int]]

baseCNF :: Int -> Int -> CNF
baseCNF r c = [[Pos $ R i 0 0] | i<-[0..r-1]] ++
                  [[Pos $ C 0 j 0] | j<-[0..c-1]]

minLen :: [Int] -> Int
minLen l = sum l + length l

valid :: Int -> [Int] -> Int -> Int -> Bool
valid size xs pos k = (minLen h <= pos) &&
                        (minLen t -1 <= (size-pos))
  where h = take k xs
        t = drop k xs

rowImplication c xs i j k
    | not (valid c xs j k) = []
    | (valid c xs (j+1) k) = [[Neg (R i j k), o1, o2] | o1<-opt1, o2<-opt2]
    | otherwise = [[Neg (R i j k), o1] | o1<-opt1]
  where x = xs!!k
        opt1 = [Pos (P i jj) | jj<-[j..(j+x-1)]] ++
                (if (j+x < c) then [Neg (P i (j+x))] else []) ++
                (if (k /= (length xs)-1) then [Pos (R i (j+x+1) (k+1))] else [Neg (P i jj) | jj <- [j+x+1..(c-1)]])
        opt2 = [Neg (P i j), Pos (R i (j+1) k)]

rowCNF :: Int -> (Int,[Int]) -> CNF
rowCNF c (i,[0]) = rowCNF c (i,[])
rowCNF c (i,[]) = [[Neg (P i j) | j <- [0..(c-1)]]]
rowCNF c (i,xs) = concat [ rowImplication c xs i j k |
                            j<-[0..(c-1)], k<-[0..((length xs)-1)]]

rowsCNF :: Int -> Int -> [[Int]] -> CNF
rowsCNF r c rows = concatMap (rowCNF c) (zip [0..(r-1)] rows)




colImplication r xs i j k
    | not (valid r xs i k) = []
    | (valid r xs (i+1) k) = [[Neg (C i j k), o1, o2] | o1<-opt1, o2<-opt2]
    | otherwise = [[Neg (C i j k), o1] | o1<-opt1]
  where x = xs!!k
        opt1 = [Pos (P ii j) | ii<-[i..(i+x-1)]] ++
                (if (i+x < r) then [Neg (P (i+x) j)] else []) ++
                (if (k /= (length xs)-1) then [Pos (C (i+x+1) j (k+1))] else [Neg (P ii j) | ii <- [i+x+1..(r-1)]])
        opt2 = [Neg (P i j), Pos (C (i+1) j k)]

colCNF :: Int -> (Int,[Int]) -> CNF
colCNF r (j,[0]) = colCNF r (j,[])
colCNF r (j,[]) = [[Neg (P i j) | i <- [0..(r-1)]]]
colCNF r (j,xs) = concat [ colImplication r xs i j k |
                            i<-[0..(r-1)], k<-[0..((length xs)-1)]]

colsCNF :: Int -> Int -> [[Int]] -> CNF
colsCNF r c cols = concatMap (colCNF r) (zip [0..(r-1)] cols)


nonBaseCNF :: Nonogram -> CNF
nonBaseCNF (Nonogram r c rows cols) =
  (rowsCNF r c rows) ++ (colsCNF r c cols)

createCNF :: Nonogram -> CNF
createCNF n@(Nonogram r c _ _) = baseCNF r c ++ nonBaseCNF n

mapLiteral :: Set Atom -> Literal -> String
mapLiteral s (Pos x) = show ((findIndex x s +1)) ++ " "
mapLiteral s (Neg x) = show (-(findIndex x s +1)) ++ " "

mapClause :: Set Atom -> Clause -> String
mapClause s c = (concat $ map (mapLiteral s) c) ++ "0"

mapCNF :: Set Atom -> CNF -> [String]
mapCNF s cnf = map (mapClause s) cnf

getAtom :: Literal -> Atom
getAtom (Pos x) = x
getAtom (Neg x) = x


createMinisatClauses :: (Set Atom) -> CNF -> [String]
createMinisatClauses s cnf = mapCNF s cnf





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


usage   = putStrLn "Usage: nonosolver [-Vh | file.non]"
version = putStrLn "IA I (2018) MAGIA nonosolver 0.1"
defaultPbmFile = "nonogram.pbm"
defaultCnfFile = "nonogram.cnf"
defaultResFile = "nonogram.sol"

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

printCnf :: [String] -> String
printCnf clauses = concatMap (\s -> s++"\n") clauses

printCNFFile n m clauses = printfCNFHeader n m ++ printCnf clauses

-- Format .pbm handling
printPBMHeader :: Int -> Int -> String
printPBMHeader h w = printf "P1\n%d %d\n" w h

printfPBMFile h w matrix = printPBMHeader h w ++ matrix


-- Main
main = do
    getEnv "PATH" >>= \path -> setEnv "PATH" (path ++ ":.") -- This allows for minisat to be installed locally in the same folder
    (nonograms, cnfFile, resFile, pbmFile, multiplier) <- getArgs >>= parseArgs
    let (h, w, Just rows, Just cols) = readNonogram 0 0 Nothing Nothing (words nonograms)

{-        clauses = allClauses h w rows cols
        lenClause = length $ filter (/= []) clauses
        (hash, count) = addToMap (Map.empty) (concat clauses) 1
        reverseHash = addToMap2 (Map.empty) hash (concat clauses)
        -}
        nonogram = Nonogram h w rows cols
        cnf = createCNF nonogram
        myMap = fromList (map getAtom (concat cnf)) :: Set Atom
        clauses = createMinisatClauses myMap cnf
    writeFile cnfFile $ printCNFFile (size myMap) (length cnf) clauses
    system $ "minisat " ++ cnfFile ++ " " ++ resFile
    sat <- readFile resFile
    let listVar = map read (drop 1 $ words sat) :: [Int]
        findS n l = case (n == l!!(n-1), -n == l!!(n-1)) of
                    (True,_) -> intercalate " " $ replicate multiplier "1"
                    (_,True) -> intercalate " " $ replicate multiplier "0"
        getBit i j = let num = 1 + findIndex (P i j) myMap
                      in findS num listVar
        getRow i = intercalate "\n" $ replicate multiplier $ foldr (\j acc-> getBit i j ++ " " ++ acc) [] [0..(w - 1)]
        matrix = foldr (\i acc -> getRow i ++ "\n" ++ acc) [] [0..(h - 1)]

    writeFile pbmFile $ printfPBMFile (multiplier * h) (multiplier * w) matrix
    return ()

