
--import Prelude hiding ((^))
--import qualified Prelude
import qualified Data.Array as A
import Data.Bits hiding (rotate)
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple
import Data.Int
--import Data.Time.Calendar (fromGregorian)
--import Data.Time.Calendar.WeekDate (toWeekDate) 
-- there's an excellent library that provides us with a list of primes: let's use it
import Data.Numbers.Primes (primes)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.List.Ordered as Ordered
import Data.Function (on)
import Debug.Trace (trace)
import qualified Data.IntMap as IntMap
import qualified Data.IntMap.Strict as IntMapS
import qualified Data.Map as Map
import Numeric.Search.Range (searchFromTo)
import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)
import Math.NumberTheory.Powers.Cubes (isCube, integerCubeRoot)
import Math.NumberTheory.Primes.Testing (isPrime)
import Math.NumberTheory.Logarithms (integerLogBase)
import Math.Polynomial
import Math.Polynomial.Interpolation
import Data.Ratio
import Data.Function.Memoize (traceMemoize,memoize,memoize2)
import System.IO.Unsafe

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

rle :: Eq a => [a] -> [(Int, a)]
rle l = [(length grp, head grp) | grp <- group l]

is_pandigital :: Integer -> Integer -> Integer -> Bool
is_pandigital n m d = (==) [n..m] $ sort $ digits d

is_pandigital19 :: Integer -> Bool
is_pandigital19 d = ((d `mod` 9) == 0) && is_pandigital 1 9 d
--is_pandigital19 = is_pandigital 1 9

isPandigital19 = is_pandigital19

tenToTheSixteenth :: Int
tenToTheSixteenth = (10^16)

squares :: [Integer]
squares = [x^2 | x <- [1..]]

--------------------------------------------------------------------------------
-- Problem 1
--------------------------------------------------------------------------------

problem1 :: Integer
problem1 = sum [x | x <- [1..999], ((x `mod` 3) == 0) || ((x `mod` 5) == 0)]

--------------------------------------------------------------------------------
-- Problem 2
--------------------------------------------------------------------------------

-- from http://www.haskell.org/haskellwiki/The_Fibonacci_sequence §2.2.5 With iterate
fibs :: [Integer]
fibs = map fst $ (\(a,b) -> (b,a+b)) `iterate` (0,1)

fibsMod :: Int -> [Int]
fibsMod m = map fst $ (\(a,b) -> (b,((a+b) `mod` m))) `iterate` (0,1)

explicitFib :: Integer -> Integer
explicitFib 0 = 0
explicitFib 1 = 1
explicitFib 2 = 1
explicitFib n
    | (n `mod` 2) == 0 =
        let m = n `div` 2
            fm = explicitFib m
            fm1 = explicitFib (m - 1) in
        fm * (2*fm1 + fm)
    | (n `mod` 2) == 1 =
        let m = n `div` 2
            fm = explicitFib m
            fm1 = explicitFib (m + 1) in
        (fm ^ 2) + (fm1 ^ 2)

problem2 :: Integer
problem2 = sum [x | x <- takeWhile ((>=) 4000000) fibs, (x `mod` 2) == 0]

--------------------------------------------------------------------------------
-- Problem 3
--------------------------------------------------------------------------------

factors' :: Integer -> [Integer]
factors' n = 
    case filter (\p -> (n `mod` p) == 0) $ takeWhile ((>=) $ integerSquareRoot n) primes of
        [] -> []
        p:_ -> p:factors (n `div` p)

factors n =
    let f = factors' n in
    if (==) f [] then [n] else f

largestPrimeFactor n =
    last $ factors n

decomp :: Integer -> [(Integer,Int)]
decomp 0 = []
decomp 1 = []
decomp n = map (\g -> (head g, length g)) $ group $ factors n

decompInt :: Integer -> [(Int,Int)]
decompInt = map (\(a,b) -> (fromInteger a, b)) . decomp

nDivisors :: Integer -> Int
nDivisors =
    product . map ((+1) . snd) . decomp

rad :: Integer -> Integer
rad = product . map fst . decomp

divisors :: Integer -> [Integer]
divisors n = Ordered.nubSort $ map product $ subsequences $ factors n

divisorsFromDecomp :: [(Integer,Int)] -> [Integer]
divisorsFromDecomp [] = []
divisorsFromDecomp [(a,b)] = [a^c | c <- [0..b]]
divisorsFromDecomp ((a,b):xs) = [d*(a^c) | c <- [0..b], d <- divisorsFromDecomp xs]

divisors' = divisorsFromDecomp . decomp

sumDivisors :: Integer -> Integer
sumDivisors n = (sum $ divisors n)

sumProperDivisors :: Integer -> Integer
sumProperDivisors n = (-) (sum $ divisors n) n

--sumSquaredDivisors :: Integer -> Integer
--sumSquaredDivisors = sum . map (^2) . divisors

sigmaK :: Int -> Integer -> Integer
sigmaK k = product . map (\(p,a) -> sum [p^(j*k) | j <- [0..a]]) . decomp
--sigmaK k = product . map (\(p,a) -> ((p^((a+1)*k) - 1) `div` (p^k - 1))) . decomp
--sigmaK k = product . map (\(p,a) -> sum $ scanl (\c d -> c*p^k) 1 [1..a]) . decomp

isSigmaKSquare :: Int -> Integer -> Bool
isSigmaKSquare k = isSquareBag . foldl' orderedMergeBag [] . map (decomp . \(p,a) -> sum [p^(j*k) | j <- [0..a]]) . decomp

sumSquaredDivisors :: Integer -> Integer
sumSquaredDivisors = sigmaK 2

isAmicable :: Integer -> Bool
isAmicable n =
    let d = sumProperDivisors n in
    ((==) n $ sumProperDivisors d) && (n /= d)

problem3 = largestPrimeFactor 600851475143

orderedMergeBag [] l = l
orderedMergeBag l [] = l
orderedMergeBag ((a,n):as) ((b,m):bs) =
    case compare a b of 
        EQ -> (a,n+m):orderedMergeBag as bs
        LT -> (a,n):orderedMergeBag as ((b,m):bs)
        GT -> (b,m):orderedMergeBag ((a,n):as) bs

isSquareBag [] = True
isSquareBag ((a,n):l) = (n `rem` 2 == 0) && (isSquareBag l)


--------------------------------------------------------------------------------
-- Problem 4
--------------------------------------------------------------------------------

-- DONE, TO BE WRITTEN

--------------------------------------------------------------------------------
-- Problem 5
--------------------------------------------------------------------------------

problem5 = foldl1 lcm [1..20]

--------------------------------------------------------------------------------
-- Problem 6
--------------------------------------------------------------------------------

-- Pen & paper

--------------------------------------------------------------------------------
-- Problem 7
--------------------------------------------------------------------------------

problem7 = primes !! 10000

--------------------------------------------------------------------------------
-- Problem 8
--------------------------------------------------------------------------------

p8_n = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

digitsBase' b n
    | b < 2 = error "b must be >= 2"
    | n < 0 = error "Non-negative integers only"
    | n <= (b-1) = [n]
    | otherwise = (rem n b) : (digitsBase' b $ div n b)

digits' = digitsBase' 10

binaryDigits' = digitsBase' 2

digits = reverse . digits'

binaryDigits = reverse . binaryDigits'

problem8 :: Integer
problem8 = last $ sort [a*b*c*d*e | (a,b,c,d,e) <- zip5 (digits p8_n) (drop 1 $ digits p8_n) (drop 2 $ digits p8_n) (drop 3 $ digits p8_n) (drop 4 $ digits p8_n)]

--------------------------------------------------------------------------------
-- Problem 9
--------------------------------------------------------------------------------

problem9 :: Integer
problem9 = head [a*b*(1000-a-b) | a<-[1..1000], b<-[a..1000], (a^2)+(b^2)==(1000-a-b)^2]

--------------------------------------------------------------------------------
-- Problem 10
--------------------------------------------------------------------------------

problem10 = sum $ takeWhile ((>=) 2000000) primes

--------------------------------------------------------------------------------
-- Problem 11
--------------------------------------------------------------------------------

grid' = "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 \
\49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 \
\81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 \
\52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 \
\22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 \
\24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 \
\32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 \
\67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 \
\24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 \
\21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 \
\78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 \
\16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 \
\86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 \
\19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 \
\04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 \
\88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 \
\04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 \
\20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 \
\20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 \
\01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"

grid = map (read::(String -> Int)) $ words grid'

mgrid = Map.fromList $ zip (map (flip quotRem 20) [0..]) grid

p11 = maximum $ map (product . map fromJust) $ filter (\l -> foldl' (&&) True $ map isJust l) $ [[a,b,c,d] | i <- [0..20], j <- [0..20], k <- [True, False], let f = flip Map.lookup mgrid . (if k then swap else id), let a = f (j,i), let b = f (j, i+1), let c = f(j,i+2), let d = f(j,i+3)] ++ [[a,b,c,d] | i <- [0..20], j <- [0..20], l <- [-1, 1], let f = flip Map.lookup mgrid, let a = f (j,i), let b = f (j+1*l, i+1), let c = f(j+2*l,i+2), let d = f(j+3*l,i+3)]

--------------------------------------------------------------------------------
-- Problem 12
--------------------------------------------------------------------------------

tri :: Integer -> Integer
tri n = (n * (n+1)) `quot` 2

p12 = head $ filter (((<=) 500) . nDivisors) [tri n | n <- [1..]]

--------------------------------------------------------------------------------
-- Problem 13
--------------------------------------------------------------------------------

problem13 = take 10 $ show $ 37107287533902102798797998220837590246510135740250+46376937677490009712648124896970078050417018260538+74324986199524741059474233309513058123726617309629+91942213363574161572522430563301811072406154908250+23067588207539346171171980310421047513778063246676+89261670696623633820136378418383684178734361726757+28112879812849979408065481931592621691275889832738+44274228917432520321923589422876796487670272189318+47451445736001306439091167216856844588711603153276+70386486105843025439939619828917593665686757934951+62176457141856560629502157223196586755079324193331+64906352462741904929101432445813822663347944758178+92575867718337217661963751590579239728245598838407+58203565325359399008402633568948830189458628227828+80181199384826282014278194139940567587151170094390+35398664372827112653829987240784473053190104293586+86515506006295864861532075273371959191420517255829+71693888707715466499115593487603532921714970056938+54370070576826684624621495650076471787294438377604+53282654108756828443191190634694037855217779295145+36123272525000296071075082563815656710885258350721+45876576172410976447339110607218265236877223636045+17423706905851860660448207621209813287860733969412+81142660418086830619328460811191061556940512689692+51934325451728388641918047049293215058642563049483+62467221648435076201727918039944693004732956340691+15732444386908125794514089057706229429197107928209+55037687525678773091862540744969844508330393682126+18336384825330154686196124348767681297534375946515+80386287592878490201521685554828717201219257766954+78182833757993103614740356856449095527097864797581+16726320100436897842553539920931837441497806860984+48403098129077791799088218795327364475675590848030+87086987551392711854517078544161852424320693150332+59959406895756536782107074926966537676326235447210+69793950679652694742597709739166693763042633987085+41052684708299085211399427365734116182760315001271+65378607361501080857009149939512557028198746004375+35829035317434717326932123578154982629742552737307+94953759765105305946966067683156574377167401875275+88902802571733229619176668713819931811048770190271+25267680276078003013678680992525463401061632866526+36270218540497705585629946580636237993140746255962+24074486908231174977792365466257246923322810917141+91430288197103288597806669760892938638285025333403+34413065578016127815921815005561868836468420090470+23053081172816430487623791969842487255036638784583+11487696932154902810424020138335124462181441773470+63783299490636259666498587618221225225512486764533+67720186971698544312419572409913959008952310058822+95548255300263520781532296796249481641953868218774+76085327132285723110424803456124867697064507995236+37774242535411291684276865538926205024910326572967+23701913275725675285653248258265463092207058596522+29798860272258331913126375147341994889534765745501+18495701454879288984856827726077713721403798879715+38298203783031473527721580348144513491373226651381+34829543829199918180278916522431027392251122869539+40957953066405232632538044100059654939159879593635+29746152185502371307642255121183693803580388584903+41698116222072977186158236678424689157993532961922+62467957194401269043877107275048102390895523597457+23189706772547915061505504953922979530901129967519+86188088225875314529584099251203829009407770775672+11306739708304724483816533873502340845647058077308+82959174767140363198008187129011875491310547126581+97623331044818386269515456334926366572897563400500+42846280183517070527831839425882145521227251250327+55121603546981200581762165212827652751691296897789+32238195734329339946437501907836945765883352399886+75506164965184775180738168837861091527357929701337+62177842752192623401942399639168044983993173312731+32924185707147349566916674687634660915035914677504+99518671430235219628894890102423325116913619626622+73267460800591547471830798392868535206946944540724+76841822524674417161514036427982273348055556214818+97142617910342598647204516893989422179826088076852+87783646182799346313767754307809363333018982642090+10848802521674670883215120185883543223812876952786+71329612474782464538636993009049310363619763878039+62184073572399794223406235393808339651327408011116+66627891981488087797941876876144230030984490851411+60661826293682836764744779239180335110989069790714+85786944089552990653640447425576083659976645795096+66024396409905389607120198219976047599490197230297+64913982680032973156037120041377903785566085089252+16730939319872750275468906903707539413042652315011+94809377245048795150954100921645863754710598436791+78639167021187492431995700641917969777599028300699+15368713711936614952811305876380278410754449733078+40789923115535562561142322423255033685442488917353+44889911501440648020369068063960672322193204149535+41503128880339536053299340368006977710650566631954+81234880673210146739058568557934581403627822703280+82616570773948327592232845941706525094512325230608+22918802058777319719839450180888072429661980811197+77158542502016545090413245809786882778948721859617+72107838435069186155435662884062257473692284509516+20849603980134001723930671666823555245252804609722+53503534226472524250874054075591789781264330331690

--------------------------------------------------------------------------------
-- Problem 14
--------------------------------------------------------------------------------

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n =
    n : (if n `rem` 2 == 0 then collatz (n `div` 2) else collatz (3*n+1))

collatzLength n = collatzLength' n 0
    where
    collatzLength' 1 aux = 1 + aux
    collatzLength' n aux = (if n `rem` 2 == 0 then collatzLength' (n `div` 2) else collatzLength' (3*n+1)) (aux + 1)

memoizedCollatzLength' :: Int -> IntMap.IntMap Int -> (Int, IntMap.IntMap Int)
memoizedCollatzLength' 1 _ = (1, IntMap.singleton 1 1)
memoizedCollatzLength' n tbl =
    if IntMap.member n tbl
    then ((IntMap.!) tbl n, tbl)
    else (
        if n `rem` 2 == 0
        then let (l,tbl0) = memoizedCollatzLength' (n `div` 2) tbl in (l+1, IntMap.insert n (l+1) tbl0)
        --else let (l,tbl0) = memoizedCollatzLength' (3*n+1) tbl in (l+1, IntMap.insert n (l+1) tbl0)
        else let (l,tbl0) = memoizedCollatzLength' ((3*n+1) `div` 2) tbl in (l+2, IntMap.insert n (l+2) tbl0)
    )

memoizedCollatzLength :: Int -> Int
memoizedCollatzLength n = fst $ memoizedCollatzLength' n IntMap.empty

problem14 = snd $ IntMap.foldlWithKey' g (1,1) htbl
    where
        g (maxl,maxn) n l = if (n > 1000000) || (l <= maxl) then (maxl,maxn) else (l,n)
        htbl = foldl' f IntMap.empty [1..1000000]
        f tbl n = snd $ memoizedCollatzLength' n tbl

--------------------------------------------------------------------------------
-- Problem 15
--------------------------------------------------------------------------------

-- 40 moves total, 20 down and 20 right: there's C(40,20) ways to pick the times at which we go right
-- http://www.wolframalpha.com/input/?i=Binomial%5B40%2C+20%5D
problem15 = 137846528820

--------------------------------------------------------------------------------
-- Problem 16
--------------------------------------------------------------------------------

sumDigits :: Integer -> Integer
sumDigits n
    | n < 0 = error "Non-negative integers only"
    | n <= 9 = n
    | otherwise = (rem n 10) + (sumDigits $ div n 10)

problem16 = sumDigits $ 2^1000

--------------------------------------------------------------------------------
-- Problem 17
--------------------------------------------------------------------------------

twentyFirst = ["one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty"]
decades = ["twenty","thirty","forty","fifty","sixty","seventy","eigthy","ninety"]

numToWords n
    | n <= 20 = twentyFirst !! (n-1)
    | (n < 100) && (n > 10) && ((n `rem` 10) == 0) = decades !! ((n `div` 10) - 2)
    | 20 < n && n <= 29 = "twenty-" ++ (twentyFirst !! (n-21))
    | 30 < n && n <= 39 = "thirty-" ++ (twentyFirst !! (n-31))
    | 40 < n && n <= 49 = "forty-" ++ (twentyFirst !! (n-41))
    | 50 < n && n <= 59 = "fifty-" ++ (twentyFirst !! (n-51))
    | 60 < n && n <= 69 = "sixty-" ++ (twentyFirst !! (n-61))
    | 70 < n && n <= 79 = "seventy-" ++ (twentyFirst !! (n-71))
    | 80 < n && n <= 89 = "eigthy-" ++ (twentyFirst !! (n-81))
    | 90 < n && n <= 99 = "ninety-" ++ (twentyFirst !! (n-91))
    | n == 100 = "one hundred"
    | 100 < n && n <= 999 = numToWords (n `div` 100) ++ " hundred" ++ (if (n `rem` 100) == 0 then "" else " and " ++ numToWords (n `rem` 100))
    | n == 1000 = "one thousand"
    | otherwise = error $ show n

countLetters "" = 0
countLetters (x:xs) =
    case x of
        ' ' -> countLetters xs
        '-' -> countLetters xs
        _ -> 1 + countLetters xs

--------------------------------------------------------------------------------
-- Problem 18
--------------------------------------------------------------------------------

stringToTriangle s = map (map (read::(String -> Integer))) $ map words $ lines s

triangle0 = stringToTriangle "3\n\
\7 4\n\
\2 4 6\n\
\8 5 9 3"

triangle1 = stringToTriangle "\
\75\n\
\95 64\n\
\17 47 82\n\
\18 35 87 10\n\
\20 04 82 47 65\n\
\19 01 23 75 03 34\n\
\88 02 77 73 07 63 67\n\
\99 65 04 28 06 16 70 92\n\
\41 41 26 56 83 40 80 70 33\n\
\41 48 72 33 47 32 37 16 94 29\n\
\53 71 44 65 25 43 91 52 97 51 14\n\
\70 11 33 28 77 73 17 78 39 68 17 57\n\
\91 71 52 38 17 14 91 43 58 50 27 29 48\n\
\63 66 04 68 89 53 67 30 73 16 69 87 40 31\n\
\04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

makePairs :: [Integer] -> [(Integer,Integer)]
makePairs [x,y] = [(x,y)]
makePairs [x,y,z] = [(x,y),(y,z)]
makePairs (x:xs) = 
    let ((a,b):ps) = makePairs xs in
    (x,a):(a,b):ps


--maxPath :: reversed triangle -> max path sums at the bottom
maxPath :: [[Integer]] -> [Integer]
maxPath [[x]] = [x]
maxPath [[b,c],[a]] = [a+b,a+c]
maxPath (x:xs) =
    let mp = maxPath xs
        mmp = map (uncurry max) $ makePairs mp
        x' = init $ tail x
        xh = head x
        xl = last x in
        ((xh+(head mp)):(zipWith (+) x' mmp)) ++ [xl + (last mp)]

p18 = maximum $ maxPath $ reverse triangle1

--------------------------------------------------------------------------------
-- Problem 19
--------------------------------------------------------------------------------

-- I may be cheating, but I'm not counting the fuckin' Sundays

--isSunday y m d = (==) 7 $ (\(a,b,c) -> c) $ toWeekDate $ fromGregorian y m d

--p19 = length $ filter (== True) $ [isSunday y m 1 | y <- [1901..2000], m <- [1..12]]

--solved, but there's an issue with the dependencies

--------------------------------------------------------------------------------
-- Problem 20
--------------------------------------------------------------------------------

fact :: Integer -> Integer
fact n = product [1..n]

problem20 = sumDigits $ fact 100

--------------------------------------------------------------------------------
-- Problem 21
--------------------------------------------------------------------------------

p21 = sum $ filter isAmicable [2..10000]

--------------------------------------------------------------------------------
-- Problem 22
--------------------------------------------------------------------------------

names = (read $ "[" ++ (unsafePerformIO $ readFile "names.txt") ++ "]")::[String]

wordToNum = sum . map (\x -> ord x - 64)

p22 = sum [idx*val | (idx,val) <- zip values [1..]] where values = map wordToNum $ sort names

--------------------------------------------------------------------------------
-- Problem 23
--------------------------------------------------------------------------------

isAbundant n = (sumProperDivisors n) > n

abundants = filter isAbundant [2..28123]

--Ordered.nubSort [x + y | x <- abundants, y <- abundants, x <= y]

p23 = sum $ Ordered.minus [1..28122] (foldl' Ordered.union [] [[x + y | y <- abundants, y >= x, x+y <= 28122] | x <- abundants])

--------------------------------------------------------------------------------
-- Problem 24
--------------------------------------------------------------------------------

p24 = (sort $ permutations [0..9]) !! ((10^6)-1)

--------------------------------------------------------------------------------
-- Problem 25
--------------------------------------------------------------------------------

-- the explicit formula for F_n yields

problem25 = floor $ (999*(log 10) + (log $ sqrt 5)) / (log $ ((1 + (sqrt 5))/2)) + 0.5

--------------------------------------------------------------------------------
-- Problem 26
--------------------------------------------------------------------------------

periodLength n = head [p | p <- divisors $ totient' n, expMod 10 p n == 1]

p26 = fst $ last $ sortBy (compare `on` snd) [(n, periodLength n) | n <- takeWhile (<= 1000) primes, not $ Ordered.subset (map fst $ decomp n) [2,5]]

--------------------------------------------------------------------------------
-- Problem 27
--------------------------------------------------------------------------------

p27 = uncurry (*) $ fst $ head $ sortBy (flip compare `on` snd) $ [((a,b),length $ takeWhile (== True) $ [isPrime $ n^2 + a*n + b | n <- [0..]]) | a <- [-1000..1000], b <- takeWhile (<= 1000) primes]

--------------------------------------------------------------------------------
-- Problem 28
--------------------------------------------------------------------------------

spiralDiag1 nmax = [(2*n+1)^2 | n <- [1..nmax]]
spiralDiag2 nmax = [2*n*(2*n-1)+1 | n <- [1..nmax]]
spiralDiag3 nmax = [(2*n)^2 + 1 | n <- [1..nmax]]
spiralDiag4 nmax = [(2*n)^2 + (2*n) + 1 | n <- [1..nmax]]

p28 :: Integer
p28 = 1 + sum (spiralDiag1 nmax) + sum (spiralDiag2 nmax) + sum (spiralDiag3 nmax) + sum (spiralDiag4 nmax) where nmax = 500

spiralDiags nmax = transpose [spiralDiag1 nmax, spiralDiag2 nmax, spiralDiag3 nmax, spiralDiag4 nmax]

--------------------------------------------------------------------------------
-- Problem 29
--------------------------------------------------------------------------------

p29 = length $ Ordered.nubSort [a^b | a <- [2..100], b  <- [2..100]]

--------------------------------------------------------------------------------
-- Problem 30
--------------------------------------------------------------------------------

nthPowerOfDigits n p
    | n < 0 = error "Non-negative integers only"
    | n <= 9 = n^p
    | otherwise = let (q,r) = quotRem n 10 in (r^p) + nthPowerOfDigits q p

p30 = sum [n | n <- [2..999999], nthPowerOfDigits n 5 == n]

--------------------------------------------------------------------------------
-- Problem 31
--------------------------------------------------------------------------------

coinSums :: [Integer] -> Integer -> [[Integer]]
coinSums l 0 = [[]]
coinSums [] s = []
coinSums (l@(x:xs)) s
    | x > s = coinSums xs s
    | x <= s = (map (x:) $ coinSums l (s - x)) ++ (coinSums xs s)

p31 = length $ coinSums [200, 100, 50, 20, 10, 5, 2, 1] 200

--------------------------------------------------------------------------------
-- Problem 32
--------------------------------------------------------------------------------

p32 = sum $ Ordered.nubSort $ map (uncurry (*)) $ [(p,q) | p <- [2..9], q <- [1234..9876], let r = p*q, sort ((digits p) ++ (digits q) ++ (digits r)) == [1..9]] ++ [(p,q) | p <- [12..98], q <- [123..987], let r = p*q, sort ((digits p) ++ (digits q) ++ (digits r)) == [1..9]]

--------------------------------------------------------------------------------
-- Problem 33
--------------------------------------------------------------------------------

problem33 = denominator $ product [((10*a+b)%(10*c+d)) | a <- [1..9], b <- [1..9], c <- [1..9], d <- [1..9], c+d /= 0, let r = ((10*a+b) % (10*c+d)), ((a==d) && (r == (b%c))) || ((b==c) && (r == (a%d))) || ((a==c) && (r == (b%d))) || ((a==d) && (r == (b%c))), ((10*a+b) % (10*c+d)) < 1]

--------------------------------------------------------------------------------
-- Problem 34
--------------------------------------------------------------------------------

p34 = sum [n | n <- [10..2540160], (==) n $ sum $ map fact $ digits n ]

--------------------------------------------------------------------------------
-- Problem 35
--------------------------------------------------------------------------------

rotate :: [a] -> [a]
rotate l = (last l):(init l)

rotations :: [a] -> [[a]]
rotations l = take (length l) (iterate rotate l)

fromDigits l = foldl' (\a b -> b + 10*a) 0 l

p35 = length $ [p | p <- takeWhile (<= 10^6) primes, all isPrime $ map fromDigits $ rotations $ digits p]

--------------------------------------------------------------------------------
-- Problem 36
--------------------------------------------------------------------------------

isPalindromic b n =
    let d = digitsBase' b n in
    (==) d $ reverse d

improperPalindromes' b 1 = [0..(b-1)]
improperPalindromes' b 2 = map ((*) (b+1)) [0..(b-1)]
improperPalindromes' b l =
    let p = improperPalindromes' b (l-2)
        n = 1 + b^(l-1) in
    concat [[y*n + b*x | x <- p] | y <- [0..b-1]]

palindromes' b 1 = [1..(b-1)]
palindromes' b 2 = map ((*) (b+1)) [1..(b-1)]
palindromes' b l =
    let p = improperPalindromes' b (l-2)
        n = 1 + b^(l-1) in
    concat [[y*n + b*x | x <- p] | y <- [1..b-1]]

palindromes b =
    concat [palindromes' b n | n <- [1..]]

palindromes10 = palindromes 10

p36 = sum $ takeWhile (<= 10^6) $ Ordered.isect (palindromes 2) (palindromes 10)

--------------------------------------------------------------------------------
-- Problem 37
--------------------------------------------------------------------------------

isTruncatablePrime f n
    | isPrime n == False = False
    | n < 10 = True
    | otherwise = isTruncatablePrime f $ f n

isRightTruncatablePrime = isTruncatablePrime truncatR
isLeftTruncatablePrime = isTruncatablePrime truncatL

truncatR n = n `div` 10
truncatL n = rem n $ 10 ^ (integerLogBase 10 n)

p37 = sum $ take 11 $ filter (>= 10) $ filter isRightTruncatablePrime $ filter isLeftTruncatablePrime primes

--------------------------------------------------------------------------------
-- Problem 38
--------------------------------------------------------------------------------

concatenatedProduct x l = fromDigits $ concat $ map digits $ map (* x) l

p38 = last $ sort [z | x <- [1..98765], let ymax = floor $ 9 / (fromIntegral (1 + integerLogBase 10 x)), y <- [2..ymax], let z = concatenatedProduct x [1..y], isPandigital19 z]

--------------------------------------------------------------------------------
-- Problem 39
--------------------------------------------------------------------------------

--integerSquareRoot :: Integer -> Integer
--integerSquareRoot _ = error "Stub"

--isSquare :: Integer -> Bool
--isSquare _ = error "Stub"

maxNumTriangles p = fst $ last $ sortBy (compare `on` snd) $ map (\l -> (head l,length l)) $ group $ sort $ filter (\x -> x <= p) [a+b+integerSquareRoot (a^2+b^2) | a <- [1..p], b <- [a..p-(2*a)], isSquare (a^2 + b^2), b <= p-b-a]

problem39 = maxNumTriangles 1000

--------------------------------------------------------------------------------
-- Problem 40
--------------------------------------------------------------------------------

champernowne_d n = (concat [digits n | n <- [1..]]) !! (n-1)

p40 = product $ map champernowne_d [10^p | p <- [0..6]]

--------------------------------------------------------------------------------
-- Problem 41
--------------------------------------------------------------------------------

makePandigitals n = sort $ map (foldl' (\a b -> b + 10*a) 0) $ permutations [1..n]

p41 = Ordered.isect (foldl' Ordered.merge [] $ [makePandigitals n | n <- [1..7]]) primes

--------------------------------------------------------------------------------
-- Problem 42
--------------------------------------------------------------------------------

p42_words :: [Integer]
p42_words = map (fromIntegral . wordToNum) $ ((read ("[" ++ p42_words_ ++ "]"))::([String])) where p42_words_ = unsafePerformIO $ readFile "words.txt"

p42 = length $ filter (flip elem triangle_nums) p42_words where triangle_nums = takeWhile (<= 200) [n*(n+1) `div` 2 | n <- [1..]]

--------------------------------------------------------------------------------
-- Problem 43
--------------------------------------------------------------------------------

slice from to xs = take (to - from + 1) (drop from xs)
slice3 n = slice n (2+n)

p43 = sum $ map fromDigits [l | l <- permutations [0..9], all (== 0) [(fromDigits $ slice3 x l) `mod` p | x <- [1..7], let p = primes !! (x-1)]]

--------------------------------------------------------------------------------
-- Problem 44
--------------------------------------------------------------------------------

is_pentagonal x
    | not $ isSquare $ 24*x + 1 = False
    | otherwise = (1 + integerSquareRoot (24*x + 1)) `mod` 6 == 0

p44 = fst $ head [(d,x) | let n = 20000000, d <- takeWhile (<= n) pentagonal_nums, x <- takeWhile (<= n) pentagonal_nums, let y = d + x, let s = x + y, is_pentagonal y, is_pentagonal s]

--------------------------------------------------------------------------------
-- Problem 45
--------------------------------------------------------------------------------

triangle_nums = [n*(n+1) `div` 2 | n <- [1..]]
pentagonal_nums = [n*(3*n-1) `div` 2 | n <- [1..]]
hexagonal_nums = [n*(2*n-1) | n <- [1..]]

p45 = (foldl1 Ordered.isect [triangle_nums, pentagonal_nums, hexagonal_nums]) !! 2

--------------------------------------------------------------------------------
-- Problem 46
--------------------------------------------------------------------------------

p46 = head $ Ordered.minus (Ordered.minus [3,5..] primes) (Ordered.nubSort [r | p <- take 1000 primes, q <- take 1000 squares, let r = p+2*q, (==) 1 $ r `mod` 2])
    where squares = map (flip (^) 2) [1..]

--------------------------------------------------------------------------------
-- Problem 47
--------------------------------------------------------------------------------

--problem47 = [(n,n+1,n+2,n+3) | n <- [1..], (==) 4 (length $ uniqueFactors n), (==) 4 (length $ uniqueFactors $ n+1), (==) 4 (length $ uniqueFactors $ n+2), (==) 4 (length $ uniqueFactors $ n+3)]

distinctPrimeFactors = length . decomp

distinctPrimeFactorsAbove m n = distinctPrimeFactors n >= m

p47 = head [x | x <- [1..], all (distinctPrimeFactorsAbove 4) [x+eps | eps <- [0..3]]]

--------------------------------------------------------------------------------
-- Problem 48
--------------------------------------------------------------------------------

-- x^p [m]
expMod x 0 m = 1
expMod x 1 m = x `mod` m
expMod x p m = (if (p `mod` 2) == 0 then ((expMod x (p `div` 2) m)^2) else (x * (expMod x (p - 1) m))) `mod` m

problem48 = foldl' (\acc n -> (acc + (expMod n n (10^10))) `mod` (10^10)) 0 [1..1000]

--------------------------------------------------------------------------------
-- Problem 49
--------------------------------------------------------------------------------

problem49 =
    let fourDigitsPrimes = takeWhile ((>=) 9999) $ dropWhile ((>) 1000) primes
        digits n
            | n < 0 = error "Non-negative integers only"
            | n <= 9 = [n]
            | otherwise = (rem n 10) : (digits $ div n 10)
        sortedDigits n = id $! sort $ digits n
        eligibleSet = filter (\(a,b,c) -> (sortedDigits b) == (sortedDigits c)) $ filter (\(a,b,c) -> (sortedDigits a) == (sortedDigits b)) $ [(a,b,2*b-a) | a <- fourDigitsPrimes, b <- fourDigitsPrimes, b > a, (2*b-a) `Ordered.member` fourDigitsPrimes]
        (a,b,c) = eligibleSet !! 1 in
        c+10000*(b+10000*a)

--------------------------------------------------------------------------------
-- Problem 50
--------------------------------------------------------------------------------

sumConsecutive' n l =
    let (a,b) = splitAt n l
        la = length a in
    if la < n then [] else (sum a):sumConsecutive' n b

sumConsecutive n l =
    concat $ transpose [sumConsecutive' n $ drop m l | m <- [0..(n-1)]]

primeSumOfConsecutivePrimes m n =
    let p = takeWhile (<= m) primes in
    Ordered.isect p $ sumConsecutive n $ p

p50 = snd $ last [(n,p) | n <- [501,503..599], p <- primeSumOfConsecutivePrimes (10^6) n]

--------------------------------------------------------------------------------
-- Problem 51
--------------------------------------------------------------------------------

genMask 0 = []
genMask 1 = [[0],[1]]
genMask n = [x:l | l <- genMask (n-1), x <- [0,1]]

genSubstitutions n =
    let d = digits n
        mask = genMask $ length d in
    init $ tail $ [[n' | r <- [0..9], let n' = fromDigits $ zipWith (\a b -> if a == 0 then r else b) m d, length (digits n') == length d] | m <- mask]

getMinPrime n =
    case filter ((==8) . length) $ map (filter isPrime) $ genSubstitutions n of
        [] -> Nothing
        l -> Just $ minimum $ map minimum l

p51 = head $ catMaybes [getMinPrime p |p <- primes]

--------------------------------------------------------------------------------
-- Problem 52
--------------------------------------------------------------------------------

sameDigits _ (d:dl) =
    and $ map ((==) d) dl

p52 = fst $ head $ filter (uncurry sameDigits) [(x,[sort $ digits (k*x) | k <- [1..6]]) | x <- [1..]]

--------------------------------------------------------------------------------
-- Problem 53
--------------------------------------------------------------------------------

pascal l = 1:(zipWith (+) l (drop 1 l)) ++ [1]

p53 = sum . map (sum . map (\x -> if x >= 10^6 then 1 else 0)) $ take 100 (iterate pascal [1,1])

--------------------------------------------------------------------------------
-- Problem 55
--------------------------------------------------------------------------------

revAndAdd n =
    n + (foldl' (\x y -> x*10 + y) 0 $ digits' n)

isLychrel n = isLychrel' n 50
    where
        isLychrel' n 0 = True
        isLychrel' n it = if isPalindromic 10 (revAndAdd n) then False else isLychrel' (revAndAdd n) (it - 1)

p55 = length $ filter isLychrel [1..10000]

--------------------------------------------------------------------------------
-- Problem 56
--------------------------------------------------------------------------------

p56 = maximum $ map sumDigits [(a^b) | a <- [1..99], b <- [1..99]]

--------------------------------------------------------------------------------
-- Problem 57
--------------------------------------------------------------------------------

p57 = length $ filter (\x -> (length $ digits $ numerator x) > (length $ digits $ denominator x)) frac
    where frac = drop 1 $ reverse $ convergents $ reverse $ take (1+1000) $ (1:repeat 2)

--------------------------------------------------------------------------------
-- Problem 58
--------------------------------------------------------------------------------

--p58 = head [m | n <- [5000..], let m = 2*n + 1, let sd = (spiralDiags m), let p = length $ filter isPrime sd, let q = length sd, (p%q) < (1/10)]

p58 = (\x -> x-1) $ (*2) $ snd $ head $ filter (\(x,n) -> (x < 1%10)) $ drop 1 $ zip (zipWith (%) (scanl (+) 0 $ psd) ([1+4*n | n <- [0..]])) [1..]
    where
        psd = map (length . filter isPrime) $ spiralDiags 100000
        sd = map (length) $ spiralDiags 1000000
--------------------------------------------------------------------------------
-- Problem 59
--------------------------------------------------------------------------------

readInt :: String -> Int
readInt = read

cipher1 = map (readInt) $ splitOn "," $ init $ unsafePerformIO $ readFile "cipher1.txt"

cipher1_3 = map (map snd) $ groupBy (\a b -> fst a == fst b) $ sortBy (compare `on` fst) $ zip (cycle [0,1,2]) cipher1

p59_key = map (xor (ord 'e') . fromIntegral) $ [snd $ (reverse $ sortBy (compare `on` fst) $ rle $ sort $ cipher1_3 !! i) !! j | (i,j) <- [(0,1),(1,2),(2,1)]]

decrypt :: [Int] -> [Int] -> [Int]
decrypt k c = zipWith xor (cycle k) c
encrypt = decrypt

p59 = sum $ decrypt p59_key cipher1

--------------------------------------------------------------------------------
-- Problem 60
--------------------------------------------------------------------------------

isPrimePairSetWith a l = all isPrime ([c | b <- l, a /= b, let c = fromDigits (digits a ++ digits b)] ++ [c | b <- l, a /= b, let c = fromDigits (digits b ++ digits a)])

isPrimePairSet l = all isPrime [c | a <- l, b <- l, a /= b, let c = fromDigits (digits a ++ digits b)]

primePairSets m 1 = [[p] | p <- takeWhile (<= m) primes]
primePairSets m n = [ l | b <- primePairSets m (n-1), a <- dropWhile (<= (head b)) (takeWhile (<= m) primes), let l = a:b, isPrimePairSetWith a b]

p60 = sum $ head $ primePairSets 10000 5

--------------------------------------------------------------------------------
-- Problem 61
--------------------------------------------------------------------------------

isCyclic l = isCyclic' (l ++ [head l])

isCyclic' [] = True
isCyclic' [l] = True
isCyclic' (x:y:l) = ((take 2 $ digits y) == (drop 2 $ digits x)) && isCyclic' (y:l)

heptagonal_nums = [div (n*(5*n-3)) 2 | n <- [1..]]
octagonal_nums = [n*(3*n-2) | n <- [1..]]

onlyFourDigits = dropWhile (<= 999) . takeWhile (<= 9999)

--, d <- onlyFourDigits hexagonal_nums, e <- onlyFourDigits heptagonal_nums, f <- onlyFourDigits octagonal_nums
--[l | a <- onlyFourDigits triangle_nums, b <- onlyFourDigits squares, c <- onlyFourDigits pentagonal_nums, let l = [a,b,c,d,e,f], isCyclic l]

--------------------------------------------------------------------------------
-- Problem 62
--------------------------------------------------------------------------------

p62 = (\x -> x^3) $ minimum $ map (minimum . snd) $ filter (\(a,b) -> length b == 5) $ Map.toAscList $ foldl' (\a b -> Map.insertWith (++) (sort $ digits $ b^3) [b] a) Map.empty [1..9999]

--------------------------------------------------------------------------------
-- Problem 63
--------------------------------------------------------------------------------

p63 = length [x^p | x <- [1..9], p <- [1..100], 10^(p-1) <= x^p]

--------------------------------------------------------------------------------
-- Problem 65
--------------------------------------------------------------------------------

--continuedFractionToConvergents :: [Integer] -> [Ratio Integer]
--continuedFractionToConvergents [] = error "Empty continued fraction"
--continuedFractionToConvergents [x] = [x % 1]
--continuedFractionToConvergents (x:xs) =
--    let cv = continuedFractionToConvergents xs in ((x % 1) + ((fromInteger 1) / (head cv))):cv

--convergents' cf = ([h],[k])
convergents' [a0] = ([a0],[1])
convergents' [a1,a0] = ([1+a1*a0,a0],[a1,1])
convergents' (a2:as) =
    let (h,k) = convergents' as in (((a2 * (h !! 0)) + (h !! 1)):h,((a2 * (k !! 0)) + (k !! 1)):k)

convergents l =
    let (hl,kl) = convergents' l in zipWith (%) hl kl

problem65 = sumDigits ((reverse $ fst (convergents' $ reverse $ (2:1:concat [[2*k,1,1] | k <- [1..100]]))) !! 99)

--------------------------------------------------------------------------------
-- Problem 67
--------------------------------------------------------------------------------

triangle2 = stringToTriangle $ unsafePerformIO $ readFile "triangle.txt"

p67 = maximum $ maxPath $ reverse $ triangle2

--------------------------------------------------------------------------------
-- Problem 69
--------------------------------------------------------------------------------

removeConsecutive :: [Integer] -> [Integer]
removeConsecutive [] = []
removeConsecutive [x] = [x]
removeConsecutive (a:(b:l)) =
    if a == b then removeConsecutive (b:l) else (a:(removeConsecutive (b:l)))

uniqueFactors :: Integer -> [Integer]
uniqueFactors = removeConsecutive . factors

totient :: Integer -> Double
totient n =
    (fromInteger n) * (product $ map (\x -> 1 - (1/(fromInteger x))) $ uniqueFactors n)

totient' :: Integer -> Integer
totient' n = product $ map (\(p,k) -> (p^(k-1))*(p-1)) $ decomp n

n_over_phi_n :: Integer -> Double
n_over_phi_n n =
    (fromInteger n) / (totient n)

problem69 = last $ takeWhile ((>) 1000000) $ scanl1 (*) primes

--------------------------------------------------------------------------------
-- Problem 70
--------------------------------------------------------------------------------

p70 = (\(a,b,c) -> a) $ head $ sortBy (compare `on` (\(a,b,c) -> c)) [(n,m,(fromIntegral n)/(fromIntegral m)) | n <- [2..(10^7-1)], let m = totient' n, m `mod` 9 == n `mod` 9, (sort $ digits m) == (sort $ digits n)]

--------------------------------------------------------------------------------
-- Problem 71
--------------------------------------------------------------------------------

p71 = numerator $ maximum [i % j | j <- [2..1000000], let i = floor $ 3.0 * (fromIntegral j) / 7.0, (i%j) < (3%7)]

--------------------------------------------------------------------------------
-- Problem 72
--------------------------------------------------------------------------------

p72 = sum [totient' n | n <- [1..1000000]]

--------------------------------------------------------------------------------
-- Problem 73
--------------------------------------------------------------------------------

p73 = length $ Ordered.nubSort [x | d <- [1..12000], i <- [1..(d-1)], let x = i%d, (1%3) < x, x < (1%2)]

--------------------------------------------------------------------------------
-- Problem 74
--------------------------------------------------------------------------------

isLooping l =
    let il = init l
        x = last l in
    elem x il

-- l is an infinite list
lengthNonRepeating l =
    head [n-1 | n <- [2..], isLooping $ take n l]

sumFactorialDigits = sum . map fact . digits'

lengthDigitFactorialChain = lengthNonRepeating . iterate sumFactorialDigits

p74 = length [n | n <- [1..10^6], let l = lengthDigitFactorialChain n, l == 60]

--------------------------------------------------------------------------------
-- Problem 75
--------------------------------------------------------------------------------

pythagoreanTriplets kMax mMax = [[a,b,c] | k <- [1..kMax], m <- [1..mMax], n <- [1..m-1], (m-n) `mod` 2 == 1, 
    gcd m n == 1, let a = k * (m^2 - n^2), let b = k*2*m*n, let c = k * (m^2 + n^2)]

--------------------------------------------------------------------------------
-- Problem 76
--------------------------------------------------------------------------------

binomial' 0 0 = 1
binomial' 1 0 = 1
binomial' n 1 = n
binomial' n p
    | p < 0 = 0
    | p > n = 0
    | otherwise = div (product [(n-p+1)..n]) (product [2..p])

binomial n p = binomial' n $ min (n-p) p

--------------------------------------------------------------------------------
-- Problem 76
--------------------------------------------------------------------------------

summation 0 = [[]]
summation 1 = [[1]]
summation n = Ordered.nubSort [Ordered.insertBag p s | p <- [1..(n-1)], s <- summation (n-p)]

summation' :: Int -> IntMap.IntMap [[Int]]
summation' 1 = IntMap.singleton 1 [[1]]
summation' n =
    let sums = summation' (n-1)
        newsum = Ordered.nubSort [Ordered.insertBag p s | p <- [1..(n-1)], s <- fromJust $ IntMap.lookup (n-p) sums] in
    IntMap.insert n newsum sums

p76 = length $ fromJust $ IntMap.lookup n $ summation' n where n = 60

--p76 = 190569292 - 1
--well-known value...

--------------------------------------------------------------------------------
-- Problem 77
--------------------------------------------------------------------------------

primeSummation 0 = [[]]
primeSummation n = Ordered.nubSort [Ordered.insertBag p s | p <- takeWhile (<= n) primes, s <- primeSummation (n-p)]

primeSummation' :: Int -> IntMap.IntMap [[Int]]
primeSummation' 0 = IntMap.singleton 0 [[]]
primeSummation' n =
    let sums = primeSummation' (n-1)
        newsum = Ordered.nubSort [Ordered.insertBag p s | p <- takeWhile (<= n) primes, s <- fromJust $ IntMap.lookup (n-p) sums] in
    IntMap.insert n newsum sums

p77 = head [n | (n,m) <- IntMap.toList $ IntMap.map length $ primeSummation' 100, m >= 5000]

--------------------------------------------------------------------------------
-- Problem 78
--------------------------------------------------------------------------------

g :: Int -> Int
g k = div (k * (3*k - 1)) 2

generalisedPentagonalNumbers = map g $ concat [[k, -k] | k <- [1..]]

integerPartition = memoize f
    where
        f 0 = 1
        f 1 = 1
        f n = sum [ s * (integerPartition (n - gk)) | (s,gk) <- zip (cycle [1,1,-1,-1]) (takeWhile (<= n) generalisedPentagonalNumbers)]

integerPartitionMod :: Int -> Int -> Int
integerPartitionMod = memoize2 f
    where
        f m 0 = 1
        f m 1 = 1
        f m n = mod (sum [ s * (integerPartitionMod m (n - gk)) | (s,gk) <- zip (cycle [1,1,-1,-1]) (takeWhile (<= n) generalisedPentagonalNumbers)]) m

p78 = snd $ head $ dropWhile ((/= 0) . fst) $ zip (map (integerPartitionMod (10^6)) [0..]) [0..]

--------------------------------------------------------------------------------
-- Problem 79
--------------------------------------------------------------------------------

keylog = map (digits . (read::(String -> Integer))) $ lines $ unsafePerformIO $ readFile "keylog.txt"
keylog_ = Ordered.nubSort keylog

genSeq 1 alphabet = [[a] | a <- alphabet]
genSeq n alphabet = [a:s | a <- alphabet, s <- genSeq (n-1) alphabet]

isPrefix [] _ = True
isPrefix (x:xs) [] = False
isPrefix (x:xs) (y:ys) = if x == y then isPrefix xs ys else False

isSubsequence [] _ = True
isSubsequence (x:xs) [] = False
isSubsequence (x:xs) (y:ys) = if x == y then isPrefix xs ys else isSubsequence (x:xs) ys

--too slow
--[s | s <- genSeq 9 [0..9], all (flip isSubsequence s) keylog]

keepShortest l =
    let len = minimum $ map length l in
    filter (\x -> (length x <= len)) l

lcmSeqs :: (Eq a) => [a] -> [a] -> [[a]]
lcmSeqs [] [] = [[]]
lcmSeqs x [] = [x]
lcmSeqs [] y = [y]
lcmSeqs (x:xs) (y:ys) =
    let ll = keepShortest $ lcmSeqs xs ys in
    if x == y then map (x:) ll else keepShortest ((map (x:) $ lcmSeqs xs (y:ys)) ++ (map (y:) $ lcmSeqs (x:xs) ys))

p79 = fromDigits $ head $ foldl' (\l x -> (keepShortest $ Ordered.nubSort $ concat $ map (lcmSeqs x) l)) [[]] $ keylog_

--------------------------------------------------------------------------------
-- Problem 80
--------------------------------------------------------------------------------

p80 = sum [f n | n <- [2..99], not $ Ordered.member n $ map (^2) [1..]]
    where
        f x = sum $ digits $ integerSquareRoot $ x*(10^(2*(100-(1+(integerLogBase 10 $ integerSquareRoot x)))))

--------------------------------------------------------------------------------
-- Problem 81
--------------------------------------------------------------------------------

listToArr m = let n = (fromIntegral $ length m) in A.array ((1,1),(n,n)) [((r,c),v) | (r,l) <- zip [1..] m, (c,v) <- zip [1..] l]

small_matrix = [[131,673,234,103,18],[201,96,342,965,150],[630,803,746,422,111],[537,699,497,121,956],[805,732,524,37,331]]
small_arr = listToArr small_matrix

big_matrix :: [[Integer]]
big_matrix = map (map read) $ map (splitOn ",") $ lines $ unsafePerformIO $ readFile "matrix.txt"

big_arr :: A.Array (Integer, Integer) Integer
big_arr = listToArr big_matrix

pathValueForCell _ _ _ (Just v) = Just v
pathValueForCell Nothing _ _ Nothing = Nothing
pathValueForCell _ Nothing _ Nothing = Nothing
pathValueForCell (Just l) (Just u) cell Nothing = Just $ cell + (min l u)

iterPathSumTwoWays arr parr =
    let (r,c) = snd (A.bounds arr) in
    A.array ((1,1),(r,c)) $ [((i,j),v) | i <- [2..r], j <- [2..c], let cell = (A.!) arr (i,j), let pcell = (A.!) parr (i,j), let up = (A.!) parr (i-1,j), let lp = (A.!) parr (i,j-1), let v = pathValueForCell lp up cell pcell] ++
                    [((i,j),v) | let i = 1, j <- [2..c], let cell = (A.!) arr (i,j), let pcell = (A.!) parr (i,j), let lp = (A.!) parr (i,j-1), let v = pathValueForCell lp lp cell pcell] ++
                    [((i,j),v) | let j = 1, i <- [2..r], let cell = (A.!) arr (i,j), let pcell = (A.!) parr (i,j), let up = (A.!) parr (i-1,j), let v = pathValueForCell up up cell pcell] ++
                    [((1,1),Just $ (A.!) arr (1,1))]

-- BAAAD
--pathSumTwoWays' acc (i,j) arr =
--    trace (show (i,j)) (
--        let (bi,bj) = snd $ A.bounds arr
--            nacc = acc + (A.!) arr (i,j) in
--        if (i,j) == (bi,bj)
--        then nacc
--        else (
--            if ((i < bi) && (j < bj))
--            then min (pathSumTwoWays' nacc (i+1,j) arr) (pathSumTwoWays' nacc (i,j+1) arr)
--            else (
--                if ((i == bi) && (j < bj))
--                then pathSumTwoWays' nacc (i,j+1) arr
--                else pathSumTwoWays' nacc (i+1,j) arr
--            )
--        )
--    )

pathSumTwoWays' arr parr =
    let (r,c) = snd (A.bounds arr) in
    case (A.!) parr (r,c) of
        Nothing -> pathSumTwoWays' arr $ iterPathSumTwoWays arr parr
        Just v -> v

pathSumTwoWays arr =
    let (r,c) = snd (A.bounds arr)
        parr = A.array (A.bounds arr) $ (((1,1),(Just $ (A.!) arr (1,1)))):[((i,j),v) | i <- [1..r], j <- [1..c], (i,j) /= (1,1), let v = Nothing] in
    pathSumTwoWays' arr parr

p81 = pathSumTwoWays big_arr

--------------------------------------------------------------------------------
-- Problem 85
--------------------------------------------------------------------------------

numRectanglesOfSize (a,b) (p,q) =
    (div p a) * (div q b) * ((rem p a) + 1) * ((rem q b) + 1)

--numRectangles (p,q) = sum [numRectanglesOfSize (a,b) (p,q) | a <- [1..p], b <- [1..q]]
numRectangles :: (Int, Int) -> Int
--numRectangles (p,q) = sum [n*m*(r+1)*(s+1) | a <- [1..p]::[Int], b <- [1..q]::[Int], let (n,r) = quotRem p a, let (m,s) = quotRem q b]
numRectangles (p,q) = (binomial (p+1) 2) * (binomial (q+1) 2)

p85 = (uncurry (*)) $ fst $ minimumBy (compare `on` ((\x -> abs (x - 2000000)) . snd)) [((a,b),numRectangles (a,b)) | a <- [1..2000], b <- [1..a]]

--------------------------------------------------------------------------------
-- Problem 86
--------------------------------------------------------------------------------

squareShortestLineDistance p q r =
    let [a,b,c] = sort [p,q,r] in
    (a+b)^2 + c^2

squareShortestLineDistance' p qr =
    (qr)^2 + p^2

numIntegerSolutionsCuboidRoute m = sum [1 | p <- [1..m], q <- [1..p], r <- [1..q], isSquare $ squareShortestLineDistance p q r]

--numIntegerSolutionsCuboidRoute' :: Integer -> Integer
numIntegerSolutionsCuboidRoute' m = trace (show m) $ sum [n | p <- [1..m], qr <- [(2)..(2*p)], let n = ((min qr ((2+2*p)-qr))) `div` 2, isSquare $ squareShortestLineDistance' p qr]

p86 = searchFromTo (\n -> numIntegerSolutionsCuboidRoute' n >= 1000000) 1000 2000

--------------------------------------------------------------------------------
-- Problem 87
--------------------------------------------------------------------------------

primesBelow n = takeWhile (<= n) primes

p87 = length $ takeWhile (<= 50000000) $ Ordered.nubSort [p^2 + q^3 + r^4 | p <- primesBelow 7500, q <- primesBelow 400, r <- primesBelow 90]

--------------------------------------------------------------------------------
-- Problem 91
--------------------------------------------------------------------------------

isRightTriangle xP yP xQ yQ =
    let [a2,b2,c2] = sort [xP^2+yP^2,(xQ-xP)^2+(yQ-yP)^2,xQ^2+yQ^2] in ((xP,yP) /= (0,0) && (xQ,yQ) /= (0,0) && (xP,yP) /= (xQ,yQ) && (c2 == (a2 + b2)))


numRightTrianglesWithOrigin n =
    length $ [((xP,yP),(xQ,yQ)) | xP <- [0..n], yP <- [0..n], xQ <- [0..n], yQ <- [0..n], (xP,yP) < (xQ,yQ), isRightTriangle xP yP xQ yQ]

p91 = numRightTrianglesWithOrigin 50

--------------------------------------------------------------------------------
-- Problem 92
--------------------------------------------------------------------------------

squareDigit = sum . map (^2) . digits

squareDigitChain 1 = 0
squareDigitChain 89 = 1
squareDigitChain n = squareDigitChain $ squareDigit n

p92 = sum [squareDigitChain n | n <- [1..10^7]]

--------------------------------------------------------------------------------
-- Problem 94
--------------------------------------------------------------------------------

problem94 = sum [3*a+eps | a <- [2..10^9], eps <- [1,-1], isSquare ((3*a+eps)*(a-eps)), 3*a+eps <= 10^9]

--------------------------------------------------------------------------------
-- Problem 97
--------------------------------------------------------------------------------

p97 = mod (1 + (28433 * (expMod 2 7830457 (10^10)))) (10^10)

--------------------------------------------------------------------------------
-- Problem 99
--------------------------------------------------------------------------------

p99 = fst $ last $ sortBy (compare `on` snd)[(i,(fromInteger b)*(log $ fromInteger a)) | (i,[a,b]) <- zip [1..] base_exp]
    where
        base_exp = map (map (read::(String -> Integer))) $ map (splitOn ",") $ lines $ unsafePerformIO $ readFile "base_exp.txt"

--------------------------------------------------------------------------------
-- Problem 101
--------------------------------------------------------------------------------

p101_u :: Integer -> Ratio Integer
p101_u n = evalPoly (poly LE $ take 11 $ cycle ([1,-1]::[Ratio Integer])) (fromIntegral n)

p101_op :: Integer -> Integer -> Ratio Integer
p101_op k n = polyInterp [(i % 1, p101_u i) | i <- [1..k]] (n % 1)

p101 = numerator $ sum [p101_op k (k+1) | k <- [1..10]]

--------------------------------------------------------------------------------
-- Problem 102
--------------------------------------------------------------------------------

det2d :: (Integer,Integer) -> (Integer,Integer) -> Integer
det2d (x0,y0) (x1,y1) = x0*y1 - x1*y0

barycentricCoordinatesOfOrigin :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer) -> (Rational, Rational)
barycentricCoordinatesOfOrigin (xA,yA) (xB,yB) (xC,yC) =
    let vecAO = (-xA,-yA)
        vecAB = (xB-xA,yB-yA)
        vecAC = (xC-xA,yC-yA)
        d = (fromIntegral $ det2d vecAB vecAC)
        u = (fromIntegral $ det2d vecAO vecAC) % d
        v = (fromIntegral $ det2d vecAO vecAB) % (-d) in
    (u,v)

originInTriangle [xA,yA,xB,yB,xC,yC] =
    let (u,v) = barycentricCoordinatesOfOrigin (xA,yA) (xB,yB) (xC,yC) in
        (u > 0) && (v > 0) && (u < 1) && (v < 1) && (u+v < 1)

p102 = length $ filter originInTriangle $ triangles
    where triangles = (map (map read . splitOn ",") $ lines $ unsafePerformIO $ readFile "triangles.txt")::([[Integer]])

--------------------------------------------------------------------------------
-- Problem 104
--------------------------------------------------------------------------------

--[k | (k,f) <- zip [0..] (fibsMod $ 10^10), is_pandigital 1 9 (fromIntegral f)]

p104 = head [k | (k,f) <- zip [0..] (fibsMod $ 10^10), is_pandigital19 (fromIntegral f), let ff = explicitFib k, let d = digits ff, (==) [1..9] $ sort $ take 9 d]

--------------------------------------------------------------------------------
-- Problem 112
--------------------------------------------------------------------------------

increasingNumbers' 1 = [1..9]
increasingNumbers' n = [ m' | m <- increasingNumbers' (n-1), let r = rem m 10, x <- [r..9], let m' = (10*m) + x]

increasingNumbers = concat [increasingNumbers' n | n <- [1..]]

decreasingNumbers' 1 = [1..9]
decreasingNumbers' n = [ m' | m <- decreasingNumbers' (n-1), let r = rem m 10, x <- [0..r], let m' = (10*m) + x]

decreasingNumbers = concat [decreasingNumbers' n | n <- [1..]]

nonBouncyNumbers = Ordered.union increasingNumbers decreasingNumbers

bouncyNumbers = Ordered.minus [1..] nonBouncyNumbers

p112 = snd $ fst $ head $ dropWhile ((< 0) . snd) $ zipWith (\a b -> ((a,b),100*a - 99*b)) [1..] bouncyNumbers

--------------------------------------------------------------------------------
-- Problem 113
--------------------------------------------------------------------------------

numIncreasingNumbers' n = binomial (n+8) 8

numDecreasingNumbers' n = (binomial (n+9) 9) - 1

numNonBouncyNumbers' n = (numIncreasingNumbers' n) + (numDecreasingNumbers' n) - 9

p113 = sum [numNonBouncyNumbers' m | m <- [1..100]]

--------------------------------------------------------------------------------
-- Problem 118
--------------------------------------------------------------------------------

partitions :: [Integer] -> Int -> [[[Integer]]]
partitions l 1 = [[l]]
partitions l 2 = [[a,b] | n <- [1..(length l - 1)], let (a,b) = splitAt n l]
partitions l n = [([a,b]++xs) | (x:xs) <- partitions l (n-1), n <- [1..(length x - 1)], let (a,b) = splitAt n x]

allPartitions l = concat [partitions l n | n <- [1..length l]] 

p118 = length $ Ordered.nubSort $ concat $ map (filter (all isPrime) . map (Ordered.nubSort . map fromDigits) . allPartitions) $ permutations [1..9]

--------------------------------------------------------------------------------
-- Problem 119
--------------------------------------------------------------------------------

isDigitPowerSum n
    | n < 10 = error "A number must contain at least two digits to have a sum."
    | otherwise = let d = sumDigits n in
        (d > 1) && (n == d ^ (integerLogBase d n))

p119 = digitPowerSums !! (30-1)
    where digitPowerSums = foldl1 Ordered.union $ [filter isDigitPowerSum [x^p | x <- [4..100]] | p <- [2..10]]

--------------------------------------------------------------------------------
-- Problem 120
--------------------------------------------------------------------------------

p120 = sum [r_max a | a <- [3..1000]]
    where
        r_max a
            | a `mod` 2 == 1 = 2*a*(a `div` 2)
            | a `mod` 2 == 0 = 2*((a `div` 2) - 1)*a
        r' a n
            | n `mod` 2 == 0 = 2
            | n `mod` 2 == 1 = 2*n*a
        r a n = mod (r' a n) (a^2)

--------------------------------------------------------------------------------
-- Problem 122
--------------------------------------------------------------------------------

-- TODO: learn about addition chains

m_ub' 0 = 0
m_ub' 1 = 0
m_ub' p = (if (p `mod` 2) == 0 then m_ub' (p `div` 2) else m_ub' (p - 1)) + 1

m_ub = map m_ub' [0..200]

mkMul :: [(Int,(Int,[Int]))] -> [(Int,(Int,[Int]))]
mkMul l = Ordered.nubSort [(z, (lz, pz)) | (x,(lx,px)) <- l, (y,(ly,py)) <-  takeWhile ((<= (200 - x)) . fst) $ dropWhile ((< x) . fst) l, x <= y, let z = x + y, z <= 200, let pz = ((px `Ordered.union` py)++(if z >= 2 then [z] else [])), let lz = length pz, lz <= (m_ub !! z)]

muls = iterate mkMul ((0,(0,[])) : (map (\x -> (2^x, (x, take x $ map (2^) [1..]))) [0..7]))

p122 = sum $ map snd $ map (\l -> (fst (l !! 0), minimum $ map (fst . snd) l)) $ groupBy ((==) `on` fst) $ muls !! 6
--p122 = 1582

--------------------------------------------------------------------------------
-- Problem 123
--------------------------------------------------------------------------------

p123 = fst $ fst $ head $ dropWhile ((<= 10^10) . snd) [((n,pn),r n pn) | (n,pn) <- zip [1..] primes]
    where r n pn = (expMod (pn - 1) n (pn^2) + expMod (pn + 1) n (pn^2)) `mod` (pn^2)

--------------------------------------------------------------------------------
-- Problem 124
--------------------------------------------------------------------------------

p124 = (sort [(rad n,n) | n <- [1..100000]]) !! (10000 - 1)

--------------------------------------------------------------------------------
-- Problem 125
--------------------------------------------------------------------------------

--sumOfConsecutiveSquares = foldl' Ordered.union [] [sumConsecutive n squares | n <- [2..]]

p125 = sum $ foldl' Ordered.union [] [Ordered.isect palin $ takeWhile (<= 10^8) $ sumConsecutive n squares | n <- [2..675]]
    where
        palin = takeWhile (<= 10^8) palindromes10

--------------------------------------------------------------------------------
-- Problem 127
--------------------------------------------------------------------------------

p127 = sum [c | c <- [1..1000-1], a <- [1..(1 + c `div` 2)], let b = c - a, a < b, (gcd a b) == 1, (gcd a c) == 1, (gcd b c) == 1, ((rad a)*(rad b)*(rad c)) < c]

--------------------------------------------------------------------------------
-- Problem 129
--------------------------------------------------------------------------------

p129_a n
    | (/=) 1 $ gcd n 10 = error "n and 10 must be coprime"
    | otherwise = head [k | k <- [1..], (==) 1 $ expMod 10 k (9*n)]

p129_a' n
    | (/=) 1 $ gcd n 10 = error "n and 10 must be coprime"
    | otherwise = head [k | k <- (divisors $ totient' (9*n)), (==) 1 $ expMod 10 k (9*n)]

p129 = head [n | n <- [(10^6)+1,(10^6)+3..], (==) 1 $ gcd n 10, let a_n = p129_a' n, a_n >= (10^6)]

--------------------------------------------------------------------------------
-- Problem 130
--------------------------------------------------------------------------------

p130 = sum $ take 25 [n | n <- [2..], (gcd n 10) == 1, ((flip mod) (p129_a' n) (n-1)) == 0, not $ isPrime n]

--------------------------------------------------------------------------------
-- Problem 132
--------------------------------------------------------------------------------

p132 = sum $ take 40 [p | p <- primes, p /= 3, primeDividesRepunit (10^9) p]

--------------------------------------------------------------------------------
-- Problem 133
--------------------------------------------------------------------------------

repunit k = repunit' k 1

repunit' 1 acc = acc
repunit' k acc = repunit' (k-1) (1 + 10 * acc)

repunitMod k m = repunitMod' k m 1
    where
    repunitMod' 1 m acc = acc
    repunitMod' k m acc = repunitMod' (k-1) m ((1 + 10 * acc) `mod` m)

primeDividesRepunit n p
    | p == 3 = error "p = 3"
    | otherwise = (==) 1 $ expMod 10 n p

dividingPrimes = [11,17,41,73,101,137,251,257,271,353,401,449,641,751,1201,1409,1601,3541,4001,4801,5051,9091,10753,15361,16001,19841,21001,21401,24001,25601,27961,37501,40961,43201,60101,62501,65537,69857,76001,76801]
--[p | p <- takeWhile ((>) (10^5)) primes, p /= 3, primeDividesRepunit (10^100) p]

p133 = sum $ Ordered.minus [p | p <- takeWhile ((>) (10^5)) primes] dividingPrimes

--------------------------------------------------------------------------------
-- Problem 148
--------------------------------------------------------------------------------

binomial7 = UV.generate 49 (\x -> flip mod 7 $ binomial (div x 7) (mod x 7))

binomialMod :: Integral a => a -> a -> a -> a
binomialMod m n p = foldl' (\a b -> ((a*b) `mod` p)) 1 $ zipWith (\mi ni -> binomial mi ni) (digitsBase' p m) (digitsBase' p n)

binomialMod7 :: Int -> Int -> Int
binomialMod7 m n = foldl' (\a b -> ((a*b) `mod` 7)) 1 $! zipWith (\mi ni -> (UV.!) binomial7 (7*mi+ni)) (digitsBase' 7 m) (digitsBase' 7 n)

p148_28id = ([length $ filter (/= 0) $ [binomialMod7 n p | n <- [0..((7^m)-1)], p <- [0..n]] | m <- [0..4]] == [28^m | m <- [0..4]])

--------------------------------------------------------------------------------
-- Problem 160
--------------------------------------------------------------------------------

removeTrailingZeros n =
    let (q,r) = quotRem n 10 in
    if r == 0 then removeTrailingZeros q else n

factorialTrailingDigits_ 1 = 1
factorialTrailingDigits_ n =
    let p = n * factorialTrailingDigits_ (n-1) in
    (removeTrailingZeros p) `mod` (10^5)

factorialTrailingDigits' 1 acc = acc
factorialTrailingDigits' n acc =
    --trace (show acc) (
    let m = (removeTrailingZeros n)
        p = (removeTrailingZeros (m * acc)) `mod` (10^5) in
    factorialTrailingDigits' (n-1) p --)

factorialTrailingDigits :: Int -> Int
factorialTrailingDigits n = trace (show n) $ factorialTrailingDigits' n 1

--------------------------------------------------------------------------------
-- Problem 179
--------------------------------------------------------------------------------

p179 = let nd = [(n,nDivisors n) | n <- [2..(10^7)-1]] in seq nd $ length $ filter (\((n,dn),(m,dm)) -> (dn == dm)) $ zip nd $ tail nd

--------------------------------------------------------------------------------
-- Problem 182
--------------------------------------------------------------------------------

--f e n = length $ filter (== True) $ [m == expMod m e n | m <- [0..(n-1)]]

--numUnconcealedMessages e n = f (n-1) 0
--    where
--        f (-1) acc = acc
--        f m acc = f (m-1) (acc + (if m == expMod m e n then 1 else 0))

--main = do putStrLn $ show $ let p = 19 in let q = 37 in let phi = (p-1)*(q-1) in let n = p*q in [(e,unconcealed) | e <- [1..phi-1], gcd phi e == 1, let unconcealed = numUnconcealedMessages e n]

--------------------------------------------------------------------------------
-- Problem 187
--------------------------------------------------------------------------------

--p187 = length $ foldl' Ordered.union [] [[x*y | y <- takeWhile (<= div n x) primes] | x <- takeWhile (<= (div n 2)) primes]
--    where n = 1000000

-- a wee bit too slow
p187 = length $ Ordered.nubSort [x*y | x <- takeWhile (<= (div n 2)) ps, y <- takeWhile (<= div n x) ps]
    where
        n = 10*(10^7)
        ps = takeWhile (<= n) primes

--------------------------------------------------------------------------------
-- Problem 188
--------------------------------------------------------------------------------

-- let p = 1777, n = 10^8
-- since p^n = 1 [n], we can apply f: a -> p^a mod n

hyperExpPrime p 1 m = p `mod` m
hyperExpPrime p k m = expMod p (hyperExpPrime p (k-1) m) m

p188 = hyperExpPrime 1777 1855 (10^8)

--------------------------------------------------------------------------------
-- Problem 190
--------------------------------------------------------------------------------

-- using pencil & paper & Langrangian method we get the expression of Pm

p190 = sum [p m | m <- [2..15]] where
    p m =
        let m' = (fromIntegral m)::Double in
        floor $ (fromIntegral $ product [i^i | i <- [1..m]]) * ((2::Double) / (m'+1.0))^(div (m*(m+1)) 2)


--------------------------------------------------------------------------------
-- Problem 206
--------------------------------------------------------------------------------

p206l = 1020304050607080900
sqp206l = integerSquareRoot p206l
p206u = 1929394959697989990
sqp206u = 1 + (integerSquareRoot p206u)

p206 = integerSquareRoot $ head $ filter ((== [1,2,3,4,5,6,7,8,9,0]) . map snd . filter (\(a,b) -> a `mod` 2 ==0) . zip [0..] . digits) [p | s <- [sqp206l,(sqp206l+10)..sqp206u], (s `mod` 100 == 30) || (s `mod` 100 == 70), let p = s^2]

--------------------------------------------------------------------------------
-- Problem 211
--------------------------------------------------------------------------------

p211 = 1 + sum [n | n <- [1..(64*10^6-1)], isSquare $ sumSquaredDivisors n]

--------------------------------------------------------------------------------
-- Problem 235
--------------------------------------------------------------------------------

u :: Double -> Integer -> Double
u r k = (fromInteger (900 - 3*k))*r^(k-1)

s r n = sum [u r k | k <- [1..n]]

-- f a > 0, f b < 0
findZero f a b =
    if (b - a) < 1e-12
    then (a+b)/2
    else (
        let c = (a+b)/2 in
        if f c < 0 then findZero f a c else findZero f c b
    )

p235 = take 14 $ show $ findZero (\r -> s r 5000 + 6e11) 1.001 1.01

--------------------------------------------------------------------------------
-- Problem 243
--------------------------------------------------------------------------------

resilience :: Integer -> Double
resilience n = (totient n) / (fromIntegral (n-1))

resilience' :: Integer -> Double
resilience' n = (resilience n) - ((15499)::Double)/94744

--l1 = take 10 $ filter (\n -> (94744 * totient' n) < (15499 * (n-1))) $ map (foldl' (*) 1) $ subsequences primes
--l2 = take 10 $ filter (\n -> (94744 * totient' n) < (15499 * (n-1))) $ (scanl1 lcm [1..])

-- + guesswork

-- http://www.research.att.com/~njas/sequences/A060735

p243 :: Integer
p243 = product [2,2,2,3,5,7,11,13,17,19,23]

--------------------------------------------------------------------------------
-- Problem 249
--------------------------------------------------------------------------------

--sumFromSubsets :: [Int] -> Int -> IntMap.IntMap Int
--sumFromSubsets l m = f l $ IntMap.singleton 0 1
--    where
--        f [] acc = acc
--        f (0:xs) acc = IntMap.map (\a -> ((a*2) `mod` m)) acc
--        f (x:xs) acc = 
--            let acc2 = IntMap.foldlWithKey (\newmap k v -> IntMap.insert (k+x) v newmap) IntMap.empty acc in
--            f xs $ IntMap.unionWith (\a b -> ((a+b) `mod` m)) acc acc2

--p249 = foldl' f 0 [n | (s,n) <- (IntMap.toList sfs), (isPrime . fromIntegral) s] --foldl' f 0 $ 
--    where
--        sfs = sumFromSubsets (takeWhile (<= 2000) primes) (10^16)
--        f a b = (a+b) `mod` (10^16)


sumFromSubsets' :: [Int] -> Int -> Int -> UV.Vector Int
sumFromSubsets' l m s = f l $! UV.generate s $ \x -> if x == 0 then 1 else 0
    where
        f [] acc = acc
        f l v = f' l v
        f' [] _ = error "Shouldn't happen"
        f' (x:xs) v =
            let w = (UV.++) (UV.replicate x 0) (UV.unsafeTake (s-x) v) in
            f xs $! UV.zipWith (\a b -> (a+b) `mod` m) v w

p249 :: Int
p249 = foldl' f 0 [n | (s,n) <- (UV.toList $ UV.imap (\a b -> (a,b)) sfs), n > 0, (isPrime . fromIntegral) s] --foldl' f 0 $ 
    where
        p = (takeWhile (<= 5000) primes)
        sfs = sumFromSubsets' p tenToTheSixteenth (sum p + 1)
        f a b = (a+b) `mod` tenToTheSixteenth

--------------------------------------------------------------------------------
-- Problem 250
--------------------------------------------------------------------------------

modSumFromSubsets' :: [Int] -> Int -> Int -> UV.Vector Int
modSumFromSubsets' l m n = f l $! UV.generate m $ \x -> if x == 0 then 1 else 0
    where
        f [] v = v
        f (0:xs) v = f xs $! UV.map (\a -> ((a*2) `mod` n)) v
        f (x:xs) v =
            let w = UV.unsafeBackpermute v (UV.generate m (\y -> (y + x) `mod` m)) in
            f xs $! UV.zipWith (\a b -> (a+b) `mod` n) v w

p250 :: Int
p250 = ((UV.!) (modSumFromSubsets' ([expMod n n 250 | n <- [1..250250]]) 250 tenToTheSixteenth) 0) - 1

--------------------------------------------------------------------------------
-- Problem 342
--------------------------------------------------------------------------------

--[q | n <- [2..], isCube (n * (totient' n))]

--[(q,q3) | q <- [1..], let q3 = q^3]

-- injective
invertNTimesPhi :: Integer -> Maybe Integer
invertNTimesPhi 0 = Nothing
invertNTimesPhi 1 = Just 1
invertNTimesPhi 2 = Just 2
invertNTimesPhi y = 
    let (a,b) = last $ decomp y in
    if b `mod` 2 /= 1
    then Nothing
    else (
        let n = a ^ (div (b + 1) 2) in
        if a == 2 then Just n else (
            let nphi = n * (totient' n) in
            case invertNTimesPhi (div y nphi) of
                Nothing -> Nothing
                Just y1 -> if y == ((n*y1) * (totient' (n * y1))) then Just (n*y1) else Nothing
        )
    )

p342 = sum $ takeWhile (<= (10^10)) $ sort $ map (fromJust . invertNTimesPhi . (^3)) $ filter (isJust . invertNTimesPhi . (^3)) [q | q <- [1..5*(10^6)]]

--p342 = concat $ filter (isJust . invertNTimesPhi . (^3)) [q | q <- [1..2000]]

--------------------------------------------------------------------------------
-- Problem 357
--------------------------------------------------------------------------------

isPrimeGeneratingInteger n = all (isPrime . (\d -> d + (n `div ` d))) $ divisors' n

p357 = 1 + (sum $ filter isPrimeGeneratingInteger [n | p <- takeWhile (<= (10^8)) primes, let n = p-1, n `mod` 4 == 2, not $ isSquare n])

--------------------------------------------------------------------------------
-- Problem 448
--------------------------------------------------------------------------------

average :: Integral a => [a] -> a
average l = 
    let (s,t) = foldl' (\(x,y) n -> ((x+n),(y+1))) (0,0) l in s `div` t

--a :: Integral a => a -> a
--a n = average [lcm i n | i <- [1..n]]

--------------------------------------------------------------------------------
-- Problem 463
--------------------------------------------------------------------------------

--f 1 = 1
--f 3 = 3
--f n
--    | n `rem` 2 == 0 = f (n `div` 2)
--    | n `rem` 4 == 1 = 2 * f ((n+1) `div` 2) - f ((n-1) `div` 4)
--    | n `rem` 4 == 3 = 3 * f ((n-1) `div` 2) - 2 * (f ((n-1) `div` 4))

-- f n = greatest odd divisor of n... nope

--s n = sum [f i | i <- [1..n]]

--s' 0 = 0
--s' 1 = 1
--s' n
--    | n `rem` 2 == 0 = let p = n `div` 2 in (p^2) + s' p
--    | n `rem` 2 == 1 = let p = n `div` 2 in ((p+1)^2) + s' p

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

--putShow = putStrLn . show
main = do putStrLn $ show p342
--main = do mapM_ (putStrLn . show) $ filter (isJust . invertNTimesPhi . (^3)) [q | q <- [1..5*(10^6)]]
--main = do putShow $ length $ Ordered.nubSort [x | d <- [1..12000], i <- [1..(d-1)], let x = i%d, (1%3) < x, x < (1%2)]
--main = do putStrLn $ show $ let p = 19 in let q = 37 in let phi = (p-1)*(q-1) in let n = p*q in [(e,unconcealed) | e <- [1..phi-1], gcd phi e == 1, let unconcealed = numUnconcealedMessages e n]
--main = do putStrLn $ show [s | s <- genSeq 9 [0..9], all (flip isSubsequence s) keylog]
