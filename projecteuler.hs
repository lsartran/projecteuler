-- there's an excellent library that provides us with a list of primes: let's use it
import Data.Numbers.Primes (primes)

import Data.List
import Data.List.Ordered (member)
import Data.Function (on)
import Debug.Trace (trace)
import qualified Data.IntMap as Map
--import Math.NumberTheory.Powers.Squares (isSquare, integerSquareRoot)
import Data.Ratio

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

problem2 :: Integer
problem2 = sum [x | x <- takeWhile ((>=) 4000000) fibs, (x `mod` 2) == 0]

--------------------------------------------------------------------------------
-- Problem 3
--------------------------------------------------------------------------------

factors :: Integer -> [Integer]
factors n = 
    case filter (\p -> (n `mod` p) == 0) $ takeWhile ((>=) n) primes of
        [] -> []
        p:_ -> p:factors (n `div` p)

largestPrimeFactor n =
    last $ factors n

decomp :: Integer -> [(Integer,Int)]
decomp n = map (\g -> (head g, length g)) $ group $ factors n

nFactors :: Integer -> Int
nFactors =
    product . map ((+1) . snd) . decomp

rad :: Integer -> Integer
rad = product . map fst . decomp

problem3 = largestPrimeFactor 600851475143

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

n = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

digits' n
    | n < 0 = error "Non-negative integers only"
    | n <= 9 = [n]
    | otherwise = (rem n 10) : (digits' $ div n 10)

digits = reverse . digits'

problem8 :: Integer
problem8 = last $ sort [a*b*c*d*e | (a,b,c,d,e) <- zip5 (digits n) (drop 1 $ digits n) (drop 2 $ digits n) (drop 3 $ digits n) (drop 4 $ digits n)]

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
-- Problem 12
--------------------------------------------------------------------------------

tri :: Integer -> Integer
tri n = (n * (n+1)) `quot` 2

p12 = head $ filter (((<=) 500) . nFactors) [tri n | n <- [1..]]

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

memoizedCollatzLength' :: Int -> Map.IntMap Int -> (Int, Map.IntMap Int)
memoizedCollatzLength' 1 _ = (1, Map.singleton 1 1)
memoizedCollatzLength' n tbl =
    if Map.member n tbl
    then ((Map.!) tbl n, tbl)
    else (
        if n `rem` 2 == 0
        then let (l,tbl0) = memoizedCollatzLength' (n `div` 2) tbl in (l+1, Map.insert n (l+1) tbl0)
        --else let (l,tbl0) = memoizedCollatzLength' (3*n+1) tbl in (l+1, Map.insert n (l+1) tbl0)
        else let (l,tbl0) = memoizedCollatzLength' ((3*n+1) `div` 2) tbl in (l+2, Map.insert n (l+2) tbl0)
    )

memoizedCollatzLength :: Int -> Int
memoizedCollatzLength n = fst $ memoizedCollatzLength' n Map.empty

problem14 = snd $ Map.foldlWithKey' g (1,1) htbl
    where
        g (maxl,maxn) n l = if (n > 1000000) || (l <= maxl) then (maxl,maxn) else (l,n)
        htbl = foldl' f Map.empty [1..1000000]
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
-- Problem 20
--------------------------------------------------------------------------------

fact :: Integer -> Integer
fact n = product [1..n]

problem20 = sumDigits $ fact 100

--------------------------------------------------------------------------------
-- Problem 25
--------------------------------------------------------------------------------

-- the explicit formula for F_n yields

problem25 = floor $ (999*(log 10) + (log $ sqrt 5)) / (log $ ((1 + (sqrt 5))/2)) + 0.5


--------------------------------------------------------------------------------
-- Problem 33
--------------------------------------------------------------------------------

problem33 = denominator $ product [((10*a+b)%(10*c+d)) | a <- [1..9], b <- [1..9], c <- [1..9], d <- [1..9], c+d /= 0, let r = ((10*a+b) % (10*c+d)), ((a==d) && (r == (b%c))) || ((b==c) && (r == (a%d))) || ((a==c) && (r == (b%d))) || ((a==d) && (r == (b%c))), ((10*a+b) % (10*c+d)) < 1]

--------------------------------------------------------------------------------
-- Problem 39
--------------------------------------------------------------------------------

integerSquareRoot :: Integer -> Integer
integerSquareRoot _ = error "Stub"

isSquare :: Integer -> Bool
isSquare _ = error "Stub"

maxNumTriangles p = fst $ last $ sortBy (compare `on` snd) $ map (\l -> (head l,length l)) $ group $ sort $ filter (\x -> x <= p) [a+b+integerSquareRoot (a^2+b^2) | a <- [1..p], b <- [a..p-(2*a)], isSquare (a^2 + b^2), b <= p-b-a]

problem39 = maxNumTriangles 1000

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
        eligibleSet = filter (\(a,b,c) -> (sortedDigits b) == (sortedDigits c)) $ filter (\(a,b,c) -> (sortedDigits a) == (sortedDigits b)) $ [(a,b,2*b-a) | a <- fourDigitsPrimes, b <- fourDigitsPrimes, b > a, (2*b-a) `member` fourDigitsPrimes]
        (a,b,c) = eligibleSet !! 1 in
        c+10000*(b+10000*a)

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

n_over_phi_n :: Integer -> Double
n_over_phi_n n =
    (fromInteger n) / (totient n)

problem69 = last $ takeWhile ((>) 1000000) $ scanl1 (*) primes

--------------------------------------------------------------------------------
-- Problem 47
--------------------------------------------------------------------------------

--problem47 = [(n,n+1,n+2,n+3) | n <- [1..], (==) 4 (length $ uniqueFactors n), (==) 4 (length $ uniqueFactors $ n+1), (==) 4 (length $ uniqueFactors $ n+2), (==) 4 (length $ uniqueFactors $ n+3)]

--------------------------------------------------------------------------------
-- Problem 57
--------------------------------------------------------------------------------

--numDigits :: Integer -> Integer
--numDigits n
--    | n < 0 = error "Non-negative integers only"
--    | n <= 9 = 1
--    | otherwise = 1 + (numDigits $ div n 10)

--continuedFractionToRatio :: [Integer] -> [Ratio Integer]
--continuedFractionToRatio [x] = [x % 1]
--continuedFractionToRatio (x:xs) =
--    let cf = continuedFractionToRatio xs in
--    (cf ++ [(fromInteger x) + ((fromInteger 1) / (head $ cf))])

--filter (\cf -> ((numDigits $ numerator cf) > (numDigits $ denominator cf))) $ continuedFractionToRatio $ take 100 $ (1:(repeat 2))

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
-- Problem 76
--------------------------------------------------------------------------------

--binomial n p =


--p76 = 

--------------------------------------------------------------------------------
-- Problem 94
--------------------------------------------------------------------------------

problem94 = sum [3*a+eps | a <- [2..10^9], eps <- [1,-1], isSquare ((3*a+eps)*(a-eps)), 3*a+eps <= 10^9]

--------------------------------------------------------------------------------
-- Problem 127
--------------------------------------------------------------------------------

p127 = sum [c | c <- [1..120000-1], a <- [1..(1 + c `div` 2)], let b = c - a, a < b, (gcd a b) == 1, (gcd a c) == 1, (gcd b c) == 1, rad(a*b*c) < c]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main = do
    --putStrLn $ show problem1
    --putStrLn $ show problem2
    --putStrLn $ show problem3
    --putStrLn $ show problem5
    --putStrLn $ show problem7
    --putStrLn $ show problem9
    --putStrLn $ show problem10
    --putStrLn $ show problem16
    --putStrLn $ show problem20
    --putStrLn $ show problem25
    putStrLn $ show p127
