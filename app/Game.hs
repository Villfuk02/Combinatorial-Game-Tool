{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Game where

import Data.Bits
import Data.List
import Data.Maybe
import Data.MemoTrie
import Data.Ratio
import Debug.Trace
import GHC.Num

-------------------- SETTINGS --------------------

-- | enable debug prints
isInDebugMode = False

-- | enable memoization results of many functions
memoizeResults = True

-------------------- DEBUG UTILS --------------------

-- | debug print the input and output of a one-argument function
traceCall1Arg s f a = if isInDebugMode then trace ("    " ++ s ++ " " ++ show a) (let r = f a in trace ("    -> " ++ show r) r) else f a

-- | debug print the input and output of a two-argument function
traceCall2Arg s f a b = if isInDebugMode then trace ("    " ++ s ++ " " ++ show a ++ " ; " ++ show b) (let r = f a b in trace ("    -> " ++ show r) r) else f a b

-------------------- MEMOIZATION UTILS --------------------

-- | memoize a function call if enabled
memoIfAble :: (Game -> t) -> (Game -> t) -> Game -> t
memoIfAble f fm = if memoizeResults then fm else f

-- | uncurry a four-argument tuple
uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

instance (HasTrie a, HasTrie b, HasTrie c, HasTrie d) => HasTrie (a, b, c, d) where
  newtype (a, b, c, d) :->: x = QuadTree (((a, b), (c, d)) :->: x)
  trie f = QuadTree (trie (f . quadToPairs))
  untrie (QuadTree t) = untrie t . pairsToQuad
  enumerate (QuadTree t) = [(quadToPairs k, v) | (k, v) <- enumerate t]

quadToPairs ((a, b), (c, d)) = (a, b, c, d)

pairsToQuad (a, b, c, d) = ((a, b), (c, d))

-------------------- LIST UTILS --------------------

-- | Try applying a list of functions to an argument until one succeeds or all fail
trySuccessive :: t -> [t -> Maybe a] -> Maybe a
trySuccessive g [] = Nothing
trySuccessive g (f : fs) = case f g of
  Nothing -> trySuccessive g fs
  Just x -> Just x

-- | replace elements of first list with elements of second list when the second list has a Just value
replaceWhenSomething :: [a] -> [Maybe a] -> [a]
replaceWhenSomething o r = [fromMaybe g h | (h, g) <- zip r o]

-- | remove elements of a list where the corresponding element of a second list is True
removeWhere :: [a] -> [Bool] -> [a]
removeWhere l p = [g | (k, g) <- zip p l, not k]

-- | the list of all Fibonacci numbers
fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

-- | delete the element at a given index in a list
deleteAt :: Int -> [a] -> [a]
deleteAt i l = take i l ++ drop (i + 1) l

-- | get the element at a given index in a list or Nothing if the index is out of bounds
atIndex :: [a] -> Int -> Maybe a
atIndex b i
  | i < 0 || i >= length b = Nothing
  | otherwise = Just $ b !! i

-- | replace the element at a given index in a list
replace :: [a] -> Int -> a -> [a]
replace l i e = take i l ++ [e] ++ drop (i + 1) l

-------------------- RATIONAL UTILS --------------------

instance HasTrie Rational where
  data Rational :->: x = RationalTrie ((Integer, Integer) :->: x)
  trie :: (Rational -> b) -> Rational :->: b
  trie f = RationalTrie (trie (f . uncurry (%)))
  untrie :: (Rational :->: b) -> Rational -> b
  untrie (RationalTrie d) r = untrie d (numerator r, denominator r)
  enumerate :: (Rational :->: b) -> [(Rational, b)]
  enumerate (RationalTrie d) = [(uncurry (%) k, v) | (k, v) <- enumerate d]

-- | print a Rational in a nice format
niceRational :: Rational -> String
niceRational r
  | denominator r == 1 = show (numerator r)
  | numerator r > denominator r = let w = numerator r `div` denominator r in show w ++ ":" ++ show (numerator r - w * denominator r) ++ "/" ++ show (denominator r)
  | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

-- | rationals with positive and negative infinity added
data ExtendedRational = NegativeInfinity | Finite Rational | PositiveInfinity deriving (Eq, Ord)

instance Show ExtendedRational where
  show NegativeInfinity = "-inf"
  show (Finite r) = niceRational r
  show PositiveInfinity = "+inf"

instance Num ExtendedRational where
  (+) NegativeInfinity PositiveInfinity = error ("Adding " ++ show NegativeInfinity ++ " and " ++ show PositiveInfinity ++ " is undefined")
  (+) PositiveInfinity NegativeInfinity = error ("Adding " ++ show PositiveInfinity ++ " and " ++ show NegativeInfinity ++ " is undefined")
  (+) NegativeInfinity _ = NegativeInfinity
  (+) PositiveInfinity _ = PositiveInfinity
  (+) (Finite r) (Finite s) = Finite (r + s)
  (+) a b = b + a
  (*) NegativeInfinity n
    | n == 0 = error ("Multiplying " ++ show NegativeInfinity ++ " and 0 is undefined")
    | n > 0 = NegativeInfinity
    | otherwise = PositiveInfinity
  (*) PositiveInfinity n
    | n == 0 = error ("Multiplying " ++ show PositiveInfinity ++ " and 0 is undefined")
    | n > 0 = PositiveInfinity
    | otherwise = NegativeInfinity
  (*) (Finite r) (Finite s) = Finite (r * s)
  (*) a b = b * a
  abs NegativeInfinity = PositiveInfinity
  abs PositiveInfinity = PositiveInfinity
  abs (Finite r) = Finite (abs r)
  negate NegativeInfinity = PositiveInfinity
  negate PositiveInfinity = NegativeInfinity
  negate (Finite r) = Finite (negate r)

  signum NegativeInfinity = -1
  signum PositiveInfinity = 1
  signum (Finite r) = Finite (signum r)

  fromInteger = Finite . fromInteger

-- | is an extended rational finite?
isFinite :: ExtendedRational -> Bool
isFinite (Finite _) = True
isFinite _ = False

-- | convert a finite extended rational to a rational
finiteToRational :: ExtendedRational -> Rational
finiteToRational (Finite r) = r

instance HasTrie ExtendedRational where
  data ExtendedRational :->: x = ExtendedRationalTrie x (Rational :->: x) x
  trie :: (ExtendedRational -> b) -> ExtendedRational :->: b
  trie f = ExtendedRationalTrie (f NegativeInfinity) (trie (f . Finite)) (f PositiveInfinity)
  untrie :: (ExtendedRational :->: b) -> ExtendedRational -> b
  untrie (ExtendedRationalTrie d _ _) NegativeInfinity = d
  untrie (ExtendedRationalTrie _ d _) (Finite r) = untrie d r
  untrie (ExtendedRationalTrie _ _ d) PositiveInfinity = d
  enumerate :: (ExtendedRational :->: b) -> [(ExtendedRational, b)]
  enumerate (ExtendedRationalTrie d e f) = [(NegativeInfinity, d)] ++ [(Finite r, g) | (r, g) <- enumerate e] ++ [(PositiveInfinity, f)]

-- | is a rational an integer?
isInteger :: Rational -> Bool
isInteger r = denominator r == 1

-------------------- GAME ORDERING --------------------

-- | the ordering of games, for values that aren't linearly ordered
data GameOrdering = LessThan | EqualTo | GreaterThan | ConfusedWith deriving (Eq, Show)

-- | convert an ordering to a game ordering
toGameOrdering :: Ordering -> GameOrdering
toGameOrdering LT = LessThan
toGameOrdering EQ = EqualTo
toGameOrdering GT = GreaterThan

-- | negate a game ordering
negateGameOrdering :: GameOrdering -> GameOrdering
negateGameOrdering LessThan = GreaterThan
negateGameOrdering GreaterThan = LessThan
negateGameOrdering x = x

-- \| the class of types that can be 'gameCompare'd
class GameOrd a where
  gameCompare :: a -> a -> GameOrdering
  infix 4 ==:
  (==:) :: a -> a -> Bool
  (==:) g h = gameCompare g h == EqualTo
  infix 4 /=:
  (/=:) :: a -> a -> Bool
  (/=:) g h = not $ g ==: h
  infix 4 <:
  (<:) :: a -> a -> Bool
  (<:) g h = gameCompare g h == LessThan
  infix 4 <=:
  (<=:) :: a -> a -> Bool
  (<=:) g h = g <: h || g ==: h
  infix 4 >:
  (>:) :: a -> a -> Bool
  (>:) g h = gameCompare g h == GreaterThan
  infix 4 >=:
  (>=:) :: a -> a -> Bool
  (>=:) g h = g >: h || g ==: h
  infix 4 ||:
  (||:) :: a -> a -> Bool
  (||:) g h = gameCompare g h == ConfusedWith
  infix 4 /|:
  (/|:) :: a -> a -> Bool
  (/|:) g h = not $ g ||: h
  infix 4 <|:
  (<|:) :: a -> a -> Bool
  (<|:) g h = g <: h || g ||: h
  infix 4 |>:
  (|>:) :: a -> a -> Bool
  (|>:) g h = g >: h || g ||: h

-------------------- GAME BOUNDS --------------------

-- | a pair of rationals representing the upper and lower bounds of a game's value
-- | note that the upper bound s written first for consistency with game notation
-- | the upper bound must be higher than the lower bound
-- | if bounds are fuzzy, then they extend infinitesimally beyond the actual bound values
data GameBounds = GameBounds {upperBound :: ExtendedRational, lowerBound :: ExtendedRational, isFuzzy :: Bool} deriving (Eq)

-- | from zero to zero
zeroBounds = GameBounds 0 0 False

-- | from zero to zero, but fuzzy
infinitesimalBounds = GameBounds 0 0 True

-- | from positive infinity to negative infinity
unlimitedBounds = GameBounds PositiveInfinity NegativeInfinity True

instance Show GameBounds where
  show (GameBounds u l False) = "[" ++ show u ++ " <- " ++ show l ++ "]"
  show (GameBounds u l True) = "(" ++ show u ++ " <- " ++ show l ++ ")"

instance Num GameBounds where
  (+) a b = GameBounds (upperBound a + upperBound b) (lowerBound a + lowerBound b) (isFuzzy a || isFuzzy b)
  fromInteger i = GameBounds (fromInteger i) (fromInteger i) False
  negate b = GameBounds (-lowerBound b) (-upperBound b) (isFuzzy b)
  (*) = undefined
  abs = undefined
  signum = undefined

-- | multiply game bounds ba an integer
multiplyBounds :: Integer -> GameBounds -> GameBounds
multiplyBounds i b = normalizeBounds $ GameBounds (fromInteger i * upperBound b) (fromInteger i * lowerBound b) (isFuzzy b)

-- | union a list of bounds
unionBounds :: [GameBounds] -> GameBounds
unionBounds [] = GameBounds 0 0 False
unionBounds b = normalizeBounds $ GameBounds (maximum (map upperBound b)) (minimum (map lowerBound b)) (any isFuzzy b)

-- | switch the upper and lower bounds of a game if the upper bound is lower than the lower bound
normalizeBounds :: GameBounds -> GameBounds
normalizeBounds b
  | lowerBound b <= upperBound b = b
  | otherwise = GameBounds (lowerBound b) (upperBound b) (isFuzzy b)

instance GameOrd GameBounds where
  gameCompare g h
    | upperBound g < lowerBound h = LessThan
    | lowerBound g > upperBound h = GreaterThan
    | lowerBound g == lowerBound h && upperBound g == upperBound h && not (isFuzzy g) && not (isFuzzy h) = EqualTo
    | otherwise = ConfusedWith

-------------------- PLAYER --------------------

-- | a player in a game
data Player = LeftPlayer | RightPlayer deriving (Eq)

instance Show Player where
  show LeftPlayer = "Left"
  show RightPlayer = "Right"

-- | the other player
opponent :: Player -> Player
opponent LeftPlayer = RightPlayer
opponent RightPlayer = LeftPlayer

-------------------- GAME --------------------

-- | definition of all games
-- | these are all abstract, for the concrete examples see CustomGame
data Game
  = Position {left :: [Game], right :: [Game]} -- an explicit position, with a list of left options and a list of right options
  | NumberGame Rational -- game representing a number value, only rationals with a power of 2 as their denominator are supported
  | StarGame Integer -- game representing a nimber value, only non-negative integers are supported
  | UpGame Integer -- game representing a multiple of ups or downs
  | Sum [Game] -- a sum of games
  | Multiple {multiple :: Integer, game :: Game} -- a game multiplied by an integer
  | Canonical {offset :: Rational, ups :: Integer, stars :: Integer, unsimplifiable :: [Game]} -- an abstract game in a canonical form - the list at the end represents the unsimplifiable parts of the game
  | CustomGame CustomGame -- some custom game, see CustomGame, these are separated for convenience
  deriving (Eq)

-- | the simpest game with value 0
zero = NumberGame 0

-- | the simplest game with value *
star = StarGame 1

-- | the simplest game with value ^
up = UpGame 1

-- | the simplest game with value v
down = UpGame (-1)

instance Show Game where
  show (Position l r) = "{ " ++ intercalate ", " (map show l) ++ " | " ++ intercalate ", " (map show r) ++ " }"
  show (NumberGame r) = niceRational r
  show (StarGame 1) = "*"
  show (StarGame i) = "*" ++ show i
  show (UpGame i)
    | i == 1 = "^"
    | i == -1 = "v"
    | i < 0 = "v" ++ show (-i)
    | otherwise = "^" ++ show i
  show (Sum []) = "[]"
  show (Sum gs) = intercalate " + " (map show gs)
  show (Multiple 1 g) = show g
  show (Multiple (-1) g) = "-(" ++ show g ++ ")"
  show (Multiple n g) = show n ++ "(" ++ show g ++ ")"
  show (Canonical 0 0 0 []) = "0"
  show (Canonical o 0 s u) =
    (if o /= 0 then show (NumberGame o) else "")
      ++ (if s /= 0 then show (StarGame s) else "")
      ++ (if u /= [] then " ++ " ++ show (Sum u) else "")
  show (Canonical o a s u) =
    (if o /= 0 then show (NumberGame o) ++ "+" else "")
      ++ show (UpGame a)
      ++ (if s /= 0 then show (StarGame s) else "")
      ++ (if u /= [] then " ++ " ++ show (Sum u) else "")
  show (CustomGame g) = show g

-- | see GameClass
instance GameClass Game where
  gamify = id

  options LeftPlayer (Position l _) = l
  options RightPlayer (Position _ r) = r
  options LeftPlayer (NumberGame r)
    | isInteger r = [NumberGame (r - 1) | r > 0]
    | otherwise = let d = 1 % denominator r in [NumberGame (r - d)]
  options RightPlayer (NumberGame r)
    | isInteger r = [NumberGame (r + 1) | r < 0]
    | otherwise = let d = 1 % denominator r in [NumberGame (r + d)]
  options _ (StarGame s) = map StarGame [0 .. s - 1]
  options _ (UpGame 0) = []
  options LeftPlayer (UpGame a)
    | a > 0 = [zero]
    | a < 0 = [Canonical 0 (a + 1) 1 []]
  options RightPlayer (UpGame a)
    | a > 0 = [Canonical 0 (a - 1) 1 []]
    | a < 0 = [zero]
  options p (Sum gs) = concatMap (moveInSum (options p) gs) [0 .. length gs - 1]
  options p (Multiple n g)
    | n == 0 = []
    | n > 0 = map (\o -> Sum [o, Multiple (n - 1) g]) (options p g)
    | n < 0 = map (\o -> Sum [negate o, Multiple (n + 1) g]) (options (opponent p) g)
  options p (Canonical o a s u) = options p (Sum [NumberGame o, UpGame a, StarGame s, Sum u])
  options p (CustomGame g) = options p g

  isImpartial (Position l r) = l == r && all isImpartial l && all isImpartial r
  isImpartial (NumberGame 0) = True
  isImpartial (NumberGame _) = False
  isImpartial (StarGame _) = True
  isImpartial (UpGame 0) = True
  isImpartial (UpGame _) = False
  isImpartial (Sum gs) = all isImpartial gs
  isImpartial (Multiple _ g) = isImpartial g
  isImpartial (Canonical o a _ u) = o == 0 && a == 0 && all isImpartial u
  isImpartial (CustomGame g) = isImpartial g

  getBounds (CustomGame g) = getBounds g
  getBounds g = traceCall1Arg "getBounds" (memoIfAble getBounds' getBoundsM) g

  simplifyOnce' :: Game -> Maybe Game
  simplifyOnce' (Position [] []) = Just zero
  simplifyOnce' (Position l r) =
    let res = trySimplifyAny l r
     in if isNothing res
          then trySuccessive (Position l r) [removeDuplicates, excludeExtremes, compactifyPosition, tryZeroOut]
          else Just $ uncurry Position (fromJust res)
  simplifyOnce' (NumberGame i) = Nothing
  simplifyOnce' (StarGame s)
    | s == 0 = Just zero
    | s < 0 = Just $ StarGame (-s)
    | otherwise = Nothing
  simplifyOnce' (UpGame 0) = Just zero
  simplifyOnce' (UpGame a) = Nothing
  simplifyOnce' (Sum []) = Just zero
  simplifyOnce' (Sum [g]) = Just g
  simplifyOnce' (Sum gs) = Just $ Canonical 0 0 0 gs
  simplifyOnce' (Multiple 0 _) = Just zero
  simplifyOnce' (Multiple 1 g) = Just g
  simplifyOnce' (Multiple (-1) (Position l r)) = Just $ Position (map negate r) (map negate l)
  simplifyOnce' (Multiple n (NumberGame r)) = Just $ NumberGame (fromInteger n * r)
  simplifyOnce' (Multiple n (StarGame s)) = if even n then Just zero else Just $ StarGame s
  simplifyOnce' (Multiple n (UpGame a)) = Just $ UpGame (n * a)
  simplifyOnce' (Multiple (-1) (Sum gs)) = Just $ Sum (map negate gs)
  simplifyOnce' (Multiple n (Canonical o a s u)) = Just $ Sum (map (Multiple n) (NumberGame o : UpGame a : StarGame s : u))
  simplifyOnce' (Multiple n g) =
    let h = simplifyOnce' g
     in if isNothing h
          then
            if n < 0
              then Just $ Multiple (-n) (negate g)
              else Just $ Sum (replicate (fromInteger n) g)
          else Just $ Multiple n $ fromJust h
  simplifyOnce' (Canonical o 0 0 []) = Just $ NumberGame o
  simplifyOnce' (Canonical 0 a 0 []) = Just $ UpGame a
  simplifyOnce' (Canonical 0 0 s []) = Just $ StarGame s
  simplifyOnce' (Canonical o a s [])
    | s < 0 = Just $ Canonical o a (-s) []
    | otherwise = Nothing
  simplifyOnce' (Canonical o a s u) = accumulateCanonical (Canonical o a s []) u False
  simplifyOnce' (CustomGame g) = simplifyOnce' g

-- | call getBounds' with memoization
getBoundsM = memo getBounds'

-- | calculate some bounds of an abstract game, the fit will not be perfect, but all bounds cover the range of values a game has
getBounds' :: Game -> GameBounds
getBounds' (Position [] []) = zeroBounds
getBounds' (NumberGame 0) = zeroBounds
getBounds' (StarGame 0) = zeroBounds
getBounds' (UpGame 0) = zeroBounds
getBounds' (UpGame a) = infinitesimalBounds
getBounds' (Sum []) = zeroBounds
getBounds' (Multiple 0 _) = zeroBounds
getBounds' (Canonical 0 0 0 []) = zeroBounds
getBounds' g = if isImpartial g then infinitesimalBounds else getBounds'' g
  where
    getBounds'' (Position l r) =
      let leftBounds = unionBounds (map getBounds l)
          rightBounds = unionBounds (map getBounds r)
       in normalizeBounds $ GameBounds (upperBound leftBounds + 1) (lowerBound rightBounds - 1) (isFuzzy leftBounds || isFuzzy rightBounds)
    getBounds'' (NumberGame r) = GameBounds (Finite r) (Finite r) False
    getBounds'' (Sum gs) = sum (map getBounds gs)
    getBounds'' (Multiple n g) = multiplyBounds n (getBounds g)
    getBounds'' (Canonical o a s u) = getBounds (Sum [NumberGame o, UpGame a, StarGame s, Sum u])

-- | apply a function (usually options LeftPlayer or options RightPlayer) to a game in a sum at a given index
moveInSum :: (Game -> [Game]) -> [Game] -> Int -> [Game]
moveInSum f gs i = map (\x -> Sum (take i gs ++ [x] ++ drop (i + 1) gs)) (f (gs !! i))

instance HasTrie Game where
  data Game :->: x = GameTrie (([Game], [Game]) :->: x) (Rational :->: x) (Integer :->: x) (Integer :->: x) ([Game] :->: x) ((Integer, Game) :->: x) ((Rational, Integer, Integer, [Game]) :->: x) (CustomGame :->: x)
  trie :: (Game -> b) -> Game :->: b
  trie f = GameTrie (trie (f . uncurry Position)) (trie (f . NumberGame)) (trie (f . StarGame)) (trie (f . UpGame)) (trie (f . Sum)) (trie (f . uncurry Multiple)) (trie (f . uncurry4 Canonical)) (trie (f . CustomGame))

  untrie :: (Game :->: b) -> Game -> b
  untrie (GameTrie d _ _ _ _ _ _ _) (Position l r) = untrie d (l, r)
  untrie (GameTrie _ d _ _ _ _ _ _) (NumberGame r) = untrie d r
  untrie (GameTrie _ _ d _ _ _ _ _) (StarGame s) = untrie d s
  untrie (GameTrie _ _ _ d _ _ _ _) (UpGame a) = untrie d a
  untrie (GameTrie _ _ _ _ d _ _ _) (Sum gs) = untrie d gs
  untrie (GameTrie _ _ _ _ _ d _ _) (Multiple n g) = untrie d (n, g)
  untrie (GameTrie _ _ _ _ _ _ d _) (Canonical o a s u) = untrie d (o, a, s, u)
  untrie (GameTrie _ _ _ _ _ _ _ d) (CustomGame g) = untrie d g

  enumerate :: Game :->: b -> [(Game, b)]
  enumerate (GameTrie d1 d2 d3 d4 d5 d6 d7 d8) =
    [(Position l r, x1) | ((l, r), x1) <- enumerate d1]
      ++ [(NumberGame r, x2) | (r, x2) <- enumerate d2]
      ++ [(StarGame s, x3) | (s, x3) <- enumerate d3]
      ++ [(UpGame a, x4) | (a, x4) <- enumerate d4]
      ++ [(Sum gs, x5) | (gs, x5) <- enumerate d5]
      ++ [(Multiple n g, x6) | ((n, g), x6) <- enumerate d6]
      ++ [(Canonical o a s u, x7) | ((o, a, s, u), x7) <- enumerate d7]
      ++ [(CustomGame g, x8) | (g, x8) <- enumerate d8]

-- | shorthand for writing games with integer value
g :: Integer -> Game
g = NumberGame . fromInteger

-- | shorthand for writing games with rational value
(/:) :: Integer -> Integer -> Game
(/:) n d = NumberGame (n % d)

-- | shorthand for writing games with nimber value
s :: Integer -> Game
s = StarGame

-- | shorthand for writing positions with each player having only one option - a number
(|:) :: Rational -> Rational -> Game
(|:) a b = Position [NumberGame a] [NumberGame b]

-- | shorthand for writing positions with each player having only one option
(.|.) :: Game -> Game -> Game
(.|.) g h = Position [g] [h]

-- | convert a game to a position
toPosition :: (GameClass g) => g -> Game
toPosition g = Position (options LeftPlayer g) (options RightPlayer g)

-- | is given game a number? assumes the input is simplified
isNumberGame :: Game -> Bool
isNumberGame (NumberGame _) = True
isNumberGame _ = False

-- | is given game a nimber? assumes the input is simplified
isStarGame :: Game -> Bool
isStarGame (NumberGame 0) = True
isStarGame (StarGame _) = True
isStarGame _ = False

-- | is given game cold and somewhat simplified?
isSimpleColdGame :: Game -> Bool
isSimpleColdGame (NumberGame _) = True
isSimpleColdGame (StarGame _) = True
isSimpleColdGame (UpGame _) = True
isSimpleColdGame (Canonical _ _ _ []) = True
isSimpleColdGame _ = False

-- | is given game a sum of numbers and nimbers? assumes the input is simplified
onlyNumbersAndStars :: Game -> Bool
onlyNumbersAndStars (NumberGame _) = True
onlyNumbersAndStars (StarGame _) = True
onlyNumbersAndStars (Canonical _ 0 _ []) = True
onlyNumbersAndStars _ = False

-- | get the nimber value of a nimber game, assumes the input is simplified
getStarDegree :: Game -> Integer
getStarDegree (NumberGame 0) = 0
getStarDegree (StarGame s) = s

-- | get the sign (-1, 0 or 1) of the up/down component of a game, assumes the input is simplified
getUpSign :: Game -> Integer
getUpSign (NumberGame _) = 0
getUpSign (StarGame _) = 0
getUpSign (UpGame 0) = 0
getUpSign (UpGame a) = if a > 0 then 1 else -1
getUpSign (Canonical _ a _ []) = getUpSign (UpGame a)

instance Num Game where
  (+) g h = gamify $ Sum [g, h]
  fromInteger = gamify . NumberGame . fromInteger
  negate = gamify . Multiple (-1)
  (*) = undefined
  abs = undefined
  signum = undefined

instance GameOrd Game where
  -- \| compare two games, first checking their bounds
  -- \| if they overlap, then compare their difference with zero
  gameCompare g h
    | g == h = EqualTo
    | g == zero = negateGameOrdering $ compareToZero h
    | h == zero = compareToZero g
    | otherwise =
        let gb = getBounds g
            hb = getBounds h
            c = gameCompare gb hb
         in if c /= ConfusedWith && c /= EqualTo
              then c
              else compareToZero (g - h)

-- | compare a game with zero
compareToZero :: Game -> GameOrdering
compareToZero = traceCall1Arg "compareToZero" (memoIfAble compareToZero' compareToZeroM)

compareToZeroM = memo compareToZero'

compareToZero' :: Game -> GameOrdering
compareToZero' (Position [] []) = EqualTo
compareToZero' (NumberGame r) = toGameOrdering $ compare r 0
compareToZero' (StarGame 0) = EqualTo
compareToZero' (StarGame s) = ConfusedWith
compareToZero' (Sum []) = EqualTo
compareToZero' (Sum [g]) = compareToZero g
compareToZero' (Sum gs) =
  let s = simplifyOnce (Sum gs) in maybe (compareToZeroByPlay (Sum gs)) compareToZero s
compareToZero' (Multiple 0 _) = EqualTo
compareToZero' (Multiple n g) = let r = compareToZero g in if n > 0 then r else negateGameOrdering r
compareToZero' (Canonical 0 0 s []) = compareToZero (StarGame s)
compareToZero' (Canonical 0 a 0 []) = compareToZero (UpGame a)
compareToZero' (Canonical 0 1 1 []) = ConfusedWith
compareToZero' (Canonical 0 (-1) 1 []) = ConfusedWith
compareToZero' (Canonical 0 a s []) = compareToZero (UpGame a)
compareToZero' (Canonical o 0 s []) = compareToZero (NumberGame o)
compareToZero' (Canonical o a s u) =
  let bc = gameCompare (getBounds (Canonical o a s u)) (getBounds zero)
   in if bc /= ConfusedWith && bc /= EqualTo
        then bc
        else
          let sg = simplifyOnce (Canonical o a s u)
           in maybe (compareToZeroByPlay (Canonical o a s u)) compareToZero sg
compareToZero' g = compareToZeroByPlay g

-- | can given player win given game?
canWin :: Player -> Game -> Bool
canWin p g = isJust $ winningMove p g

-- | find the first move that leads to a win for given player, if any
winningMove :: Player -> Game -> Maybe Game
winningMove LeftPlayer g = find ((\c -> c == GreaterThan || c == EqualTo) . compareToZero) (options LeftPlayer g)
winningMove RightPlayer g = find ((\c -> c == LessThan || c == EqualTo) . compareToZero) (options RightPlayer g)

-- | compare a game with zero by playing it out
-- | used when it can't be decided at a first glance
compareToZeroByPlay :: Game -> GameOrdering
compareToZeroByPlay g =
  let leftWin = canWin LeftPlayer g
      rightWin = canWin RightPlayer g
   in case (leftWin, rightWin) of
        (True, True) -> ConfusedWith
        (True, False) -> GreaterThan
        (False, True) -> LessThan
        (False, False) -> EqualTo

-- | do one step of simplification or return nothing when no simplifications were found
simplifyOnce :: Game -> Maybe Game
simplifyOnce = traceCall1Arg "simplifyOnce" (memoIfAble simplifyOnce' simplifyOnceM)

simplifyOnceM = memo simplifyOnce'

-- | try do one step of simplification of any game in the lists or return nothing when no simplifications were found
trySimplifyAny :: [Game] -> [Game] -> Maybe ([Game], [Game])
trySimplifyAny [] [] = Nothing
trySimplifyAny l r =
  let sl = trySimplifyAny' l []
      sr = trySimplifyAny' r []
   in if isNothing sl
        then
          if isNothing sr
            then Nothing
            else Just (l, fromJust sr)
        else
          if isNothing sr
            then Just (fromJust sl, r)
            else Just (fromJust sl, fromJust sr)

-- | try do one step of simplification of any game in the list or return nothing when no simplifications were found
trySimplifyAny' :: [Game] -> [Game] -> Maybe [Game]
trySimplifyAny' [] _ = Nothing
trySimplifyAny' (g : gs) a =
  let s = trySimplify g
   in if isNothing s
        then trySimplifyAny' gs (a ++ [g])
        else Just (fromJust s : gs ++ a)

-- | add to sum games in a list together into one game in canocical form
-- | the games which can't be added together are left in the unsimplified list of the canonical form
accumulateCanonical :: Game -> [Game] -> Bool -> Maybe Game
accumulateCanonical c [] False = Nothing
accumulateCanonical c [] True = Just c
accumulateCanonical (Canonical o a s u) (q : qs) ch =
  let sq = trySimplify q
   in if isNothing sq
        then
          let nc = addCanonical (Canonical o a s u) q
           in if isNothing nc
                then accumulateCanonical (Canonical o a s (u ++ [q])) qs ch
                else accumulateCanonical (fromJust nc) qs True
        else accumulateCanonical (Canonical o a s u) (fromJust sq : qs) ch

-- | add two games together if able, the first argument must be in canonical form, the second argumnent must be simplified
addCanonical :: Game -> Game -> Maybe Game
addCanonical = traceCall2Arg "addCanonical" addCanonical'

addCanonical' :: Game -> Game -> Maybe Game
addCanonical' (Canonical o a s u) (NumberGame r) = Just $ Canonical (o + r) a s u
addCanonical' (Canonical o a s u) (StarGame t) = Just $ Canonical o a (abs s `xor` abs t) u
addCanonical' (Canonical o a s u) (UpGame b) = Just $ Canonical o (a + b) s u
addCanonical' (Canonical o a s u) (Canonical p b t v) = Just $ Canonical (o + p) (a + b) (abs s `xor` abs t) (u ++ v)
addCanonical' (Canonical o a s u) h = Nothing

-- | get the simplest form of a game
simplify :: Game -> Game
simplify g = let h = trySimplify g in fromMaybe g h

-- | get the simplest form of a game or return Nothing if it already is in simplest form
trySimplify :: Game -> Maybe Game
trySimplify = traceCall1Arg "simplify" (memoIfAble trySimplify' trySimplifyM)

trySimplifyM = memo trySimplify'

trySimplify' :: Game -> Maybe Game
trySimplify' g =
  let h = simplifyOnce g
   in if isNothing h
        then Nothing
        else
          let i = trySimplify (fromJust h)
           in if isNothing i then h else i

-- | remove duplicates in the options of a position or nothing if no duplicates were found
removeDuplicates :: Game -> Maybe Game
removeDuplicates = traceCall1Arg "removeDuplicates" removeDuplicates'

removeDuplicates' :: Game -> Maybe Game
removeDuplicates' (Position l r) =
  let nl = nub l
      nr = nub r
   in if length l == length nl && length r == length nr
        then Nothing
        else Just $ Position nl nr

-- | remove games from the options of a position which are dominated by other options or nothing if no simplifications were found, assumes the options are simplified
excludeExtremes :: Game -> Maybe Game
excludeExtremes = traceCall1Arg "excludeExtremes" excludeExtremes'

excludeExtremes' :: Game -> Maybe Game
excludeExtremes' (Position l r) =
  let el = map (\x -> any (x <:) l) l
      er = map (\x -> any (x >:) r) r
   in if all (== False) el && all (== False) er
        then Nothing
        else Just $ Position (removeWhere l el) (removeWhere r er)

-- | try to convert a position into a simpler abstract game, assumes the options are simplified
compactifyPosition :: Game -> Maybe Game
compactifyPosition = traceCall1Arg "compactifyPosition" (memoIfAble compactifyPosition' compactifyPositionM)

compactifyPositionM = memo compactifyPosition'

compactifyPosition' :: Game -> Maybe Game
compactifyPosition' (Position [] []) = Just zero
compactifyPosition' (Position [NumberGame o] []) = if o < 0 then Just zero else Just $ NumberGame (o + 1)
compactifyPosition' (Position [] [NumberGame o]) = if o > 0 then Just zero else Just $ NumberGame (o - 1)
compactifyPosition' (Position [NumberGame o] [NumberGame p])
  | o < 0 && p > 0 = Just zero
  | o == p = Just $ Canonical o 0 1 []
  | o < p = Just $ NumberGame $ fitNumber o p
  | otherwise = Nothing -- HOT GAME!!!
compactifyPosition' (Position [UpGame a] []) = if a >= 0 then Just (g 1) else Just zero
compactifyPosition' (Position [] [UpGame a]) = if a <= 0 then Just (g (-1)) else Just zero
compactifyPosition' (Position [NumberGame 0] [UpGame a]) = Just $ Canonical 0 (a + 1) 1 []
compactifyPosition' (Position [UpGame a] [NumberGame 0]) = Just $ Canonical 0 (a - 1) 1 []
compactifyPosition' (Position [UpGame a] [UpGame b])
  | a == 1 && b == -1 = Just star
  | a > 0 && b > 0 = Just $ Canonical 0 (b + 1) 1 []
  | a < 0 && b < 0 = Just $ Canonical 0 (a - 1) 1 []
  | a < 0 && b > 0 = Just zero
  | otherwise = Nothing -- HOT GAME!!!
compactifyPosition' (Position l r)
  | length l == 1 && length r == 1 && head l == Canonical 0 1 1 [] && head r == Canonical 0 1 1 [] = Just zero
  | length l == 1 && length r == 1 && head l == Canonical 0 (-1) 1 [] && head r == Canonical 0 1 1 [] = Just zero
  | length l == 1 && length r == 1 && head l == Canonical 0 1 1 [] && head r == Canonical 0 (-1) 1 [] = Just zero
  | length l == 1 && length r == 1 && head l == Canonical 0 (-1) 1 [] && head r == Canonical 0 (-1) 1 [] = Just zero
  | length l == 1 && length r == 1 && head l == head r && isSimpleColdGame (head l) && isSimpleColdGame (head r) = Just $ head l + star + UpGame (getUpSign (head l))
  | all isStarGame l && all isStarGame r = simplifyStars (map getStarDegree l) (map getStarDegree r)
  | all onlyNumbersAndStars l && all onlyNumbersAndStars r =
      let lb = unionBounds $ map getBounds l
          rb = unionBounds $ map getBounds r
       in if lb <: rb
            then
              if isFinite (upperBound lb) && isFinite (lowerBound rb)
                then Just $ NumberGame $ fitNumber (finiteToRational $ upperBound lb) (finiteToRational $ lowerBound rb)
                else Just zero
            else Nothing
  | otherwise = Nothing
compactifyPosition' g = Nothing

-- | find the simplest rational strictly between two rationals
fitNumber :: Rational -> Rational -> Rational
fitNumber o p =
  if p - o > 1
    then fitNumber' 1 (truncate o) (truncate p)
    else
      let cd = max (denominator o) (denominator p)
          no = numerator (o * fromInteger cd)
          np = numerator (p * fromInteger cd)
       in fitNumber' cd no np
  where
    fitNumber' cd no np
      | np - no == 1 = (no + np) % (2 * cd)
      | cd == 1 && no < 0 && np > 0 = 0
      | cd == 1 && np <= 0 = fromInteger np - 1
      | cd == 1 && no >= 0 = fromInteger no + 1
      | otherwise =
          let dd = largestPowerOfTwoIn (np - no - 1)
              fd = cd `div` dd
              fo = no `div` dd
           in (fo + 1) % fd

-- | find the largest power of two in a positive integer
largestPowerOfTwoIn :: Integer -> Integer
largestPowerOfTwoIn n = 1 `shiftL` fromIntegral (integerLog2 n)

-- | try to simplify a position whose options are only nimber games
-- | the inputs are in the form of a list of integers, each representing the degree of a star game
simplifyStars :: [Integer] -> [Integer] -> Maybe Game
simplifyStars l r =
  let sl = map head $ group $ sort l
      sr = map head $ group $ sort r
      cl = length [i | (e, i) <- zip sl [0 ..], e == i]
      cr = length [i | (e, i) <- zip sr [0 ..], e == i]
      nl = drop cl sl
      nr = drop cr sr
   in simplifyStars' l r sl sr cl cr nl nr
  where
    simplifyStars' l r sl sr cl cr nl nr
      | sl == [0] && sr == [1] = Just $ UpGame 1
      | sl == [1] && sr == [0] = Just $ UpGame (-1)
      | sl == [0] && sr == [0, 1] = Just (UpGame (-1) + star)
      | sl == [0, 1] && sr == [0] = Just (UpGame 1 + star)
      | cl == cr = Just $ StarGame $ toInteger cl
      | abs (cl - cr) > 1 = Just $ Position (map StarGame (take (cr + 1) [0 .. toInteger cl - 1] ++ nl)) (map StarGame (take (cl + 1) [0 .. toInteger cr - 1] ++ nr))
      | length sl /= length l || length sr /= length r = Just $ Position (map StarGame sl) (map StarGame sr)
      | otherwise = Nothing

-- | find if a game has the value zero by playing it out, then return a zero game or nothing accordingly
tryZeroOut :: Game -> Maybe Game
tryZeroOut g = if compareToZeroByPlay g == EqualTo then Just zero else Nothing

-------------------- GAME CLASS --------------------

class (Show g, HasTrie g, Eq g) => GameClass g where
  -- | the options each player has in this position
  -- | try to order them from the ones that end the game fastest
  options :: Player -> g -> [Game]

  -- | box a custom game instance into a the game type
  gamify :: g -> Game

  -- | is this game impartial? - i.e. can both players make the same moves?
  isImpartial :: g -> Bool

  -- | get the bounds this game's value fits into
  -- | the default implementation works, but if you can provide a tighter bound without complex computation, do so
  getBounds :: g -> GameBounds
  getBounds g = if isImpartial g then infinitesimalBounds else unlimitedBounds

  -- | try to simplify the game once, if it cannot be simplified, return Nothing
  -- | use simplifyOnce to simplify and memoize results
  -- | when no simplification is found, convert the game into a position, so it can be simplified from there
  simplifyOnce' :: g -> Maybe Game
  simplifyOnce' g = Just $ toPosition g

-------------------- CUSTOM GAMES --------------------

-- | games that are not abstract - for their rules see readme.md
data CustomGame
  = ClassicNim Integer
  | UnlimitedNim Integer
  | FibonacciNim Integer
  | FactorsAndMultiples Integer [Integer]
  | TicTacToe [[PlayerSymbol]]
  | Cutcake Integer Integer
  | ToadsAndFrogs [PlayerSymbol]
  | LinearCol [(PlayerSymbol, Bool)]
  deriving (Eq)

instance Show CustomGame where
  show (ClassicNim n) = "Classic Nim " ++ show n
  show (UnlimitedNim n) = "Unlimited Nim " ++ show n
  show (FibonacciNim n) = "Fibonacci Nim " ++ show n
  show (FactorsAndMultiples n m) = "Factors and Multiples of " ++ show n ++ " in " ++ show m
  show (TicTacToe b) = "\n+---+\n" ++ intercalate "\n" (map (\s -> "|" ++ concatMap showTicTacToeSymbol s ++ "|") b) ++ "\n+---+\n"
  show (Cutcake n m) = "Cutcake " ++ show n ++ "x" ++ show m
  show (ToadsAndFrogs b) = "[" ++ unwords (map showToadsAndFrogsSymbol b) ++ "]"
  show (LinearCol b) = "[" ++ concatMap showLinearColSymbol b ++ "]"

instance GameClass CustomGame where
  options _ (UnlimitedNim n) = map (gamify . UnlimitedNim) [0 .. n - 1]
  options _ (ClassicNim n) = [gamify $ ClassicNim m | m <- [n - 3 .. n - 1], m >= 0]
  options _ (FibonacciNim n) = reverse [gamify $ FibonacciNim (n - rm) | rm <- drop 2 $ takeWhile (<= n) fib]
  options _ (FactorsAndMultiples n m) = [gamify $ FactorsAndMultiples r (deleteAt i m) | (i, r) <- zip [0 ..] m, r `mod` n == 0 || n `mod` r == 0]
  options p (TicTacToe b) = map (gamify . TicTacToe) (catMaybes [ticTacToeMove p b x y | x <- [0 .. length b - 1], y <- [0 .. length (head b) - 1]])
  options LeftPlayer (Cutcake n m) = [Sum $ map (gamify . uncurry Cutcake) $ filter (/= (1, 1)) [(n - c, m), (c, m)] | c <- [1 .. n `div` 2]]
  options RightPlayer (Cutcake n m) = [Sum $ map (gamify . uncurry Cutcake) $ filter (/= (1, 1)) [(n, m - c), (n, c)] | c <- [1 .. m `div` 2]]
  options p (ToadsAndFrogs b) = map (gamify . ToadsAndFrogs) (catMaybes [toadsAndFrogsMove p b i | i <- [0 .. length b - 1]])
  options p (LinearCol b) = map (gamify . LinearCol) (catMaybes [linearColMove p b i | i <- [0 .. length b - 1]])
  isImpartial (UnlimitedNim n) = True
  isImpartial (ClassicNim n) = True
  isImpartial (FibonacciNim n) = True
  isImpartial (FactorsAndMultiples n m) = True
  isImpartial (TicTacToe b) = False
  isImpartial (Cutcake n m) = False
  isImpartial (ToadsAndFrogs b) = False
  isImpartial (LinearCol b) = False
  gamify = CustomGame

  -- \| ClassicNim, UnlimitedNim and Cutcake have simple formulas to determie their value, but they are not being simplified for testing purposes
  -- \| for TicTacToe, ToadsAndFrogs and LinearCol, some basic simplifications are implemented
  simplifyOnce' (TicTacToe b) =
    let s = ticTacToeSimplify b
     in if isNothing s
          then Just $ toPosition (TicTacToe b)
          else Just $ gamify $ TicTacToe $ fromJust s
  simplifyOnce' (ToadsAndFrogs b) =
    let s = toadsAndFrogsSimplify b
     in if isNothing s
          then Just $ toPosition (ToadsAndFrogs b)
          else s
  simplifyOnce' (LinearCol b) =
    let s = linearColSimplify b
     in if isNothing s
          then Just $ toPosition (LinearCol b)
          else s
  simplifyOnce' g = Just $ toPosition g

instance HasTrie CustomGame where
  data CustomGame :->: a = CustomGameTrie (Integer :->: a) (Integer :->: a) (Integer :->: a) ((Integer, [Integer]) :->: a) ([[PlayerSymbol]] :->: a) ((Integer, Integer) :->: a) ([PlayerSymbol] :->: a) ([(PlayerSymbol, Bool)] :->: a)
  trie f = CustomGameTrie (trie $ f . ClassicNim) (trie $ f . UnlimitedNim) (trie $ f . FibonacciNim) (trie $ f . uncurry FactorsAndMultiples) (trie $ f . TicTacToe) (trie $ f . uncurry Cutcake) (trie $ f . ToadsAndFrogs) (trie $ f . LinearCol)
  untrie (CustomGameTrie t _ _ _ _ _ _ _) (ClassicNim n) = untrie t n
  untrie (CustomGameTrie _ t _ _ _ _ _ _) (UnlimitedNim n) = untrie t n
  untrie (CustomGameTrie _ _ t _ _ _ _ _) (FibonacciNim n) = untrie t n
  untrie (CustomGameTrie _ _ _ t _ _ _ _) (FactorsAndMultiples n m) = untrie t (n, m)
  untrie (CustomGameTrie _ _ _ _ t _ _ _) (TicTacToe b) = untrie t b
  untrie (CustomGameTrie _ _ _ _ _ t _ _) (Cutcake n m) = untrie t (n, m)
  untrie (CustomGameTrie _ _ _ _ _ _ t _) (ToadsAndFrogs b) = untrie t b
  untrie (CustomGameTrie _ _ _ _ _ _ _ t) (LinearCol b) = untrie t b

  enumerate (CustomGameTrie d1 d2 d3 d4 d5 d6 d7 d8) =
    [(ClassicNim n, x1) | (n, x1) <- enumerate d1]
      ++ [(UnlimitedNim n, x2) | (n, x2) <- enumerate d2]
      ++ [(FibonacciNim n, x3) | (n, x3) <- enumerate d3]
      ++ [(FactorsAndMultiples n m, x4) | ((n, m), x4) <- enumerate d4]
      ++ [(TicTacToe b, x5) | (b, x5) <- enumerate d5]
      ++ [(Cutcake n m, x6) | ((n, m), x6) <- enumerate d6]
      ++ [(ToadsAndFrogs b, x7) | (b, x7) <- enumerate d7]
      ++ [(LinearCol b, x8) | (b, x8) <- enumerate d8]

-- | symbols fore each player and empty - used in TicTacToe, ToadsAndFrogs and LinearCol
data PlayerSymbol = L | E | R deriving (Eq, Show)

-- | gat the symbol for a player
playerToSymbol LeftPlayer = L
playerToSymbol RightPlayer = R

instance HasTrie PlayerSymbol where
  data PlayerSymbol :->: a = PlayerSymbolTrie (a, a, a)
  trie f = PlayerSymbolTrie (f L, f E, f R)
  untrie (PlayerSymbolTrie (l, _, _)) L = l
  untrie (PlayerSymbolTrie (_, e, _)) E = e
  untrie (PlayerSymbolTrie (_, _, r)) R = r
  enumerate (PlayerSymbolTrie (l, e, r)) = [(L, l), (E, e), (R, r)]

-------------------- TIC TAC TOE --------------------

-- | a blank tic tac toe board
blankTicTacToe = gamify $ TicTacToe $ map (const $ map (const E) [1 .. 3]) [1 .. 3]

-- | textual representation of a player symbol in the context of tic tac toe
showTicTacToeSymbol L = "X"
showTicTacToeSymbol R = "O"
showTicTacToeSymbol E = " "

-- | try to put a players symbol on the board at position x y
-- | returns the new board or nothing if the move is not valid
ticTacToeMove :: Player -> [[PlayerSymbol]] -> Int -> Int -> Maybe [[PlayerSymbol]]
ticTacToeMove p b x y
  | isTicTacToeFinished b = Nothing
  | b !! x !! y /= E = Nothing
  | otherwise = Just $ replace b x $ replace (b !! x) y (playerToSymbol p)

-- | check if the game is finished - are there three of the same symbol in a row?
isTicTacToeFinished :: [[PlayerSymbol]] -> Bool
isTicTacToeFinished b = checkDiagonals b || any (checkRow b) [0 .. 2] || any (checkRow (transpose b)) [0 .. 2]

checkDiagonals b = let t = b !! 1 !! 1 in t /= E && ((b !! 0 !! 0 == t && t == b !! 2 !! 2) || (b !! 0 !! 2 == t && t == b !! 2 !! 0))

checkRow b i = let t = head (b !! i) in t /= E && all (== t) (b !! i)

-- | simplify a tic tac toe board, returning a new board or nothing if no simplification was found
-- | only simplification is to rotate and flip the board so it has the most symbols in the top left corner, making use of symmetries
ticTacToeSimplify :: [[PlayerSymbol]] -> Maybe [[PlayerSymbol]]
ticTacToeSimplify b =
  let vi = imbalance b
      hi = imbalance $ transpose b
   in orient b vi hi
  where
    orient b vi hi
      | abs vi < abs hi = Just $ transpose b
      | vi < 0 = Just $ reverse b
      | hi < 0 = Just $ transpose $ reverse $ transpose b
      | otherwise = Nothing

-- | calculate the imbalance of a tic tac toe board along its vertical axis
-- | negative values mean the bottom row has more symbols than the top row
-- | crosses are heavier than noughts as a tie breaker
imbalance :: [[PlayerSymbol]] -> Int
imbalance b = sum (map value (head b)) - sum (map value (b !! 2))
  where
    value L = 11
    value R = 10
    value E = 0

-------------------- TOADS AND FROGS --------------------

-- | textual representation of a player symbol in the context of toads and frogs
showToadsAndFrogsSymbol L = ">"
showToadsAndFrogsSymbol R = "<"
showToadsAndFrogsSymbol E = "_"

-- | try to move a player symbol on the board at position i
-- | returns the new board or nothing if the move is not valid
toadsAndFrogsMove :: Player -> [PlayerSymbol] -> Int -> Maybe [PlayerSymbol]
toadsAndFrogsMove p b i
  | atIndex b i /= Just (playerToSymbol p) = Nothing
  | atIndex b (i + toadsAndFrogsMovement p) == Just E = Just $ replace (replace b (i + toadsAndFrogsMovement p) (playerToSymbol p)) i E
  | atIndex b (i + toadsAndFrogsMovement p) == Just (playerToSymbol $ opponent p) && atIndex b (i + 2 * toadsAndFrogsMovement p) == Just E = Just $ replace (replace b (i + 2 * toadsAndFrogsMovement p) (playerToSymbol p)) i E
  | otherwise = Nothing

-- | calculate the movement of a player's amphibians - toads move right, frogs move left
toadsAndFrogsMovement :: Player -> Int
toadsAndFrogsMovement LeftPlayer = 1
toadsAndFrogsMovement RightPlayer = -1

-- | simplify a toads and frogs board, returning a new game or nothing if no simplification was found
-- | the simplifications are explained below
toadsAndFrogsSimplify :: [PlayerSymbol] -> Maybe Game
toadsAndFrogsSimplify b = let t = toadsAndFrogsTrim b [] 0 in if isNothing t then toadsAndFrogsSplit b [] 0 else t

-- | a finite automaton to scan the board and split it in two when a ">><<" is found
-- | this is a blockade that no amphibian can pass and so the game can be split in a sum of two smaller games
toadsAndFrogsSplit :: [PlayerSymbol] -> [PlayerSymbol] -> Int -> Maybe Game
toadsAndFrogsSplit [] _ _ = Nothing
toadsAndFrogsSplit (L : bs) a 0 = toadsAndFrogsSplit bs (a ++ [L]) 1
toadsAndFrogsSplit (L : bs) a 1 = toadsAndFrogsSplit bs (a ++ [L]) 2
toadsAndFrogsSplit (L : bs) a 2 = toadsAndFrogsSplit bs (a ++ [L]) 2
toadsAndFrogsSplit (R : bs) a 2 = toadsAndFrogsSplit bs (a ++ [R]) 3
toadsAndFrogsSplit (R : bs) a 3 = Just $ gamify (ToadsAndFrogs $ take (length a - 3) a) + gamify (ToadsAndFrogs bs)
toadsAndFrogsSplit (e : bs) a _ = toadsAndFrogsSplit bs (a ++ [e]) 0

-- | scan the board and trim off toads stuck on the right end and frogs stuck on the left end
toadsAndFrogsTrim :: [PlayerSymbol] -> [PlayerSymbol] -> Int -> Maybe Game
toadsAndFrogsTrim [] a i
  | i <= 0 = Nothing
  | otherwise = Just $ gamify (ToadsAndFrogs $ take (length a - i) a)
toadsAndFrogsTrim (R : bs) [] _ = Just $ gamify (ToadsAndFrogs bs)
toadsAndFrogsTrim (L : bs) a i = toadsAndFrogsTrim bs (a ++ [L]) (i + 1)
toadsAndFrogsTrim (e : bs) a i = toadsAndFrogsTrim bs (a ++ [e]) 0

-- | create a toads and frogs game from a string
toadsAndFrogsFromString :: [Char] -> Game
toadsAndFrogsFromString s = gamify $ ToadsAndFrogs $ map toadsAndFrogsFromChar s
  where
    toadsAndFrogsFromChar '>' = L
    toadsAndFrogsFromChar '<' = R
    toadsAndFrogsFromChar '_' = E

-------------------- LINEAR COL --------------------

-- | textual representation of a player symbol in the context of linear col
showLinearColSymbol (L, True) = "L"
showLinearColSymbol (R, True) = "R"
showLinearColSymbol (E, True) = "x"
showLinearColSymbol (L, False) = "l"
showLinearColSymbol (R, False) = "r"
showLinearColSymbol (E, False) = "-"

-- | try to color in a player symbol on the board at position i
-- | returns the new board or nothing if the move is not valid
linearColMove :: Player -> [(PlayerSymbol, Bool)] -> Int -> Maybe [(PlayerSymbol, Bool)]
linearColMove p b i =
  let s = b !! i
   in if not (snd s) && fst s /= playerToSymbol (opponent p)
        then Just $ linearColTintAt (replace (linearColTintAt b (opponent p) (i - 1)) i (playerToSymbol p, True)) (opponent p) (i + 1)
        else Nothing

-- | tint a symbol on the board at position i, if possible, returning the new board
linearColTintAt :: [(PlayerSymbol, Bool)] -> Player -> Int -> [(PlayerSymbol, Bool)]
linearColTintAt b p i =
  let s = atIndex b i
   in if isNothing s || snd (fromJust s)
        then b
        else
          let s' = fst (fromJust s)
           in replace b i (linearColTint s' p)

-- | get the new sybol after tinting it with the given player's color
linearColTint :: PlayerSymbol -> Player -> (PlayerSymbol, Bool)
linearColTint s p =
  if s == playerToSymbol (opponent p)
    then (E, True)
    else (playerToSymbol p, False)

-- | simplify a linear col board, returning a new game or nothing if no simplification was found
-- | the board can be split in two when on an uncolorable symbol (L, R or x) - the left side does not affect the right side anymore
-- | when a sequence of 7 dashes (-) is found, the middle dash can be removed, splitting the game in two without affecting its value
linearColSimplify :: [(PlayerSymbol, Bool)] -> Maybe Game
linearColSimplify b = linearColSimplify' b [] 0
  where
    linearColSimplify' [] _ _ = Nothing
    linearColSimplify' ((_, True) : bs) a _ = Just $ gamify (LinearCol a) + gamify (LinearCol bs)
    linearColSimplify' ((E, False) : bs) a 6 = Just $ gamify (LinearCol $ take (length a - 3) a) + gamify (LinearCol $ [(E, False), (E, False), (E, False)] ++ bs)
    linearColSimplify' ((E, False) : bs) a i = linearColSimplify' bs (a ++ [(E, False)]) (i + 1)
    linearColSimplify' (b : bs) a i = linearColSimplify' bs (a ++ [b]) 0

-- | create a linear col game from a string
linearColFromString :: [Char] -> Game
linearColFromString s = gamify $ LinearCol $ map linearColFromChar s
  where
    linearColFromChar 'L' = (L, True)
    linearColFromChar 'R' = (R, True)
    linearColFromChar 'x' = (E, True)
    linearColFromChar 'l' = (L, False)
    linearColFromChar 'r' = (R, False)
    linearColFromChar '-' = (E, False)