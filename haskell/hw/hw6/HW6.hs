-- 6차 숙제: 위치 기수법 (Positional Notation)

module HW6 where

---------------------------------------------------------------------
--    숫자 (DIGITS)

-- data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
-- data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
--            | Ten | Eleven | Twelve | Thirteen | Fourteen | Fifteen
    deriving (Show, Eq, Ord, Bounded, Enum)

digitCount  =  length [(minBound::Digit)..maxBound]

digitCharCandids  =  ['0'..'9'] ++ ['A'..'Z']

digitChars  =  take digitCount digitCharCandids


-- 문제
nextD :: Digit -> Digit
nextD x 
    | x == maxBound = minBound
    | otherwise     = succ x 

-- 문제
prevD :: Digit -> Digit
prevD x 
    | x == minBound = maxBound
    | otherwise     = pred x

---------------------------------------------------------------------
--    캐리 (CARRY)

-- True면 캐리가 발생한 것, False면 캐리가 없는 것
type Carry = Bool


-- 문제
addDd :: Carry -> Digit -> Digit -> Digit
addDd b x y     
    | b              = addDd False (nextD x) y
    | x == minBound  = y 
    | y == minBound  = x 
    | x > y          = addDd False (nextD x) (prevD y) 
    | y >= x         = addDd False (prevD x) (nextD y)
    
-- 문제
addDc :: Carry -> Digit -> Digit -> Carry
addDc b x y 
    | x == minBound  = b && (y == maxBound)
    | y == minBound  = b && (x == maxBound)
    | x == maxBound  = True 
    | y == maxBound  = True 
    | x > y          = addDc b (nextD x) (prevD y) 
    | y >= x         = addDc b (prevD x) (nextD y)
 
 
    


-- For Testing
testAdd tc  =  mapM_ putStrLn [ f cr d1 d2 | (cr,d1,d2) <- tc ] where
    f c x y  =  g c ++ showD x ++ " + " ++ showD y ++ " = " ++
                h (addDc c x y) ++ showD (addDd c x y)
    g c      =  if c then showD (succ minBound) ++ " + " else "    "
    h c      =  if c then showD (succ minBound) else " "
    showD :: Digit -> String    -- 없으면 타입 자동 추론 불가
    showD d  =  [digitChars !! fromEnum d]

tc1  =  [ (c,x,y) | c <- [False, True],
                    x <- [Zero, Three, Six, Nine],
                    y <- [Zero, Three, Six, Nine]]

-- tc2  =  [ (c,x,y) | c <- [False, True],
--                     x <- [Zero, Five, Ten, Fifteen],
--                     y <- [Zero, Five, Ten, Fifteen]]

-- tc3  =  [ (c,x,y) | c <- [False, True],
--                     x <- [Zero, One],
--                     y <- [Zero, One]]



---------------------------------------------------------------------
--    자연수 (NATURAL NUMBERS)

-- 자연수란 "숫자들의 리스트"의 별칭일 뿐
-- 리스트 속의 숫자 순서는 일상적인 위치 기수법 순서 대로 나열
type Nat = [Digit]

showN :: Nat -> String
showN   []    =  ""
showN (d:ds)  =  digitChars !! fromEnum d : showN ds

readN :: String -> Nat
readN   ""    =  []
readN (c:cs)  =  toEnum (fstIdx c digitChars) : readN cs where
    fstIdx x (y:ys)
        | x == y     =  0
        | otherwise  =  1 + fstIdx x ys
    fstIdx _   _     =  error "Cannot be parsed as Natural number"


normalize :: Nat -> Nat
normalize (d:ds)
    | d == minBound  =  normalize ds
    | otherwise      =  d:ds
normalize   []       =  [minBound]


incN :: Nat -> Nat
incN  =  reverse . fn . reverse  where
    fn (d:ds)
        | d == maxBound  =  minBound : fn ds
        | otherwise      =  succ d : ds
    fn   []              =  [succ minBound]

decN :: Nat -> Nat
decN  =  reverse . fn . reverse  where
    fn (d:ds)
        | d == minBound  =  maxBound : fn ds
        | otherwise      =  pred d : ds
    fn   []              =  error "Cannot decrease more"


isNull, isMono :: Nat -> Bool
isNull n  =  all (minBound ==) n

-- isMono n  =  f (reverse n)  where
--     f   []    =  False
--     f (d:ds)  =  d == succ minBound && isNull ds

-- isMono n  =  not (isNull n) && isNull (init n) && last n == succ minBound

isMono n  =  normalize n == [succ minBound]

leN :: Nat -> Nat -> Bool
leN n m  =  fn (normalize n) (normalize m)  where
    fn x y  =  length x < length y || (length x == length y && x <= y)


addNslow :: Nat -> Nat -> Nat
addNslow x y  =  if leN x y then add x y else add y x  where
    add a b
        | isNull a   =  b
        | otherwise  =  add (decN a) (incN b)


-- 문제
addNfast :: Nat -> Nat -> Nat
addNfast x y = if leN x y then add False (reverse y) (reverse x) [] else add False (reverse x) (reverse y) [] where
    -- 꼬리재귀 형태로 사용하기 위해 뒤에 result 리스트를 만들었는데 사실 진수형 계산이라 그렇게 효용은 없을것 -> 오히려 리스트로 자연스럽게 정의하기 어렵다.
    add b (c:cs) (d:ds) result
        | isNull ds = (if (addDc b c d) then incN (reverse cs) else reverse cs) ++ ((addDd b c d):result) --앞에 if 괄호 쳐줘야 함.
        -- 자리수에 따른 복잡도를 낮은수에 맞추기위해 if then else 를 사용했다. 
        -- 조건을 추가해 높은수에 맞추면 아마 복잡도가 자리수차이만큼 올라가고 식이 간단해질것
        | otherwise = add (addDc b c d) cs ds ((addDd b c d):result) 
addNfastNT :: Nat -> Nat -> Nat
addNfastNT x y = if leN x y then reverse (add False (reverse y) (reverse x)) else reverse (add False (reverse x) (reverse y)) where
    -- 꼬리재귀 없는 함수
    add b (c:cs) (d:ds)
        | isNull ds = (addDd b c d) : if (addDc b c d) then reverse (incN (reverse cs)) else cs 
        -- 자리수에 따른 복잡도를 낮은수에 맞추기위해 if then else 를 사용했다. 
        -- 조건을 추가해 높은수에 맞추면 자리수차이만큼 더 하고 식이 간단해질것
        -- reverse (incN (reverse cs))를 좀더 이쁘게 할수 있을것 같은데...
        | otherwise = (addDd b c d) : (add (addDc b c d) cs ds) 


-- 문제 (작업)
addN :: Nat -> Nat -> Nat
addN  =  addNfast      -- addNslow 혹은 addNfast


mulNslow :: Nat -> Nat -> Nat
mulNslow x y  =  if leN x y then mul x y [minBound] else mul y x [minBound]  where
    mul a b acc
        | isNull a   =  acc
        | otherwise  =  mul (decN a) b (addN b acc)


-- 문제
mulNfast :: Nat -> Nat -> Nat
mulNfast x y =   if leN x y then mul x y else mul y x  where
    mul a b
        | isNull a  = [minBound] 
        | otherwise = addN (mulNslow [last a] b) ((mul (init a) b)++[minBound])



mulN :: Nat -> Nat -> Nat
mulN  =  mulNslow      -- mulNslow 혹은 mulNfast

facN :: Nat -> Nat
facN n  =  f n [succ minBound]  where
    f x acc
        | isNull x   =  acc
        | otherwise  =  f (decN x) (mulN x acc)