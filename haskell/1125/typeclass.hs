module TCex where

    data Rat = Rat Integer Integer --Rat을 만드는 방법은 하나뿐. 정수두개를 받은 순서쌍 -> 유리수를 만드는 
    --첫번째가 분자, 두번째가 분모로 정의할것
    
    --mkRat을 쓰도록 권장. 
    --0으로 나누면 안됨. 그외에 Rat 자료연산자를 사용해 값을 만들어줌.
    --분모는 절대 양수도록 하고 약수를 만듬
    -- :i signim 해당 타입에 맞는 부호가 나옴 (-1 0 1) 
    -- div _ g 분모 분자를 최대공약수로 나눠줌
    mkRat :: Integer -> Integer -> Rat
    mkRat _ 0  =  error "Zero denominator!"
    mkRat n d  =  Rat (signum (n * d) * div an g) (div ad g)  where
        an = abs n          --절대값
        ad = abs d          
        g = gcd an ad       --gcd 내장함수 최대공약수
    
    --외우세요
    --Rat 타입은 Show 클래스에서 사용가능하다는걸 증명
    -- :i Show 찍어보기 {-# MINIMAL showsPrec | show #-} - 둘중 하나만 하면 됨 이중 show 만 알면 됨
    -- show :: a -> String
    -- show 는 a 를 받아 스트링으로 내보낼수 있으면 된다는 소리
    --오른쪽 show 는 정수 n을 show한 것, 왼쪽 show는 Rat을 이용한 정의
    instance Show Rat where
        show (Rat n d)  =  show n ++ "/" ++ show d
    
    -- :i eq == 또는 != 만들어진거
    -- 오른쪽에서의 등호는 두 유리수가 같다는 말
    -- 등호만 선언하면 eq 클래스에서 알아서 만들어 줌
    instance Eq Rat where
        Rat xn xd == Rat yn yd  =  xn * yd == yn * xd

    -- Num 클래스에 대해 증명
    instance Num Rat where
        Rat xn xd + Rat yn yd  =  mkRat (xn * yd + xd * yn) (xd * yd)
        Rat xn xd - Rat yn yd  =  mkRat (xn * yd - xd * yn) (xd * yd)
        Rat xn xd * Rat yn yd  =  mkRat (xn * yn) (xd * yd)
        abs (Rat n d)          =  mkRat (abs n) (abs d)
        signum (Rat n d)       =  mkRat (signum n) (signum d)               -- signum 1/1 0/1 -1/1 셋 중에 하나가 나올것
        fromInteger i          =  Rat i 1                                  -- 정수가 주어지면 그 정수를 우리가 원하는 Rat 으로 만들어져야 한다는 망
    
    
    
    euclidean x y
        | rem y x == 0  =  x
        | otherwise     =  euclidean (rem y x) x
    
    
    
    --본론임
    --하드웨어 정수에서 지원하지않는 범위를 소프트웨어로 정의하기
    --digit은 10개의 종료조건을 가짐
    --앞으로 만든 함수들은 이 digit을 이어붙여서 만듬.
    --그리고 Digit의 범위와 상관없이 만들어지는 함수
    data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
        deriving (Show, Bounded, Enum, Eq) --순서를 봐서 알아서 추정 해줌. Zero가 minbound Nine이 maxBound
    
    -- Digit들의 리스트를 Nat으로 정의함.
    type Nat = [Digit]
    
    --이거보지마
    normalize' :: Nat -> Nat
    normalize' []         =  [Zero]
    normalize' (Zero:ds)  =  normalize' ds
    normalize' n          =  n
    
    --같은 자연수를 여러 방식으로 만들수 있게 됨. 같은값인데 다른패턴인 경우 ex) [zero,zero,two] == [two]
    --이런경우 표준형으로 만들어 줄수 있게끔 노말라이즈 
    normalize :: Nat -> Nat
    normalize []  =  [minBound] -- 빈 리스트? zero로 만들자. minBound는 Nat이 선언 되어있으니까 Digit의 것으로 추론됨 
    normalize (d:ds)
        | d == minBound  =  normalize ds
        | otherwise      =  d:ds
    
    --Zero는 그냥 우리가 만든 자료형일 뿐. 개념적으로 NULL
    --자연수에 들어있는게 모두 minbound 면 n
    --all 함수 : 불 타입을 만족하는 함수가 들어오고 뒤에 있는 리스트가 그 함수에에 만족하면 TRUE 그외엔 FALSE
    isNull :: Nat -> Bool
    isNull n  =  all (== minBound) n
    
    -- all 정의 
    -- all f [] = True
    -- all f (d:ds)
    --     | f d = all f ds
    --     | otherwise = False
    
    


    -- 여기서부터신경쓰지
    -- 1 큰 자연수를 리턴
    -- norm 먼저 하고 reverse 먼저 넣어줌.
    -- 맨 끝 값이 가장큰 maxBound 면 minBound로 바꾸고 그 앞에꺼를 해야함.
    -- 리스트를 뒤집어 놓고 앞에서부터 하다가 다 끝나면 뒤집음.
    inc :: Nat -> Nat
    inc  =  reverse . inc' . reverse . normalize where
        inc' []              =  [succ minBound]         -- 1
        inc' (d:ds)
            | d == maxBound  =  minBound : inc' ds      --앞에꺼 계속
            | otherwise      =  succ d : ds

    -- -- Enum 타입 클래스 
    -- enumFrom 어떤수부터
    -- enumto 어떤수까지
    -- enimfromthen 어떤수부터 어떤 간격으로
    
    -- 1 작은 수
    -- 빈리스트면 에러 나야함 자연수니까 -1 정의 ㅗㅁㅅ하잖아!
    dec :: Nat -> Nat
    dec  =  normalize . reverse . dec' . reverse . normalize where
        dec' []  =  error "Cannot decrease further"
        dec' (d:ds)
            | d == minBound  =  maxBound : dec' ds
            | otherwise      =  pred d : ds
    
    addSlow :: Nat -> Nat -> Nat
    addSlow m n
        | isNull m   =  n -- m이 0이면
        | isNull n   =  m
        | otherwise  =  inc (inc (addSlow (dec m) (dec n))) -- 지난번에 만든거랑 똑같음.
    
    data Nat' = Nat' [Digit]
    
    instance Show Nat' where
        show (Nat' lst)  =  show (reverse lst)