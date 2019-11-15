module HW3 where

    -- 패턴 매칭 (설명 예시 코드)
    
    f :: Integer -> Bool
    f 3  =  True
    f 5  =  False
    
    f' :: Integer -> Bool
    f' 3  =  True
    f' n  =  False
    
    f'' :: Integer -> Bool
    f'' n  =  False
    f'' 3  =  True
    
    
    
    fac :: Integer -> Integer
    fac 0 = 1
    fac n = n * fac (n - 1)
    
    
    rev :: [a] -> [a]
    rev [] = []
    rev (x:xs) = (rev xs)  ++ [x]
    
    
    
    -- 타입 정의 (설명 예시 코드)
    
    data List t  =  EmptyList | ListCons t (List t)
    
    data Nat  =  Z
              |  S Nat
        deriving Show
    
    toNat :: Integral i => i -> Nat
    toNat 0          =  Z
    toNat n | n > 0  =  S (toNat (n-1))

    
    fromNat :: Integral i => Nat -> i
    
    fromNat Z       = 0
    fromNat (S n)   = 1 + fromNat(n)

    
    addNat :: Nat -> Nat -> Nat
    
    addNat Z Z          = Z
    addNat n Z          = n
    addNat Z m          = m
    --normal
    addNat (S n) (S m)  = S( S (addNat n m))
    

    mulNat :: Nat -> Nat -> Nat
    
    --decalation for 0
    mulNat _ Z      = Z
    mulNat Z _      = Z
    --decalation for 1
    mulNat n (S Z)  = n
    mulNat (S Z) m  = m
    --normal
    mulNat (S n) (S m) = (S Z) `addNat` n `addNat` m `addNat` (mulNat n m)
    
    
    facNat :: Nat -> Nat
    
    facNat Z     = (S Z)
    facNat (S n) = (S n) `mulNat` (facNat n) 

