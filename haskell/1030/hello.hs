--파일 하나당 모듈 하나로 생각.
--모듈이름을 시작할때 첫문자가 대문자여야 함
--모듈명과 파일명이 달라도 괜찮다. 근데 혼란스러우니 모듈이름 일치하자.
module Hello where

{-블럭주석다는법-}
{-
{-중첩주석-}
여기도 주석
-}
--이 아래로는 모듈의 정의들

--변수
--변수와 함수는 무조껀 소문자로 시작해야함
--나중에 배울 데이터 타입은 대문자로 시작해야함

--아래는 절차형 언어와 다르게 대입이 아닌 정의
--대부분 디폴트값을 계산해서 넣어줌
--integer
x = 3
--위문장 다음에 x = 5로 재정의 불가능하다.

--정의의 순서는 중요하지 않음
--아래처럼 어딘가에 있기만 하면 괜찮다
--패턴매칭외에는 순서 마음대로 해도 괜찮다.
y = z + 1
z = 8

--명시적으로 타입 명시하기
x' :: Double
x' = 3
{-
*Hello> x'
3.0
*Hello> :t x'
x' :: Double
-}

--type safe한 언어이기 때문에 아래같은 언어는 불가능
--3을 문자로 타입 추론 하는것은 절대 불가능
--x'' :: Char
--x'' = 3
--여기까지는 이미 화면에서 어떻게 찍을지 아는얘들
--타입 클래스중 show 클래스 

--함수정의
--아래 f와 f'은 같다.
f = \x -> x + x
--위에 보단 밑에가 defualt타입으로 안변하는 안전한 방식
f' x = x + x
{-
*Hello> f
<interactive>:35:1: error:
    • No instance for (Show (Integer -> Integer))
-}
{-
*Hello> :t f
f :: Integer -> Integer
-}
{-
*Hello> :browse
x :: Integer
y :: Integer
z :: Integer
x' :: Double
f :: Integer -> Integer
f' :: Num a => a -> a
-}
{-
*Hello> :info f
f :: Integer -> Integer 	-- Defined at hello.hs:47:1
-}

--중위연산자의 정의
--특수기호를 이용해서 정의
--파라메터로 x y 쓰는중
--중위연산자를 괄호로 쓰면 함수됨
x ### y = x + x * y
--함수로 정의
--파라메터로 x y쓰는중
foo x y = x + (x * y)

{-
*Hello> 4###8
36
it :: Num a => a
*Hello> foo 5 6
35
it :: Num a => a
--중위연산자를 괄호로 감싸면 함수
*Hello> (###) 4 6
28
it :: Num a => a
--`(back quate) 로 함수를 감싸면 중위연산자 
*Hello> 4 `foo` 8
36
it :: Num a => a
-}

{-
*Hello> :t (###)
(###) :: Num a => a -> a -> a

--아래의 경우 좌측 피연산자가 필요한 함수로 인식

*Hello> :t (### 3)
(### 3) :: Num a => a -> a

--아래의 경우 우측 피연산자가 필요한 함수로 인식

*Hello> :t (4 ###)
(4 ###) :: Num a => a -> a

*Hello> (< 5) 3
True
it :: Bool

-}


bar x y z = x + (y * z)
{-*Hello> (4 `bar` 5) 6
34
it :: Num a => a
-}

{-
*Main> :cd ~/19-2_class/haskell/1030
Warning: changing directory causes all loaded modules to be unloaded,
because the search path has changed.
*Main> :load hello.hs
[1 of 1] Compiling Hello            ( hello.hs, interpreted )
Ok, modules loaded: Hello.
*Hello> :reload
[1 of 1] Compiling Hello            ( hello.hs, interpreted )
Ok, modules loaded: Hello.
*Hello> x
3
*Hello> y --y는 z+1으로만 가지고 있다가, 명령창에 y 를 칠때만 계산함. 느긋한 계산.
9
*Hello> :t y
y :: Integer
*Hello> 
-}
