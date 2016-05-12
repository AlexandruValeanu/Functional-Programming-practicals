> import Data.Char

File pa1.lhs for Practical 1: Factoring Numbers

Finding the smallest prime factor

> factor :: Integer -> (Integer,Integer)
> factor n = factorFrom 2 n

> factorFrom :: Integer -> Integer -> (Integer,Integer)
> factorFrom m n 
>   | r == 0     = (m, q)
>   | n <= m * m = (n, 1)
>   | otherwise  = factorFrom (m + 1) n
>   where (q, r) = divMod n m

Exercise 1
"factorFrom m n" is searching for the smallest factor of n, in the range [m...sqrt(n)].
If n is not prime, then n has a factor smaller than (or equal to) sqrt(n)
(let f be that factor ; if f would be bigger than sqrt(n) then f * f > n which leads to a contradiction)
So, we don't have to go all the way to n in order to find a factor.
Stopping the function using n <= m * m as a test will lead to a complexity of O(sqrt n). 
On the other hand, using the test n == m will lead to a complexity of O(n).
The first version is clearly better.

The first two lines can be interchanged.
The only difference will be when n is equal to 0.
The original algorithm returns (2,0) while the second returns (0,1). (both values returned are correct)

There will be approximately sqrt(n) recursive calls, leading to a O(sqrt n) complexity.

Exercise 2 (and 3)
factor 0 = (2, 0)
factor 1 = (1, 1)

An improved version, as explained in the text of the practical:

> factorFrom1 :: Integer -> Integer -> (Integer,Integer)
> factorFrom1 m n
>   | r == 0     = (m, q)
>   | q <= m     = (n, 1)
>   | otherwise  = factorFrom1 (m + 1) n
>   where (q, r) = divMod n m

Exercise 4
q is defined as [n / m], where [x] is the largest integer not larger than x.
The only difference between factorFrom and factorFrom1 is between the tests : "n <= m * m" and "q <= m". 
We can turn the first test into the second by dividing the inequality by m.

factorFrom1 is more efficient than factorFrom because is does one less multiplication.
factorFrom1 does not compute m * m which makes it faster when m is a large number.

Exercise 5

> factor2 :: Integer -> (Integer,Integer)
> factor2 n = 
>   if n == 0 then (0, 0)
>   else if r == 0 then (2, q)
>   else factorFrom2 3 n
>   where (q, r) = divMod n 2

> factorFrom2 :: Integer -> Integer -> (Integer,Integer)
> factorFrom2 m n
>   | r == 0    = (m, q)
>   | q <= m    = (n, 1)
>   | otherwise = factorFrom2 (m + 2) n
>   where (q, r) = divMod n m

Complexity
The algorithm's complexity remains the same, O(sqrt N).
In practice, the function should run twice as fast as factor (the original one).

Exercise 6

Tests : 
*Main> factor2 0
(0,0)
*Main> factor2 1
(1,1)
*Main> factor2 2
(2,1)
*Main> factor2 10
(2,5)
*Main> factor2 100
(2,50)
*Main> factor2 13
(13,1)
*Main> factor2 91
(7,13)
*Main> factor2 9991
(97,103)
*Main> factor2 1000000007
(1000000007,1)
*Main> factor2 9982341
(3,3327447)
*Main> factor2 654343456
(2,327171728)
*Main> factor2 654343457
(73,8963609)


Exercise 7

> factor3 :: Integer -> (Integer,Integer)
> factor3 n = 
>   if n == 0 then (0, 0)
>   else if r1 == 0 then (2, q1)
>   else if r2 == 0 then (3, q2)
>   else factorFrom3 5 n 2
>   where (q1, r1) = divMod n 2
>         (q2, r2) = divMod n 3

> factorFrom3 :: Integer -> Integer -> Integer -> (Integer,Integer)
> factorFrom3 m n step
>   | r == 0    = (m, q)
>   | q <= m    = (n, 1)
>   | otherwise = factorFrom3 (m + step) n (6 - step)
>   where (q, r) = divMod n m

Tests : 

*Main> factor3 0
(0,0)
*Main> factor3 1
(1,1)
*Main> factor3 2
(2,1)
*Main> factor3 3
(3,1)
*Main> factor3 4
(2,2)
*Main> factor3 5
(5,1)
*Main> factor3 10
(2,5)
*Main> factor3 100
(2,50)
*Main> factor3 121
(11,11)
*Main> factor3 139
(139,1)
*Main> factor3 91
(7,13)
*Main> factor3 5672
(2,2836)
*Main> factor3 567254353
(3779,150107)
*Main> factor3 1000000009
(1000000009,1)
*Main> factor3 53452343445
(3,17817447815)

Exercise 8 - Finding all prime factors

The downside : we have to generate all the primes smaller than the number we want to factorize.
For this problem we don't have a sublinear algorithm.

> primeFactors :: Integer -> [Integer]
> primeFactors n = factorsFrom 2 n

> factorsFrom :: Integer -> Integer -> [Integer]
> factorsFrom m n =
>    if n == 1 then [] else p:factorsFrom p q
>    where (p,q) = factorFrom m n

Tests : 

*Main> primeFactors 2
[2]
*Main> primeFactors 10
[2,5]
*Main> primeFactors 20
[2,2,5]
*Main> primeFactors 191
[191]
*Main> primeFactors 192
[2,2,2,2,2,2,3]
*Main> primeFactors 1024
[2,2,2,2,2,2,2,2,2,2]
*Main> primeFactors 1023
[3,11,31]
*Main> primeFactors 443132
[2,2,139,797]
*Main> primeFactors 4431325453
[4431325453]
*Main> primeFactors 4431325453321
[13,4931,69128207]
*Main> primeFactors 93214124
[2,2,23,1013197]
*Main> primeFactors 93432432423
[3,11,11,31,8302891]
*Main> primeFactors 34
[2,17]
*Main> primeFactors 58
[2,29]
*Main> primeFactors 368880
[2,2,2,2,3,5,29,53]

Exercise 9 (and 10)

> primeFactors2 :: Integer -> [Integer]
> primeFactors2 n
>   | r1 == 0   = 2:primeFactors2 q1
>   | r2 == 0   = 3:primeFactors2 q2
>   | otherwise = factorsFrom2 5 n
>   where (q1, r1) = divMod n 2
>         (q2, r2) = divMod n 3

> getStep :: Integer -> Integer
> getStep m
>   | m `rem` 4 == 1 = 2
>   | otherwise      = 4

> factorsFrom2 :: Integer -> Integer -> [Integer]
> factorsFrom2 m n = 
>   if n == 1 then []
>   else p:factorsFrom2 p q
>   where s = getStep m
>         (p, q) = factorFrom3 m n s 

Tests :

*Main> primeFactors 3548752904375840000000000
[2,2,2,2,2,2,2,2,2,2,2,2,2,2,5,5,5,5,5,5,5,5,5,5,7,19,23,7250639311]
(0.12 secs, 58936696 bytes)
*Main> primeFactors2 3548752904375840000000000
[2,2,2,2,2,2,2,2,2,2,2,2,2,2,5,5,5,5,5,5,5,5,5,5,7,19,23,7250639311]
(0.08 secs, 20707888 bytes)

*Main> primeFactors 87534928572430985
[5,103,169970735092099]
(9.60 secs, 7406430208 bytes)
*Main> primeFactors2 87534928572430985
[5,103,169970735092099]
(3.76 secs, 2539011040 bytes)

*Main> primeFactors 4357438958934
[2,3,3,242079942163]
(0.38 secs, 280176400 bytes)
*Main> primeFactors2 4357438958934
[2,3,3,242079942163]
(0.15 secs, 96568648 bytes)

*Main> primeFactors 214343543253465466356
[2,2,3,79,8207707,27547375171]
(6.89 secs, 5581983008 bytes)
*Main> primeFactors2 214343543253465466356
[2,2,3,79,8207707,27547375171]
(2.60 secs, 1905126136 bytes)

Jevons' problem :
*Main> primeFactors 8616460799
[89681,96079]
(0.07 secs, 52750456 bytes)
*Main> primeFactors2 8616460799
[89681,96079]
(0.03 secs, 18615032 bytes)

Conclusion : primeFactors2 is much better than primeFactor. (around 2.6x faster)

-------------------------------OPTIONAL PART---------------------------------

Exercise 11

n = u * v; 
n = x^2 - y^2 , 0 <= y < x
x = (u + v) / 2 ; y = (u - v) / 2

Both x and y are even because they are the sum/difference of two odd numbers (u and v are both odd because n is odd and n = u * v).

The smallest possible value of x is sqrt(n), because sqrt(n) * sqrt(n) - 0 = n. (y = 0 in the last equation)


> search :: Integer -> Integer -> Integer -> (Integer,Integer)
> search p q r
>   | r == 0    = (p, q)
>   | r < 0     = search (p+1) q (r + 2 * p + 1)
>   | otherwise = search p (q + 1) (r - 2 * q - 1)

> fermat :: Integer -> (Integer,Integer)
> fermat n = (x + y, x - y)
>        where (x, y) = search p q r
>  				where (p, q, r) = (isqrt n, 0, (p * p) - (q * q) - n)

> isqrt :: Integer -> Integer
> isqrt = truncate.sqrt.fromInteger

Tests :

*Main> fermat (1000000007 * 1000000009)
(1000000009,1000000007)
(0.00 secs, 1550496 bytes)

*Main> fermat (3 * 71)
(71,3)
(0.00 secs, 1581600 bytes)

*Main> fermat (87 * 41)
(87,41)
(0.00 secs, 1549896 bytes)

*Main> fermat (7 * 47)
(47,7)
(0.00 secs, 1581408 bytes)

Exercise 12

Jevons :
*Main> fermat 8616460799
(96079,89681)
(0.01 secs, 3126640 bytes)

n = 1963272347809
*Main> fermat 1963272347809
(8123471,241679)
(6.24 secs, 5282200680 bytes)


-------------------------VERY OPTIONAL----------------------------------------

Calculate y^m mod n

> expMod :: Integer -> Integer -> Integer -> Integer
> expMod y m n = expMod' y m 1
>   where expMod' y m z -- calculate (y^m * z) `mod` n
>          | m==0      = z
>          | even m    = expMod' ((y*y) `mod` n) (m `div` 2) z
>          | odd m     = expMod' y (m-1) ((y*z) `mod` n)

The following code for the extended Euclidean algorithm might be useful.
Find g, a, b such that g = gcd(x,y) = a*x + b*y
Invariant: p = a*x+b*y, q = c*x+d*y, p >= q

> euclid :: Integer -> Integer -> (Integer, Integer, Integer)
> euclid x y =
>   if x>=y then euclid1 x y (1,0) (0,1)
>   else euclid1 y x (0,1) (1,0)
>   where euclid1 p q (a, b) (c, d) =
>           if q==0 then (p, a, b)
>           else euclid1 q r (c, d) (a-c*k, b-d*k)
>             where (k,r) = p `divMod` q

----------------

Texts : 

> cipherText =
>  [772653220544,1915678282609,276920912360,1140547573312,317556866509,
>   665998855449,1882188522238,653343178985,539272906038,1331719140974,
>   1881623288858,1221070877456,97336]

> cipherText2 =
>  [1834325134427, 871052444065, 1092404683400, 404932447487, 1030739040307,
>   299852500588, 993189881531,  228941291814,       250047]


----------------

RSA algorithm

1. Key generation
p, q primes
n = p * q

n = 1963272347809
p = 8123471 q = 241679

phi(n) = (p - 1) * (q - 1) = 1963263982660

1 < e < phi(n); gcd(e, phi(n)) = 1;
e = 3; gcd(3, 1963263982660) = 1

d = e^(-1) (modulo phi(n))
d = 3^(-1) (modulo 1963263982660)
d = -654421327553 = 1963263982660 - 654421327553 = 1308842655107

Public key : (n, e) = (1963272347809, 3)
Private key : (n, d) = (1963272347809, 1308842655107)

Note : p, q, phi(n) must also be private (because you can compute d using them)

2. Encryption

Suppose we want to send a message M.
We first turn M into m, such that 0 <= m < n and gcd(m, n) = 1 by using a reversible protocol known as a padding scheme.
We compute the ciphertext c, c = m^e (mod n) (computed using modular exponentiation)

The padding scheme is : turning a 5-char list into in integer considering every char as an integer in base 128.

3. Decryption

We recover m from c by computing m = c^d (mod n)
From m we can get M by reversing the padding scheme.

> addCharacter :: ([Char],Int) -> [Char]
> addCharacter (ls, newCh) =
>   if newCh > 0 then ls ++ [chr(newCh)]
>   else ls

> decryption_block :: Integer -> Integer -> [Char]
> decryption_block n lg =
>    if lg == 0 then []
>    else  addCharacter (newList, fromIntegral(newCh))
>         where newCh = n `rem` 128
>               newList = (decryption_block ((n - newCh) `div` 128) (lg - 1))

> getText :: [Integer] -> Int -> Int -> [Char]
> getText ls ind lg = 
>   if ind < lg then (decryption_block m 5) ++ (getText ls (ind + 1) lg)
>   else []
>   where m = expMod (ls !! ind) 1308842655107 1963272347809

Solutions : 

*Main> getText cipherText 0 (length cipherText)
"I think it is unlikely that anyone but myself will ever know."
*Main> getText cipherText2 0 (length cipherText2)
"I LOVE FUNCTIONAL PROGRAMMING, DON'T YOU?"

This completes the script.
