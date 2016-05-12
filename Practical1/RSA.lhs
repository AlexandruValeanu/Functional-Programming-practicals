Code for the very optional part of Practical 1

> import Data.Char

> n = 1963272347809

> type Block = Integer
> type CipherText = [Block]

Encrypt a string

> encrypt :: String -> CipherText
> encrypt = map encryptBlock . toBlocks

Encrypt a block

> encryptBlock :: Block -> Block
> encryptBlock x = expMod x 3 n

Calculate y^m mod n

> expMod :: Integer -> Integer -> Integer -> Integer
> expMod y m n = expMod' y m 1
>   where expMod' y m z -- calculate (y^m * z) `mod` n
>          | m==0      = z
>          | even m    = expMod' ((y*y) `mod` n) (m `div` 2) z
>          | odd m     = expMod' y (m-1) ((y*z) `mod` n)

Split a string into blocks

> toBlocks :: String -> [Block]
> toBlocks [] = []
> toBlocks xs = chars2block (take 5 xs) : toBlocks (drop 5 xs)

Convert list of characters into single integer.  Each character is interpreted
as an integer in base 128.

> chars2block :: [Char] -> Block
> chars2block (c:cs) = chars2block' (fromIntegral (ord c)) cs
> chars2block' e [] = e
> chars2block' e (c:cs) = chars2block' (e*128 + (fromIntegral (ord c))) cs

The result of the encryption is the following

> cipherText :: CipherText
> cipherText =
>  [772653220544,1915678282609,276920912360,1140547573312,317556866509,
>   665998855449,1882188522238,653343178985,539272906038,1331719140974,
>   1881623288858,1221070877456,97336]

An alternative encrypted text:

> cipherText2 :: CipherText
> cipherText2 =
>  [1834325134427, 871052444065, 1092404683400, 404932447487, 1030739040307,
>   299852500588, 993189881531,  228941291814,       250047]

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




