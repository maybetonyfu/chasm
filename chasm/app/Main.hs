{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import RIO
import Data.String.QQ

program :: Text
program = [s|

Main.hs
--------------
x 0 = 1
x 1 = 1
x n = x (n - 1) + x (n - 2)

t = x '9'


All constraints
--------------
0 : x <=> v1
1 : v1 <=> Function Int Int
2 : x <=> v2
3 : v2 <=> Function Int Int
4 : x <=> v3
5 : v3 <=> Function v4 v5
6 : v4 <=> n
7 : v6 <=> Function v7 v8 v9
8 : v6 <=> (+)
9 : v5 <=> v9
10 : v10 <=> Function v11 v12
11 : v10 <=> x
12 : v12 <=> v7
13 : v13 <=> Function v14 v15
14 : v13 <=> x
15 : v15 <=> v8
16 : v16 <=> Function v17 v18 v19
17 : v16 <=> (-)
18 : v19 <=> v11
19 : v17 <=> n
20 : v18 <=> Int
21 : v20 <=> Function v21 v22 v23
22 : v20 <=> (-)
23 : v23 <=> v14
24 : v21 <=> n
25 : v22 <=> Int
26 : t <=> v24
27 : v27 <=> v24
28 : v25 <=> Function v26 v27
29 : v25 <=> x
30 : v26 <=> Char
31 : (+) <=> Function Int Int Int
32 : (-) <=> Function Int Int Int


MUS 0
--------------
0 : x <=> v1
1 : v1 <=> Function Int Int
28 : v25 <=> Function v26 v27
29 : v25 <=> x
30 : v26 <=> Char


MUS 1
--------------
2 : x <=> v2
3 : v2 <=> Function Int Int
28 : v25 <=> Function v26 v27
29 : v25 <=> x
30 : v26 <=> Char


MUS 2
--------------
4 : x <=> v3
5 : v3 <=> Function v4 v5
6 : v4 <=> n
16 : v16 <=> Function v17 v18 v19
17 : v16 <=> (-)
19 : v17 <=> n
28 : v25 <=> Function v26 v27
29 : v25 <=> x
30 : v26 <=> Char
32 : (-) <=> Function Int Int Int


MUS 3
--------------
4 : x <=> v3
5 : v3 <=> Function v4 v5
6 : v4 <=> n
21 : v20 <=> Function v21 v22 v23
22 : v20 <=> (-)
24 : v21 <=> n
28 : v25 <=> Function v26 v27
29 : v25 <=> x
30 : v26 <=> Char
32 : (-) <=> Function Int Int Int
|]

main :: IO ()
main = runSimpleApp $ do 
    logInfo (display program)
