---
title: Haskellで簡単に乱数を使ってみる
date: 2015-04-08
---

Haskellで乱数というとすぐにStdGenが出てきて面倒くさいイメージがありますが
再現性を気にせず手軽にランダムな値が欲しいだけなら難しいことを考える必要はありません

以下はモンテカルロ法で円周率を求めるプログラムです。
乱数生成に使っているのは[System.Random](https://hackage.haskell.org/package/random/docs/System-Random.html)の[randomRIO](https://hackage.haskell.org/package/random/docs/System-Random.html#v:randomRIO)という[Random](https://hackage.haskell.org/package/random/docs/System-Random.html#t:Random)クラスのメソッドです

```haskell
import System.Random

trial :: Int -> IO Int
trial inner = do
    x <- randomRIO (0, 1) :: IO Double
    y <- randomRIO (0, 1) :: IO Double
    if x ^ 2 + y ^ 2 <= 1
    then return (inner + 1)
    else return inner

experience :: [IO Int]
experience = iterate (>>= trial) (return 0)

main = do
    inner <- experience !! 10000
    print $ 4 * (fromIntegral inner) / 10000
```

これで`x, y`には0から1までのランダムなDoulbeの値が入ります
