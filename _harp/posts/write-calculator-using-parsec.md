#Parsecで計算機を書いてみた
2015/02/10

Parsec に入門してみました。
構文解析の難しいことは知らずプログラムを書くのも始めてだったのですが、思っていたより全然簡単にかけたのでParsecに感動しています。
勉強のついでにメモとして動くパーサーを並べていきます。

<blockquote class="twitter-tweet" lang="ja"><p>Haskell 界隈はParsec持ち上げ過ぎやろって思ってたけど使ってみたらやっぱりParsecマジ最高&#10;<a href="https://t.co/USvgvKLv5p">https://t.co/USvgvKLv5p</a></p>&mdash; こひにぶく (@lotz84_) 2015, 2月 10
</blockquote>

勉強途中なので非効率な実装もあると思いますが、気づいたら随時更新していくつもりです。

##四則演算
###1+2+3
* 数字: 一桁
* 演算子: `+` のみ
* 空白なし

```haskell
import Data.Char
import Control.Applicative
import Text.Parsec

num = digitToInt <$> digit

op = const (+) <$> char '+'

expr = num `chainl1` op

main = print $ parse expr "" "1+2+3"
-- Right 6
```

###1+2-3
* 数字: 一桁
* 演算子: `+`, `-`
* 空白なし

```haskell
import Data.Char
import Control.Applicative
import Text.Parsec hiding ((<|>))

num = digitToInt <$> digit

op = (const (+) <$> char '+') <|> (const (-) <$> char '-')

expr = num `chainl1` op

main = print $ parse expr "" "1+2-3"
-- Right 0
```

###10+2-3
* 数字: 自然数
* 演算子: `+`, `-`
* 空白なし

```haskell
import Data.Char
import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>))

num = do
  xs <- many $ digitToInt <$> digit
  return $ foldl f 0 xs
  where
  f x y = x * 10 + y

op = (const (+) <$> char '+') <|> (const (-) <$> char '-')

expr = num `chainl1` op

main = print $ parse expr "" "10+2-3"
-- Right 9
```

###1.2+2.0+3
* 数字: 非負実数
* 演算子: `+`, `-`
* 空白なし

```haskell
import Data.Char
import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>))

digitToDouble :: Char -> Double
digitToDouble = fromIntegral . digitToInt

num = do
  xs <- many $ digitToDouble <$> digit
  dot <- optionMaybe (char '.')
  ys <- many $ digitToDouble <$> digit
  return $ foldl f 0 xs + foldl g 0 ys
  where
  f x y = x * 10 + y
  g x y = x + y * 0.1

op = (const (+) <$> char '+') <|> (const (-) <$> char '-')

expr = num `chainl1` op

main = print $ parse expr "" "1.2+2.0-3"
-- Right 0.20000000000000018
```

###10 + 2.0 - 3
* 数字: 非負実数
* 演算子: `+`, `-`
* 空白あり

```haskell
import Data.Char
import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>))

digitToDouble :: Char -> Double
digitToDouble = fromIntegral . digitToInt

symbol xs = do
  result <- string xs
  spaces
  return result

num = do
  xs <- many $ digitToDouble <$> digit
  dot <- optionMaybe (char '.')
  ys <- many $ digitToDouble <$> digit
  spaces
  return $ foldl f 0 xs + foldl g 0 ys
  where
  f x y = x * 10 + y
  g x y = x + y * 0.1

op = (const (+) <$> symbol "+") <|> (const (-) <$> symbol "-")

expr = do
  spaces
  num `chainl1` op

main = print $ parse expr "" "10 + 2.0 - 3"
-- Right 9.0
```

###10 + 2.0 * 3 / 4
* 数字: 非負実数
* 演算子: `+`, `-`, `*`, `/`
* 空白あり

```haskell
import Data.Char
import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>))

digitToDouble :: Char -> Double
digitToDouble = fromIntegral . digitToInt

symbol xs = do
  result <- string xs
  spaces
  return result

num = do
  xs <- many $ digitToDouble <$> digit
  dot <- optionMaybe (char '.')
  ys <- many $ digitToDouble <$> digit
  spaces
  return $ foldl f 0 xs + foldl g 0 ys
  where
  f x y = x * 10 + y
  g x y = x + y * 0.1

op0 = (const (*) <$> symbol "*") <|> (const (/) <$> symbol "/")
op1 = (const (+) <$> symbol "+") <|> (const (-) <$> symbol "-")

expr = do
  spaces
  num `chainl1` op0 `chainl1` op1

main = print $ parse expr "" "10 + 2.0 * 3 / 4"
-- Right 11.5
```

###(10 + 2.0) * 3 / 4
* 数字: 非負実数
* 演算子: `+`, `-`, `*`, `/`
* 空白あり
* 括弧あり

```haskell
import Data.Char
import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>))

digitToDouble :: Char -> Double
digitToDouble = fromIntegral . digitToInt

symbol xs = do
  result <- string xs
  spaces
  return result

num = do
  xs <- many $ digitToDouble <$> digit
  dot <- optionMaybe (char '.')
  ys <- many $ digitToDouble <$> digit
  spaces
  return $ foldl f 0 xs + foldl g 0 ys
  where
  f x y = x * 10 + y
  g x y = x + y * 0.1

parens = do
  symbol "("
  result <- expr
  symbol ")"
  return result

term = try parens <|> num

op0 = (const (*) <$> symbol "*") <|> (const (/) <$> symbol "/")
op1 = (const (+) <$> symbol "+") <|> (const (-) <$> symbol "-")

expr = do
  spaces
  term `chainl1` op0 `chainl1` op1

main = print $ parse expr "" "(10 + 2.0) * 3 / 4"
-- Right 9.0
```

##AST
ここまでは式の文字列から直接計算した結果を返していましたがここでは抽象構文木を生成するようにしてみます。
計算のASTを`Expr`というデータ型で表し文字列をパースしてデータを作ろうと思います。
実際にやってみると上のソースからほんの少しの修正でASTを作ることができました！

```haskell
import Data.Char
import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>))

data Expr = Value Double
          | Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Divide Expr Expr
          deriving Show

digitToDouble :: Char -> Double
digitToDouble = fromIntegral . digitToInt

symbol xs = do
  result <- string xs
  spaces
  return result

num = do
  xs <- many $ digitToDouble <$> digit
  dot <- optionMaybe (char '.')
  ys <- many $ digitToDouble <$> digit
  spaces
  return $ Value $ foldl f 0 xs + foldl g 0 ys
  where
  f x y = x * 10 + y
  g x y = x + y * 0.1

parens = do
  symbol "("
  result <- expr
  symbol ")"
  return result

term = try parens <|> num

op0 = (const Times <$> symbol "*") <|> (const Divide <$> symbol "/")
op1 = (const Plus  <$> symbol "+") <|> (const Minus  <$> symbol "-")

expr = do
  spaces
  term `chainl1` op0 `chainl1` op1

main = print $ parse expr "" "1.2+2.0-3"
-- Right (Minus (Plus (Value 1.2) (Value 2.0)) (Value 3.0))
```
