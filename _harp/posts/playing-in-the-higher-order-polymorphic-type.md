「高階多相型ってなんですの？」

TaPLを読んだわけでもなくSystemFを理解したわけでもなくただキーワードに惹かれて高階多相型について最近調べていたのですが、面白い例を見つけたのでまとめてみようと思います。

##なんでも入るリスト

みなさんはJavaScriptを書きますか？僕はJavaScriptが大好きです。JavaScriptでは例えば以下のような配列を作ることができます。

```javascript
var array = [1, 2, "three", false, {"five":6}];
```

では同じようなことをHaskellでも出来るでしょうか？

```haskell
list = [1, 2, "three", False, ("five", 6)]
```

```bash
Couldn't match expected type ‘[Char]’ with actual type ‘Bool’
```

うーん、怒られてしまいますね。どうにかしてこのリストを正しく型付けできないでしょうか？

そこで出てくるのが高階多相型です！

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
 
data Any = forall a. Any a

list :: [Any]
list = [Any 1, Any 2, Any "three", Any False, Any ("five", 6)]
```

実行はできませんが実際これがghciで正常にロードできるのを確認してみてください！

このリストなんでも入ると言っておきながら`Any`というデータ構築子で中身を一回包んでいますね。データ構築子を使っていいなら例えばMaybe型だって全て`Maybe a`の型にできるので同じようなリストを作れそうなものです。

```haskell
list = [Just 1, Just 2, Just "three", Just False, Just ("five", 6)]
```

```bash
Couldn't match expected type ‘[Char]’ with actual type ‘Bool’
```

しかしさっきと全く同じエラーで怒られてしまいます。なんでだろうとセルフ型推論して見ると、まずリストの先頭は`Just 1`で型はたぶん`Num a => Just a`です。次の要素も型は同じで、その次の要素は`Just "three"`で型は`Just [Char]`になっています。これは型が違いますね！

ではなぜAny型では成功したのでしょうか。Maybe型とAny型の定義をもう一度並べてみます。

```haskell
data Any     = forall a. Any a
data Maybe a = Nothing | Just a
```

何が違うかわかりますか？一番の違いは型変数が左辺に現れているかどうか、つまりそもそも型のカインドが違うのです！

```bash
ghci> :k Any
Any :: *
ghci> :k Maybe
Maybe :: * -> *
```

`Maybe a` は`Nothing`もしくは`Just a`を値としてとります。この時`Just a`の`a`の型は`Maybe a`の型として決まってしまっています。それに対して`Any`は`forall a. Any a`を値としてとります。読み方は任意の型aに対して`Any a`はAny型の値となる、です。なのでAny型のデータ構築子`Any`に適用する値はどんな型のものでも大丈夫なのです！そしてそれら全てがAny型という一つの型に型付けされるからコンパイラに怒られなくて済むというわけです。

さてここまではいいのですが、いざ`list :: [Any]`を何かに使ってみようとすると少し困ったことがわかります。`Any`でくるんだ値はパターンマッチで取り出せばいいのですが、取り出したあとにそれがどういう型の値かわからないので使いようがありません。数値として扱えるのか`print`で表示できるのか`==`で評価できるのかさえわかりません。

ですのでもう少し使いやすくするためにAny型より少し制約を強めた型を作ってみましょう

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

data Printable = forall a. Show a => Printable a

instance Show Printable where
    show (Printable a) = show a

list :: [Printable]
list = [Printable 1, Printable 2, Printable "three", Printable False, Printable ("five", 6)]

main = print list
```

```bash
$ runhaskell Main.hs
[1,2,"three",False,("five",6)]
```

やった！ごちゃまぜのリストを表示することができました！！すごいぞ高階多相型！！！

Printable型の定義は

```haskell
data Printable = forall a. Show a => Printable a
```

こうなっています。読み方は任意の型aに対してもしそれがShowクラスのインスタンスなら`Printable a`はPrintable型の値となる、です。Showクラスのインスタンスで無い型の値を`Printable`で包むことはできません。Printable型をShowクラスのインスタンスにするのは簡単で単純に中身を取り出して`show`を適用すればいいだけですね！

##高階多相型が必要になる時
面白いことが出来るのはわかったけど実際に使いどころはあるのだろうか？ネットの海をさまよっているとstackoverflowでこんな質問を見つけました。[Polymorphism within higher-order functions?](http://stackoverflow.com/questions/7061538/polymorphism-within-higher-order-functions). なるほどこれは高階多相型が役に立っている！この章ではこのstackoverflowの内容を元に高階多相型の使用例を解説したいと思います。

```haskell
data Value = I Int
           | S String
           | NoValue
```

例えば簡単なプログラミング言語を作ろうと思った時など上のように"値"を一括りにするような型が欲しくなったりします。例えばこのValue型を比較するために`deriving Eq`などでEqクラスのインスタンスにすると

```haskell
I 1       == I 1        -- True
S "test"  == S "test"   -- True
I 1       == I 2        -- False
S "apple" == S "orange" -- False
```

となりますが

```haskell
I 1      == S "test" -- False
S "test" == NoValue  -- False
```

のようにデータ構築子が違う時も`False`を返してしまい、データ構築子が同じで中身が違うから`False`なのかそもそもデータ構築子が違うのか見分けがつかなくなってしまいます。

そこで新しい比較演算子`equal :: Value -> Value -> Maybe Bool`を用意してデータ構築子がそもそも違うときは`Nothing`を返して、データ構築子が同じなら中身を比較して`Just`に包んで返すようにしましょう。

```haskell
equal :: Value -> Value -> Maybe Bool
equal (I x) (I y) = Just (x == y)
equal (S x) (S y) = Just (x == y)
equal _ _ = Nothing

unequal :: Value -> Value -> Maybe Bool
unequal (I x) (I y) = Just (x == y)
unequal (S x) (S y) = Just (x == y)
unequal _ _ = Nothing
```

ついでに`unequal`も作りました。でもこれValue型の型構築子が増えるたびにすべての関数の定義を修正するのはあまり嬉しくないですよね。実際の動作は`==`や`/=`の部分であとはそれをMaybe型で包んでるだけなのでもう少しうまく書けそうです。なので`helper :: (Eq a) => (a -> a -> Bool) -> Value -> Value -> Maybe Bool`のような関数をつかって比較関数を生成できるようにしてやりましょう

```haskell
helper :: (Eq a) => (a -> a -> Bool) -> Value -> Value -> Maybe Bool
helper f (I x) (I y) = Just (f x y)
helper f (S x) (S y) = Just (f x y)
helper _ _ _ = Nothing

equal :: Value -> Value -> Maybe Bool
equal = helper (==)

unequal :: Value -> Value -> Maybe Bool
unequal = helper (/=)
```

どうです！しかしこれはコンパイルが通りません。

```markup
No instance for (Eq a0) arising from a use of ‘helper’
The type variable ‘a0’ is ambiguous
```

と言って怒られてしまいます。それもそのはずで、

```haskell
helper f (I x) (I y) = Just (f x y)
```

の時`f`は`Int -> Int -> Bool`と型付けされ

```haskell
helper f (S x) (S y) = Just (f x y)
```

の時`f`は`String -> String -> Bool`と型付けされるので

```haskell
helper :: (Eq a) => (a -> a -> Bool) -> Value -> Value -> Maybe Bool
```

という型では定義の中で`a`が一貫して同じ型である必要があるのでコンパイルエラーになります。そこで高階多相型の出番です！

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

data Value = I Int
           | S String
           | NoValue

helper :: (forall a. Eq a => a -> a -> Bool) -> Value -> Value -> Maybe Bool
helper f (I x) (I y) = Just (f x y)
helper f (S x) (S y) = Just (f x y)
helper _ _ _ = Nothing

equal :: Value -> Value -> Maybe Bool
equal = helper (==)

unequal :: Value -> Value -> Maybe Bool
unequal = helper (/=)
```

思わず全部のコードを書いてしましました。大事なのは`helper`の型ですね。

```haskell
helper :: (forall a. Eq a => a -> a -> Bool) -> Value -> Value -> Maybe Bool
```

この型だと

```haskell
helper f (I x) (I y) = Just (f x y)
helper f (S x) (S y) = Just (f x y)
```

の定義で`f`の型は`Eq a => a -> a -> Bool`と型付けされます。これは`==`や`/=`の型そのものですね！なので制約を受けること無く使うことができるのです。

以上2つの例を通して高階多相型を見てきました。これはとても便利そうですね。自分もまだ学び始めたところなのでまだまだ面白い使い方を知ってるよとかいや高階多相型はそうじゃないんだと教えてくださる方がいらしたら是非教えてください！
