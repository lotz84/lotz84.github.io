---
title: Haskellを書いてるプログラマーはザコなのか
date: 2015-04-13
---

**tl;dr**  
そんなわけないからJavaと比較して検討してみた。

自分のツイートがプチ炎上しました

[![](http://i.gyazo.com/04de63d6dfce54b05f05c8d0d575f1e3.png)](https://twitter.com/lotz84_/status/586785908105416704)

関数型界隈はただでさえコミュニティが尖ってて怖いとか言われるのに自分もその端くれながら攻撃的な発言をしてしまって申し訳なく思っています。弁明も兼ねてnullが無くても再代入ができ無くても社会で通用するコードを量産しているJavaと負けず劣らないコードが書けることを具体例を上げて説明したいと思います。

##再代入できない
Haskellは変数への再代入ができません。どういうことかというと一度値を代入した値に別の値を入れることが不可能なのです。例えばJavaでは

```java
class Main {
    public static void main(String[] args) {
        int x = 0;
        x = 1;

        System.out.println(x);
    }
}
```

この様に一度宣言した変数xに何回も値を代入することができます。しかしHaskellでは

```haskell
x = 0
x = 1

main = print x
```

なんて書くと`Multiple declarations of ‘x’`と言ってコンパイラに怒られます。

だったら再代入が絶対に必要そうな処理はどうかくのか、例えば配列の中身を全部足し合わせる処理は合計を保存するような変数を用意して`for`文でリストの中身を回して足し合わせなければいけないような気がします。Javaで書くと

```java
class Main {
    public static void main(String[] args) {
        int[] array = {1, 2, 3, 4, 5};
        int total = 0;
        for (int i : array) {
            total += i;
        }

        System.out.println(total);
    }
}
```

こう書けます。ではHaskellで同じような処理を書くとどうなるでしょう。

```haskell
array = [1, 2, 3, 4, 5]
total = sum array

main = print total
```

こうなります。ですがちょっと待って下さい、今回は`sum`という**リストの中身を全部足し合わせる**関数がたまたまあっただけかもしれません。しかし`sum`を更に詳しく見ていけば再代入が不要になるトリックが見えてきます。`sum`の定義は

```haskell
sum = foldl (+) 0
```

となっています。`foldl`という新しい関数が出てきました。この関数の定義は

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z []     =  z
foldl f z (x:xs) =  foldl f (f z x) xs
```

となります。目を回すかもしれませんが具体的な値を入れてその動作を見て行きましょう。 さっきの例の場合は結局

```haskell
foldl (+) 0 [1, 2, 3, 4, 5]
```

を計算することになります。これを`foldl`の定義を照らしあわせて評価していくと

```markup
foldl (+) 0 [1, 2, 3, 4, 5]
foldl (+) 0 (1 : [2, 3, 4, 5])
foldl (+) (0 + 1) [2, 3, 4, 5]
foldl (+) (0 + 1) (2 : [3, 4, 5])
foldl (+) ((0 + 1) + 2) [3, 4, 5]
foldl (+) ((0 + 1) + 2) (3 : [4, 5])
foldl (+) (((0 + 1) + 2) + 3) [4, 5]
foldl (+) (((0 + 1) + 2) + 3) (4 : [5])
foldl (+) ((((0 + 1) + 2) + 3) + 4) [5]
foldl (+) ((((0 + 1) + 2) + 3) + 4) (5 : [])
foldl (+) (((((0 + 1) + 2) + 3) + 4) + 5) []
((((0 + 1) + 2) + 3) + 4) + 5
1 + 2 + 3 + 4 + 5
```

`foldl`という関数がリストの値を1つづつ足していく関数を巧みに組み立てていくのがおわかりいただけたでしょうか。Haskellがやっていたのは変数に値を足していくことではなく、**関数で関数を組み上げていくこと**だったのです。同じ方法を用いて"文字列を全て足し合わせる処理"とか"リストの中の最大値を求める処理"を書くこともできます。

```haskell
strs = ["apple", "orange", "grape"]
coupled = foldl (++) "" strs

main = putStrLn coupled
-- appleorangegrape
```

```haskell
nums = [3, 1, 4, 1, 5, 9, 2]
highest = foldl max 0 nums

main = print highest
-- 9
```

累積用の変数も一時保存用の変数も必要ありません。

もちろんこんな単純な例だけで再代入を使わずに全てのプログラムが書けることを証明したわけではありません。ですが再代入を使えなくても案外いろんな処理を書けるものだと思ってもらえれば幸いです。

##nullが無い
最近になってJavaにもOptionalが導入されましたが既存ライブラリがすぐに対応するわけでもなく今でもJavaはnullの温床になっています。nullをなんでこんなに悪く言うのかというと10億ドルの過ちだからとか別にそういうのはどうでもよくて単にこいつのせいで何度もコードが落ちて大変な目にあったからです。個人的な恨みです。

具体例から見て行きましょう。名前と成績が組になったデータ構造を考えてその配列から特定の成績をとった人の名前を調べたいとします。Javaの場合は成績と配列を渡せば目的の値を返すか、もし見つからなければnullを返すような関数を書けばいいでしょう。

```java
class Result {
    private String name;
    private Integer score;

    Result (String name, Integer score) {
        this.name = name;
        this.score = score;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Integer getScore() {
        return score;
    }

    public void setScore(Integer score) {
        this.score = score;
    }
}
```

```java
class Main {
    public static void main(String[] args) {
        Result array[] = new Result[5];
        array[0] = new Result("藤原", 80);
        array[1] = new Result("水橋", 65);
        array[2] = new Result("伊吹", 20);
        array[3] = new Result("八雲", 100);
        array[4] = new Result("散野", 0);

        Result target = search(100, array);
        if (target == null) {
            System.out.println("満点はいませんでした");
        } else {
            System.out.println("満点は" + target.getName() + "です");
        }
    }

    public static Result search(Integer score, Result[] array) {
        if (score == null) {
            return null;
        }
        for (Result target : array) {
            if (target.getScore() != null && target.getScore().intValue() == score.intValue()) {
                return target;
            }
        }
        return null;
    }
}
```

ではHaskellだとどう書けるでしょうか。もし目的の値が見つからなかった時に何を返せばいいでしょうか。Haskellはこういう場合、すなわち関数が返すべき値が無いような場合が存在する時、関数の返り値をMaybe型に包んで返すということをします。Maybe型とは

```haskell
data Maybe a = Nothing | Just a
```

と定義される型でその値は`Nothing (何も無い値)`か`Just a (存在してその値はa)`の二種類があります。早速ですが例を見て行きましょう。

```haskell
import Data.List

data Result = Result { name :: String
                     , score :: Int
                     }

array = [ Result {name="藤原", score=80}
        , Result {name="水橋", score=65}
        , Result {name="伊吹", score=20}
        , Result {name="八雲", score=100}
        , Result {name="散野", score=0}
        ]

main = do
    let target = find (\r -> score r == 100) array
    case target of
        Just r  -> putStrLn $ "満点は" ++ name r ++ "です"
        Nothing -> putStrLn "満点はいませんでした"
```

先ほどのJavaと同等なプログラムです。ポイントはtargetの型が単なる`Result`ではなく`Maybe Result`であるというところです。そのため`name`や`score`といった関数を直接使うことが**できず**、case文を用いて処理を分岐してやる必要があります。もし`name target`なんて書いた日にはコンパイラに怒られます。厳しいですがこれが愛なのです。

簡単な例でしたがHaskellではnullのような概念を使わずにMaybe型でうまくやっているというのがわかってもらえたでしょうか。

##まとめ
Haskellにはnullが無いし再代入できる変数もありません。だからといって使えない言語でもありません。簡単な例でしたがJavaと同等で負けず劣らないコードを書けることを見てきました。ツイートはプチ炎上しましたが多くの人がHaskellを知るきっかけになってもらえれば幸いです。最後に僕が言うまでもなくHaskellは多くの企業で採用され実際に使われている言語です。あとこの手の話が好きならこっちの記事もオススメです👉[【翻訳】Haskellのエンジニアは二流なのか？　（答えはノーである）](http://postd.cc/are-haskell-engineers-second-rate/)
