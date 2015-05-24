みなさんHTMLテンプレートエンジンは使っていますか？ 
[FreeMarker](http://freemarker.org/), [Haml](http://haml.info/), [Slim](http://slim-lang.com/), [Jade](http://jade-lang.com/), [Jinja2](http://jinja.pocoo.org/docs/dev/), [Hamlet](https://hackage.haskell.org/package/shakespeare) などなど世の中には言語やWAFに合わせていろんなテンプレートエンジンが存在しますよね…

![](http://i.gyazo.com/01402ecefe6c8b85e532aeeab02841a9.png)

自分はJadeが好きでnode.jsでアプリを作った時は必ず使っています。
どのテンプレートエンジンも同じですがいくら文法を元の言語に似せていると言ってもベースはHTMLなのでどうしても独自の記法を使わざるを得ません。
あれ、こういう時Jadeでどう書くんだっけ？と疑問に思ったらどんなにJavaScriptに詳しくてもJadeのドキュメントを引っ張ってこないといけません。

HaskellのHTMLテンプレートエンジンはHamletがたぶん有名だと思うんですが、これにも同じ問題があります。**もう新しくテンプレートエンジンの独自記法は覚えたくないんです！！**

そんなことを考えながらネットの海を彷徨っていると[Lucid](https://hackage.haskell.org/package/lucid)というHackageを見つけました。例えばeRubyはHTMLにRubyを埋め込みますが、これは発想が逆でHaskellの中でHTMLを作ってしまいます！何のことはなくてモナドで作ったHTML構築用のEDSLを提供してくれます。もう新しい文法を学ばなくていいんです！EDSL最高！！

早速ですがどんな感じで書けるのかJadeと比較してみましょう

```markup
doctype
html
    head
        title Hello Jade!
        link(rel="stylesheet", type="text/css", href="/css/main.css")
    body
        h1 Jade Template Engine
        p Lorem ipsum dolor sit amet, consectetur adipiscing elit,
        script(src="/js/main.js")
```

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Lucid
import Data.Text

indexHtml :: Html ()
indexHtml = do
    -- ここからJadeと対応する
    doctype_
    html_ $ do
        head_ $ do
            title_ [] "Hello Lucid!"
            link_  [rel_ "stylesheet", type_ "text/css", href_ "/css/main.css"]
        body_ $ do
            h1_ [] "Lucid"
            p_  [] "Lorem ipsum dolor sit amet, consectetur adipiscing elit,"
            script_ [src_ "/js/main.js"] empty
    -- ここまで
```

どうですか？importや型宣言がある分Haskellの方が少し長くなっていますが、Haskellに慣れてさえいればJadeとさほど変わらない可読性があるはずです。むしろそれ以上かもしれません！例えばbodyの中身以外をテンプレートとして使いまわせるようにしましょう。Jadeの場合はどうしますか？Jadeの場合はextendsとblockを使って実現しますね。具体的な使い方は[ドキュメント](http://jade-lang.com/reference/inheritance/)を追ってくださいｗではHaskellの場合はどうするでしょうか。ただ関数として切り出せば良いんです！やり方はHaskellさえ知っていればとても簡単でしょう。

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Lucid
import Data.Text

indexHtml :: Html ()
indexHtml = layoutHtml $ do
    h1_ [] "Lucid"
    p_  [] "Lorem ipsum dolor sit amet, consectetur adipiscing elit,"
    script_ [src_ "/js/main.js"] empty

layoutHtml :: Html () -> Html ()
layoutHtml content = do
    doctype_
    html_ $ do
        head_ $ do
            title_ [] "Hello Lucid!"
            link_  [rel_ "stylesheet", type_ "text/css", href_ "/css/main.css"]
        body_ content
```

layoutHtml がテンプレートです！普通のHaskellの関数ですね。それではテンプレートに値を渡す方法はわかりますか？if文を書く方法はどうでしょう？コメントの書き方は？リストの中身をイテレートする方法は？？全部Haskellのやり方で出来ます！だってただのEDSLなのですから。

とはいえLucidもドキュメントを読む必要はあります。どんな関数があるのかとかどういう型クラスが提供されているのかとかです。ただHTMLタグとほとんど一対一に対応した関数が提供されているので探すのに困ることはありません。関数名の後ろにはアンダースコアがついてるので`id`と`id_`が衝突することもありません。

新しい言語で新しくWebアプリを作るたびに新しいHTMLテンプレートの記法を覚えていた今までは何だったのでしょうか。これからはEDSLで頭を切り替えることなくHTMLも作っていけそうです。実はLucid以外にもHTMLを構築するライブラリはいくつかあって一番有名なのがyesodでも使われている[blaze-html](http://hackage.haskell.org/package/blaze-html)だと思います。しかしこれは関数名や演算子名がbaseと衝突して修飾子付インポートせざるを得ず使いにくくなってしまう印象があります。その辺の比較とLucidを作った経緯が作者の[ブログ記事](http://chrisdone.com/posts/lucid)にあります。

最後にローカルでサーバーを建ててHTMLを返してくれる簡単なアプリの完全なコードを載せて終わります。  
ぜひ手元で動かしてみてください！

```bash
$ cabal install wai warp lucid
```

必要なHackageは上記のとおりです。

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Lucid
import Data.Text

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types

main :: IO ()
main = do
    putStrLn "Your server is listening at http://localhost:3000/"
    run 3000 $ \_ respond ->
        respond $ responseLBS status200 [] $ renderBS indexHtml

indexHtml :: Html ()
indexHtml = layoutHtml $ do
    h1_ [] "Lucid"
    p_  [] "Lorem ipsum dolor sit amet, consectetur adipiscing elit,"
    script_ [src_ "/js/main.js"] empty

layoutHtml :: Html () -> Html ()
layoutHtml content = do
    doctype_
    html_ $ do
        head_ $ do
            title_ [] "Hello Lucid!"
            link_  [rel_ "stylesheet", type_ "text/css", href_ "/css/main.css"]
        body_ content
```
