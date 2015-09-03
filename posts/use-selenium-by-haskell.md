---
title: HaskellでSeleniumを使ってみる＠勉強会
date: 2014-12-14
---

休日を利用して京都に帰って[【CAMPHOR- × サイボウズ】Selenium 勉強会](https://atnd.org/events/59820)に参加してきました。主催してくださったCAMPHOR-とサイボウズの皆さん、楽しい勉強会をありがとうございました。
勉強会ではハンズオン形式でSeleniumを学んだのですが最初のスライドに非公式だけどHaskellでも使えるとあったので挑戦してみました。単身でのHaskell参戦でしたがせっかくなのでたまった知見を書き起こしておこうと思います。

[Selenium](http://www.seleniumhq.org/)はプログラムから自動的にブラウザを操作することが出来るツールです。
WebサービスのUIテストに使われているようで勉強会では歴史的なとこからわかりやす解説してもらいました。

主にテストに使われているらしいのですが自動的にブラウザを操作することが出来るならもっと色々使えそうで夢が広がります。
そういえば友達がソシャゲの毎日ログインを自動化しようとしてたのでSeleniumをオススメできますね。その時はPhantomJSだけでゴリゴリ書いてたみたいですが…
FirefoxとかChromeとかのブラウザを使ってテストができるのもSeleniumのいいとこですね！

Seleniumが公式でサポートしている言語は[Document](http://www.seleniumhq.org/docs/)を見てみると[Java, C#, Python, Ruby, PHP, Perl, JS]のあたりのようです。これ以外にも非公式でライブラリが用意されていてかなり多くの言語から使うことができそうです。

さて肝心のHaskellからSeleniumを使う方法ですが「Haskell Selenium」でググると一番上に(2014/12/15調べ)出てくる[webdriver](http://hackage.haskell.org/package/webdriver)を使えば良さそうです.
PCはMacでブラウザはGoogleChrome、以下今回の記事で使うバージョン一覧です。

|name|version|
|:--|:--|
|[webdriver](http://hackage.haskell.org/package/webdriver)|0.6.0.3|
|[Selenium Server](http://www.seleniumhq.org/download/)|2.44.0|
|[ChromeDriver](https://sites.google.com/a/chromium.org/chromedriver)|2.13|

##環境構築
作業用ディレクトリを作ります。

```bash
$ mkdir selenium-test
$ cd selenium-test
```

Selenium ServerとChromeDriverをここにダウンロードしてきてください

```bash
$ tree .
.
├── chromedriver
└── selenium-server-standalone-2.44.0.jar
```

さっそくSelenium Serverを起動しておきましょう

```bash
$ java -jar selenium-server-standalone-2.44.0.jar
```

webdriverを導入します。cabalコマンドで一発です。

```bash
$ cabal install webdriver
```

10分ぐらいかかるのでお茶でも飲みましょう。

##Hello Selenium
環境構築が終わったらさっそくSeleniumを使っていきましょう！
webdriverの[README](https://github.com/kallisti-dev/hs-webdriver)に書いてあるコードをそのまま実行してみましょう。Chromeで使う想定なのでmyConfigの定義は修正しています。

```bash
$ vim hello.hs
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Test.WebDriver

myConfig :: WDConfig
myConfig = defaultConfig { wdCapabilities = (defaultCaps {browser = chrome})}

main :: IO ()
main = runSession myConfig $ do
  openPage "http://google.com"
  searchInput <- findElem (ByCSS "input[type='text']")
  sendKeys "Hello, World!" searchInput
```

```bash
$ runghc hello.hs
```

バックグラウンドでブラウザが起動して"Hello, World!"をググっていれば成功です！

コードを簡単に説明すると

```haskell
myConfig :: WDConfig
myConfig = defaultConfig { wdCapabilities = (defaultCaps {browser = chrome})}
```

はWebDriverの基本設定を表す関数です。[`WDConfig`](https://hackage.haskell.org/package/webdriver/docs/Test-WebDriver.html#t:WDConfig)はHostとかPortとかを設定するレコードになっています。今回変更しているのは`wdCapabilities`のところでこれは[`Capabilities`](https://hackage.haskell.org/package/webdriver/docs/Test-WebDriver.html#t:Capabilities)というレコードです。`Capabilities`はブラウザの種類を設定したりアラートを制御できるようにするかとかWebStorageを制御できるようにするかとか許可系の設定をするレコードになっています。今回はブラウザをChromeに設定しています。

```haskell
main :: IO ()
main = runSession myConfig $ do
```

`runSession`の型は`WDConfig -> WD a -> IO a`になっていて設定と実行手順を渡せば実行してくれる関数です。
Seleniumの命令は基本的にはWDモナドを返す関数なのでdo構文を使って命令を組み立てていきます。

```haskell
openPage "http://google.com"
```

まず<http://google.com>をブラウザで開きます。

```haskell
searchInput <- findElem (ByCSS "input[type='text']")
```

次にCSSセレクタで"input[type='text']"なる要素を取得して`searchInput`に束縛しています。

```haskell
sendKeys "Hello, World!" searchInput
```

最後に`searchInput`に"Hello, World!"と打って検索を実行しています。


##webdriver で使える命令
webdriverはSelenium公式ではないので(?)Seleniumのドキュメントに載ってる命令と違う名前の関数が多々あります。例えばさっきの例の`openPage`は公式のライブラリでは`get`と書く関数と同じです。眺めてみるとwebdriverの方が関数名はわかりやすくなっている気がします。
Seleniumのドキュメントを読んでだいたいどんな命令が使えるのかを把握してからwebdriverのHackageを読んで実際どういう関数で使えるのかを調べるのがいいかもしれませんね。
命令は[Test.WebDriver.Commands](http://hackage.haskell.org/package/webdriver/docs/Test-WebDriver-Commands.html)にだいたい載っています。以下によく使いそうな命令を列挙してみます。なお以下の命令表では全て型宣言の頭に`WebDriver wd =>`が付いているものとします

###ナビゲーション
|命令|説明|
|:--|:--|
|openPage :: String -> wd ()|指定されたURLのページを開く|
|forward :: wd ()|次へ進む|
|back :: wd ()|前に戻る|
|refresh :: wd ()|更新|

###ページ情報
|命令|説明|
|:--|:--|
|getCurrentURL :: wd String|開いてるページのURLを取得|
|getSource :: wd Text|開いてるページのソースを取得|
|screenshot :: wd ByteString|開いてるページのスクリーンショットを撮る|

###要素の検索
|命令|説明|
|:--|:--|
|findElem :: Selector -> wd Element|セレクタ(後述)から要素を取得|
|findElems :: Selector -> wd [Element]|セレクタから全ての要素を取得|
|findElemFrom :: Element -> Selector -> wd Element|セレクタから要素下の要素を取得|
|findElemsFrom :: Element -> Selector -> wd [Element]|セレクタから要素下の全ての要素を取得|

###要素へのアクション
|命令|説明|
|:--|:--|
|click :: Element -> wd ()|要素をクリック|
|submit :: Element -> wd ()|要素をsubmit|
|getText :: Element -> wd Text|要素のテキストを取得|

###要素へのキー入力
|命令|説明|
|:--|:--|
|sendKeys :: Text -> Element -> wd ()|要素にテキストを入力してEnter|
|clearInput :: Element -> wd ()|要素のテキストをクリア|

###要素の情報
|命令|説明|
|:--|:--|
|attr :: Element -> Text -> wd (Maybe Text)|要素の属性を取得|
|cssProp :: Element -> Text -> wd (Maybe Text)|要素のCSSを取得|
|elemPos :: Element -> wd (Int, Int)|要素の位置を取得|
|elemSize :: Element -> wd (Word, Word)|要素のサイズを取得|
|isSelected :: Element -> wd Bool|要素が選択されているかどうかを取得|
|isEnabled :: Element -> wd Bool|要素が有効かどうかを取得|
|isDisplayed :: Element -> wd Bool|要素が表示されているかどうかを取得|
|tagName :: Element -> wd Text|要素のタグ名を取得|
|activeElem :: wd Element|現在フォーカスされている要素を取得|

ざっと挙げて見ましたがかなり色々できそうですね！この他にも複数Windowを制御したりクッキーをいじったりもできるので一度[Test.WebDriver.Commands](http://hackage.haskell.org/package/webdriver/docs/Test-WebDriver-Commands.html)を読むことをおすすめします。

さて上に挙げた命令にちょくちょく出てくるSelectorですが重要なので定義を挙げておきます

```haskell
data Selector = ById Text
              | ByName Text
              | ByClass Text
              | ByTag Text
              | ByLinkText Text
              | ByPartialLinkText Text
              | ByCSS Text
              | ByXPath Text
              deriving (Eq, Show, Ord)
```

型構築子を見るとわかると思いますが、様々な方法で要素を選択する型ですね。
`ById "someid"` は大体 `document.getElementById("someid")`
`ByClass "someclass"` は大体 `document.getElementsByClassName("someclass")`
に対応してるわけです。さっきの`hello.hs`では`ByCSS`を使ってましたね！
中でも便利なのが`ByLinkText`でこれは特定のテキストのaタグを取ってくることができる便利関数です！ハンズオンでも使って盛り上がっていました。

##ググってリンクを踏んでみる
次の例としてGoogleで検索して1件目のリンクを踏んでURLを取得するシナリオを考えてみましょう。
Googleで検索して検索結果を利用する時の問題として検索結果が非同期で取得されるという事があります。ちょっと考えると特定のDOMが生成されるまで待てばいいことはわかるのですがwebdriverではどのように実装すればいいでしょう。答えを言ってしまうと、こういう時は[Test.WebDriver.Commands.Wait](http://hackage.haskell.org/package/webdriver/docs/Test-WebDriver-Commands-Wait.html)にある`waitUntil`を使います。

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Test.WebDriver
import Test.WebDriver.Commands.Wait

myConfig :: WDConfig
myConfig = defaultConfig { wdCapabilities = (defaultCaps {browser = chrome})}

main :: IO ()
main = do
  url <- runSession myConfig $ do
    openPage "http://google.com"
    searchInput <- findElem (ByCSS "input[type='text']")
    sendKeys "CAMPHOR" searchInput
    waitUntil 30 $ findElem (ById "search")
    search <- findElem (ById "search")
    links <- findElemsFrom search (ByPartialLinkText "CAMPHOR-")
    click $ head links
    url <- getCurrentURL
    closeSession
    return url
  print url
```

実行して`"https://camph.net/"`が返ってくれば成功です！
（ずっとトップに出て欲しいものです）

```bash
$runghc search.hs
"https://camph.net/"
```

コードを見て行きましょう。`"CAMPHOR"`で検索するところまでは`hello.hs`と同じですね。

```haskell
waitUntil 30 $ findElem (ById "search")
```

`waitUntil` はその名の通り条件が満たされるまで待つ関数です。

```haskell
waitUntil :: WDSessionState m => Double -> m a -> m a
```

第一引数にはタイムアウト時間を設定します。
第二引数には条件を記述するのですが

* 条件が満たされない時は`ExpectFailed`を投げ
* 条件が満たされた時は`ExpectFailed`を投げない

ような関数を渡します。`findElem`は指定されたセレクタが存在しない時に`NoSuchElement`の例外を投げるので条件として使えるわけです。

```haskell
search <- findElem (ById "search")
links <- findElemsFrom search (ByPartialLinkText "CAMPHOR-")
click $ head links
```

検索結果が表示されたらまず`id="search"`の要素を`search`に束縛します。次に`search`の下にあるリンクの要素でテキストに"CAMPHOR-"を含むものを取ってきて`links`に束縛します。`links`には条件に合う要素が全て束縛されているので`head`で最初の要素を取り出して`click`でクリックします。

```haskell
url <- getCurrentURL
closeSession
return url
```

リンクを飛んだらURLを取得して`closeSession`でブラウザを閉じてしまいます。
最後に`return url`で取得したURLを返却します。

```haskell
url <- runSession myConfig $ do
    ...
    return url
print url
```

全体として上記のようになっているので`return url`で返したURLは外側の`url`に束縛されてそれを`print`しています。

以上SeleniumをHaskellで使う簡単な方法を見てきました。Haskellで書くとSeleniumの操作がWDモナドの統一されたインターフェースで記述することができてかなり読みやすく書くことが出来ると思います。今後は勉強会で得たSeleniumの知識を活かしてソシャゲの自動化をどんどん進めていきたいです(｀･ω･´)
