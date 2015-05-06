世の中はゴールデンウィークですね！今日は最終日です！！  
GWは自由な時間がいっぱいあるのでたくさん勉強ができますね！（イヤッホー！）  
というわけで暇な時間を持て余して[*Haskell by Example*](http://lotz84.github.io/haskellbyexample/)というコンテンツを作りました。(はてブ、スター、プルリクください！)

[![](http://i.gyazo.com/bbfd8bf6052376e3adc572f4dcc64bad.png)](http://lotz84.github.io/haskellbyexample/)

知ってる人も多いと思いますが[要出典]、もともと[*Go by Example*](https://gobyexample.com/)という[Go](https://golang.org/)をExampleを通して学んでいこうという素晴らしいサイトがあって、Haskell by Exampleはそれを出来るだけそのまま[Haskell](https://www.haskell.org/)に翻訳したものです。

これを作った狙いはいくつかあって

* Haskell の理論的な入門はあっても実践的な入門が少ない
* Go言語が流行ってるっぽいのでその界隈にHaskellも認知してもらう
* まだHaskellあんまり書けないので自分の勉強になると思った(GWなので！)

と、そんな感じです。

もともとGoを勉強してた時、[A Tour of Go](https://tour.golang.org/)をひと通りやって次にひたすら*Go by Example*を写経していました。*Go by Example*は適度な大きさで多様なトピックのコードが散らばってるので写経するととてもいい勉強になります。座学ではなく動くコードを書いて学ぶようなコンセプトだったので写経が終わったはあとすぐにGoを使ってコードを書き始めることができました。

Haskellは研究でも使われている言語なので入門となるとどうしても理論中心になってしまう気がするのですが、自分は普通に仕事でも使いたいと思ってるのでそういう人のためにも読んですぐ書けるようになるような入門が必要だと思いました。そんなこんなで昔お世話になった*Go by Example*をHaskellで書きなおそうと思ったわけです。

最初はHaskellだってなんでも書けるんだぞー！と思って作ってたのですが当然言語には向き不向きがあるわけでGoの良いところもあらためて勉強になりました。やっぱり非同期強い！

まずはSelect文。これはあらためて別の記事でも書こうと思ってるのですが、GoのSelect文に相当する機能がHaskellにはなくて無理やり自作しました。ネットで検索すると[STMで実装する方法](http://stackoverflow.com/questions/5879128/a-way-to-form-a-select-on-mvars-without-polling)とかも出てきましたが結局ひたすらMVarをtryする方法で実装しました。時間があれば他の方法も試してみたいです。とにかくHaskellでは以下のように書けるようになりました。

```haskell
select [ Case messages $ \msg -> putStrLn $ "received message " ++ msg
       , Case signals  $ \sig -> putStrLn $ "received signal " ++ show sig
       , Default $ putStrLn "no activiry"
       ] 
```

元のSelect文そっくりですね(๑´ڡ`๑)

次にatomicです。Haskellでは代わりにSTMを使ってみましたが、失敗でした／(^o^)＼。atomic が low-level なのに対してSTMは high-level ですからそもそも引き合いに出すのもおかしいと思うのですが、Goの例では50スレッドでひたすら1を毎回足すみたいな処理をやっていてそれをSTMでやろうとしたものですからたちまちメモリが食いつぶされてしまいました。なので毎回スレッドを眠らせるなどして何とか動くようにしましたが実行効率はかなり悪いですね… 普通にwithMVarとかで書いたほうがいいのだろうかとか思いつつ結局直さないままになっています。誰か助けて／(^o^)＼

あとChan。GoのChanはclose出来るんですね！カッコイイ！[HaskellのChan](http://hackage.haskell.org/package/base/docs/Control-Concurrent-Chan.html)は無限長のチャネルを作ったらあとはひたすら読み書きするだけで閉じることができません… 閉じれないと何が不便かというともうデータが来ないという時点がわからないので`getChanContents`を使ってリストとして取り出しても無限長リストになってるし工夫しないと永遠に終わらない処理しか書けなくなってしまいます。なので`ClosableChan`みたいなデータ構造を作って開閉出来るようにすればいいと思うのですがめんどくさくなって放置しました…

あとTime。いっぱい型あるし小回りがきかなくて苦労しました…

なんだか愚痴っぽくなってしまいましたが読んでて分かる通り自分の無知と怠惰の結果な気もするのでもう少しがんばって修正しようと思います(^_^;)

あと当然HaskellにできてGoにできないこともたくさんあって例えば正規表現は書きませんでしたがHaskellにはParsecという強力なパーサコンビネータがあります。STMもちゃんと使えばとても便利なものですし、MonadもLensもGoに無いから書いてないだけで必ず学ぶべきトピックだと思います。なので今後*Go by Example*からはみ出てHaskell特有のコードを書いていくこともしていきたいですね。

最後に、見たらすぐわかると思うのですが*Haskell by Example*の方には説明文がありません。理由は単純で僕が英語できないからです／(^o^)＼もしも反響が大きくてGithubのスターが1000とか超えたらその時は英会話教室とかに通って英語勉強して説明もつけようと思いますｗ

以上いろいろ課題はあるのですが、今後もしばらく運用していくつもりなので[*Haskell by Example*](http://lotz84.github.io/haskellbyexample/)をよろしくお願いしますm(__)m  
またちょっとした修正とか気になったとことか俺が実装しなおしてやったよとかありましたらIssue/PullRequestを遠慮無くお願いします！  
[lotz84/haskellbyexample](https://github.com/lotz84/haskellbyexample)  
日本語でも大丈夫ですのでー！  
