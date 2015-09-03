---
title: HaskellでCtrl-Cを制御する
date: 2015-04-26
---

SIGINT等のシグナルの扱い方です。

##tl;dr
[System.Posix.Signals](https://hackage.haskell.org/package/unix/docs/System-Posix-Signals.html)を使います

----

`Ctrl-C`が押されたらクロージングの処理を伴って終了するプログラムを書いてみます。

```haskell
import Control.Concurrent
import System.Posix.Signals

main :: IO ()
main = do
    tid <- myThreadId
    let handler = do
            putStrLn "goodbye!"
            killThread tid
    installHandler keyboardSignal (Catch handler) Nothing

    let loop n = do
            putStr $ show n ++ ", "
            threadDelay 1000000
            loop (n+1)
    loop 0
```

実行してみましょう

```bash
$ runhaskell Main.hs
0, 1, 2, 3, 4, 5, ^Cgoodbye!
Main.hs: thread killed
$
```

ちゃんとgoodbyeと出力されて終了しました！`killThread`は例外を伴って終了するので`Main.hs: thread killed`というメッセージが出てしまっています。もし気になるなら例外を握りつぶすか`MVar`を使って終了を監視する仕組みを作るといいでしょう。

肝心のSignalを制御する関数は`installHandler :: Signal -> Handler -> Maybe SignalSet -> IO Handler`です。`Signel`には制御するSignal、`Handler`にはSignalが投げられた時の処理、`Maybe SignalSet`には処理時にブロックする他のSignalを指定します。

`Signal`の値は予め用意されていて

|Function           |Signal |
|:------------------|:------|
|keyboardSignal     |SIGINT |
|keyboardStop       |SIGTSTP|
|keyboardTermination|SIGQUIT|
|openEndedPipe      |SIGPIPE|

と言った感じです。

`Handler`は`Default`にすれば標準の動作を、`Ignore`にすれば何もしなくなり、`Catch (IO ())`で処理を記述すればSignalが投げられた時の処理を記述することができます。上の例では`handler`という関数にメッセージの表示とメインスレッドの停止の処理を書いて`Handler`として渡していました。

簡単に解説しましたがまだまだ色々な使い方ができるので是非一度[System.Posix.Signals](https://hackage.haskell.org/package/unix/docs/System-Posix-Signals.html)を読んでみてください(^^)
