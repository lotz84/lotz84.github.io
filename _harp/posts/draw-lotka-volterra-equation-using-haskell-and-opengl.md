先日「ロトカ＝ヴォルテラ方程式知ってる？」と聞かれて人工生命などに熱中していた懐かしい記憶が蘇りました。そう言えば昔はPythonで相図を書いた記憶があったのですが今だったらとHaskellを使ってロトカ＝ヴォルテラ方程式の時間発展を描画してみました。描画にはOpenGLを使いました。

![](http://i.gyazo.com/5095d0a1b2e6a73925272fe3944650a4.png)

OpenGLを使ってゴニョゴニョするのに[GLUT](https://hackage.haskell.org/package/GLUT)を使っています。OpenGL Utility ToolkitのHaskellバインディングです。予めインストールしておきましょう。

```bash
$ cabal install GLUT
```

ロトカ＝ヴォルテラ方程式は[wiki](https://www.wikiwand.com/ja/%E3%83%AD%E3%83%88%E3%82%AB%EF%BC%9D%E3%83%B4%E3%82%A9%E3%83%AB%E3%83%86%E3%83%A9%E3%81%AE%E6%96%B9%E7%A8%8B%E5%BC%8F)のものをそのまま使用しました。あとHaskellでOpenGLを使うときは`runhaskell`ではなく`ghc`でちゃんとコンパイルしてから実行しないので気をつけてください。これなんで何でしょう。以下のコードは完全なコードなのでコピペして試すことができます。

```haskell
import Data.IORef
import Graphics.UI.GLUT

type Line = [(GLfloat, GLfloat)]

-- 計算用パラメータ
dt    = 0.01
alpha = 5.0
beta  = 3.0
gamma = 2.0
delta = 2.0

start = (1.0, 1.0)

main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    _window <- createWindow "Lotka-Volterra Equation"
    orbit <- newIORef [start]
    idleCallback $= Just (idle orbit)
    displayCallback $= display orbit
    mainLoop

idle :: IORef Line -> IdleCallback
idle orbit = do
    ps <- get orbit
    let (x, y) = head ps
    let nx = (\x' -> x + x' * dt) $ x * (alpha - beta * y)
    let ny = (\y' -> y + y' * dt) $ (negate y) * (gamma - delta * x)
    orbit $= ((nx, ny) : take 500 ps)
    postRedisplay Nothing

display :: IORef Line -> DisplayCallback
display orbit = do 
    clear [ColorBuffer]
    ps <- get orbit
    let scale = 0.5
    let (ox, oy) = (1.2, 2.1)
    renderPrimitive Lines $
        mapM_ (\(x, y) -> vertex $ Vertex3 ((x - ox) * scale) ((y - oy) * scale) 0) ps
    swapBuffers
```

簡単に解説していきます。

```haskell
main :: IO ()
main = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    _window <- createWindow "Lotka-Volterra Equation"
    orbit <- newIORef [start]
    idleCallback $= Just (idle orbit)
    displayCallback $= display orbit
    mainLoop
```

最初の方はほとんどお決まりの処理です。大事なのは`idleCallback`と`displayCallback`でそれぞれ待機時、描画時に呼ばれる関数を指定します。まず待機時に呼ばれる関数を見て行きましょう。

```haskell
idle :: IORef Line -> IdleCallback
idle orbit = do
    ps <- get orbit
    let (x, y) = head ps
    let nx = (\x' -> x + x' * dt) $ x * (alpha - beta * y)
    let ny = (\y' -> y + y' * dt) $ (negate y) * (gamma - delta * x)
    orbit $= ((nx, ny) : take 500 ps)
    postRedisplay Nothing
```

この関数は毎フレーム呼ばれるのでここでロトカ＝ヴォルテラ方程式の計算をしています。orbitというIORefを用意してここに計算した軌道を貯めていっています。`postRedisplay`を呼ぶと`displayCallback`が実行されます。

```haskell
display :: IORef Line -> DisplayCallback
display orbit = do 
    clear [ColorBuffer]
    ps <- get orbit
    let scale = 0.5
    let (ox, oy) = (1.2, 2.1)
    renderPrimitive Lines $
        mapM_ (\(x, y) -> vertex $ Vertex3 ((x - ox) * scale) ((y - oy) * scale) 0) ps
    swapBuffers
```

ここではorbitの軌道を実際に描画しています。実は`main`でDisplayModeにDoubleBufferedを指定していたのでダブルバッファリングを使うことができます。この場合`swapBuffers`を呼べばバッファの内容が描画されます。

だいたいこんな感じです。何も難しいことはしていません。実際に実行すると軌道がぐるぐる動くのが見れて楽しいと思います。発散するのは計算誤差からでしょうか・・・
