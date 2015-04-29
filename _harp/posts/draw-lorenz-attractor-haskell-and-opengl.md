[前回の記事](http://lotz84.github.io/posts/draw-lotka-volterra-equation-using-haskell-and-opengl)を公開したところ、友達にどうせならアトラクタを持ってる方程式を書けと言われたので前回に引き続き今度は[ローレンツ方程式](https://www.wikiwand.com/ja/%E3%83%AD%E3%83%BC%E3%83%AC%E3%83%B3%E3%83%84%E6%96%B9%E7%A8%8B%E5%BC%8F)を描画してみることにしました。

![](http://i.gyazo.com/c0db8f56853d3358d1cc6c15b629a819.png)

今回はWASDでズームと回転が出来るようにしています。

```haskell
import Data.IORef
import Graphics.UI.GLUT
```

GLUTが必要なので入っていない場合は

```bash
$ cabal install GLUT
```

で予めインストールしておいてください☕

```haskell
type Point = (GLfloat, GLfloat, GLfloat)
type Line  = [Point]
```

型宣言がごちゃごちゃするのを防ぐために点と線の型を定義しておきます

```haskell
-- 初期値
start = (1.0, 1.0, 1.0)

-- ローレンツ方程式
lorenz :: Point -> Point
lorenz = \(x, y, z) ->
    let dt = 0.01
        p  = 10.0
        r  = 28.0
        b  = 8.0/3.0
    in  ( (\x' -> x + x' * dt) $ p * (y - x)
        , (\y' -> y + y' * dt) $ x * (r - z) - y
        , (\z' -> z + z' * dt) $ x * y - b * z
        )
```

ローレンツ方程式本体と初期値です。`lorenz`は点の情報を受け取ると微小時間だけ動いた次の点の情報を返します。微小時間は`dt = 0.01`と決めで実装してますが引数で取るように修正してもいいでしょう。

```haskell
main :: IO ()
main = do
    -- OpenGLの初期化
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    _window <- createWindow "Lorenz Attractor"
    -- 共有パラメータの設定
    rot <- newIORef 0
    sc  <- newIORef 0.02
    orbit <- newIORef [start]
    -- コールバックの設定
    keyboardMouseCallback $= Just (keyboardMouse rot sc)
    idleCallback $= Just (idle orbit)
    displayCallback $= display orbit rot sc
    -- 実行
    mainLoop
```

共有パラメータは`rot`は回転角を`sc`は拡大率を`orbit`は計算した軌道を保存しておくのに使います。

```haskell
idle :: IORef Line -> IdleCallback
idle orbit = do
    ps <- get orbit
    orbit $= (lorenz (head ps) : take 10000 ps)
    postRedisplay Nothing
```

待機時に実行される関数でローレンツ方程式の計算をしています。計算した値はorbitに保存しています。

```haskell
display :: IORef Line -> IORef GLfloat -> IORef GLfloat -> DisplayCallback
display orbit rot sc = do 
    -- バッファをクリア
    clear [ColorBuffer]
    -- 共有パラメータの取得
    r <- get rot
    s <- get sc
    ps <- get orbit
    -- 変換行列の初期化
    loadIdentity
    preservingMatrix $ do
        scale s s s
        rotate r $ Vector3 0 1 0
        -- 軌道の描画
        let mkVertex (x, y, z) = vertex $ Vertex3 x y z
        renderPrimitive LineStrip $ mapM_ mkVertex ps
    -- 表示
    swapBuffers
```

軌道を実際に描画する部分です。`preservingMatrix`はとても便利で行列のpushとpopと勝手にやってくれて現在の状態を切り出して中で自由に使うことができます。

```haskell
keyboardMouse :: IORef GLfloat -> IORef GLfloat -> KeyboardMouseCallback
keyboardMouse rot sc _key _state _ _ = do
    case _key of
        Char 'w' -> sc  $~! (*1.1)
        Char 'a' -> rot $~! (+5)
        Char 's' -> sc  $~! (*0.9)
        Char 'd' -> rot $~! (subtract 5)
        _        -> return ()
```

キーボード（とマウス）のイベントを制御する関数です。WASDが押されると対応するパラメータの値を変更しています。

実行するときは必ずコンパイルしてから実行してください。

```haskell
$ ghc Main.hs
$ ./Main
```

描画されたグラフを眺めていると手前と奥が表示されなくなっていることに気が付きました。調べるとカメラの設定でどこまで表示するかを調整できるようで`frustum`とか`perspevtive`とかをいじってたのですが結局直すことができませんでした… OpenGLの勉強を真面目にしたいと思います。

コード全体を以下に載せておきます。

----


```haskell
import Data.IORef
import Graphics.UI.GLUT

type Point = (GLfloat, GLfloat, GLfloat)
type Line  = [Point]

-- 初期値
start = (1.0, 1.0, 1.0)

-- ローレンツ方程式
lorenz :: Point -> Point
lorenz = \(x, y, z) ->
    let dt = 0.01
        p  = 10.0
        r  = 28.0
        b  = 8.0/3.0
    in  ( (\x' -> x + x' * dt) $ p * (y - x)
        , (\y' -> y + y' * dt) $ x * (r - z) - y
        , (\z' -> z + z' * dt) $ x * y - b * z
        )

main :: IO ()
main = do
    -- OpenGLの初期化
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    _window <- createWindow "Lorenz Attractor"
    -- 共有パラメータの設定
    rot <- newIORef 0
    sc  <- newIORef 0.02
    orbit <- newIORef [start]
    -- コールバックの設定
    keyboardMouseCallback $= Just (keyboardMouse rot sc)
    idleCallback $= Just (idle orbit)
    displayCallback $= display orbit rot sc
    -- 実行
    mainLoop

idle :: IORef Line -> IdleCallback
idle orbit = do
    ps <- get orbit
    orbit $= (lorenz (head ps) : take 10000 ps)
    postRedisplay Nothing

display :: IORef Line -> IORef GLfloat -> IORef GLfloat -> DisplayCallback
display orbit rot sc = do 
    -- バッファをクリア
    clear [ColorBuffer]
    -- 共有パラメータの取得
    r <- get rot
    s <- get sc
    ps <- get orbit
    -- 変換行列の初期化
    loadIdentity
    preservingMatrix $ do
        scale s s s
        rotate r $ Vector3 0 1 0
        -- 軌道の描画
        let mkVertex (x, y, z) = vertex $ Vertex3 x y z
        renderPrimitive LineStrip $ mapM_ mkVertex ps
    -- 表示
    swapBuffers

keyboardMouse :: IORef GLfloat -> IORef GLfloat -> KeyboardMouseCallback
keyboardMouse rot sc _key _state _ _ = do
    case _key of
        Char 'w' -> sc  $~! (*1.1)
        Char 'a' -> rot $~! (+5)
        Char 's' -> sc  $~! (*0.9)
        Char 'd' -> rot $~! (subtract 5)
        _        -> return ()
```
