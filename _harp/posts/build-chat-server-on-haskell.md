今日[GithubのHaskellのトレンド](https://github.com/trending?l=haskell)を見ていたら[jaspervdj/websockets](https://github.com/jaspervdj/websockets)が上がっていました。ソケット通信といえば[Socket.IO](http://socket.io/)な昨今ですがHaskellにも[socket-io](https://hackage.haskell.org/package/socket-io)というバインディングが存在します。しかし何度か使おうと思って挑戦したんですがまだまだレベルが低くて使い方がわからず…

今日見つけたwebsocketは例のコードも短く使いやすそうだったので勉強がてら簡単なチャットを作ってみました。

まず依存ライブラリをインストールします

```bash
$ cabal install websockets warp wai-websockets
```

インストールが終わるまでコーヒーでも飲んで待ちましょう☕

終わったら早速アプリを書いていきます！

`chat.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forever)
import Control.Exception (finally)
import Data.IORef
import Data.Text (Text)
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Application, responseFile)
import Network.Wai.Handler.WebSockets (websocketsOr)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

type Client = (Int, WS.Connection)

broadcast :: Text -> [Client] -> IO ()
broadcast msg = mapM_ (flip WS.sendTextData msg) . map snd

addClient :: WS.Connection -> [Client] -> ([Client], Int)
addClient conn cs = let i = if null cs then 0 else maximum (map fst cs) + 1
                    in  ((i, conn):cs, i)

removeClient :: Int -> [Client] -> ([Client], ())
removeClient i cs = (filter (\c -> fst c /= i) cs, ())

chat :: IORef [Client] -> WS.ServerApp
chat ref pending = do
    conn <- WS.acceptRequest pending
    identifier <- atomicModifyIORef ref (addClient conn)
    flip finally (disconnect identifier) $ forever $ do
        msg <- WS.receiveData conn
        conns <- readIORef ref
        broadcast msg conns
    where
    disconnect identifier = atomicModifyIORef ref (removeClient identifier)

app :: Application
app req respond = respond $ responseFile status200 [(hContentType, "text/html")] "index.html" Nothing

main :: IO ()
main = do
    let port = 3000
    let setting = Warp.setPort port Warp.defaultSettings
    putStrLn $ "Your server is listening at http://localhost:" ++ show port ++ "/"
    ref <- newIORef []
    Warp.runSettings setting $ websocketsOr WS.defaultConnectionOptions (chat ref) app
```

ドーン！！と一気に全部のコードを載せましたが少しずつ解説していきますｗ

まずmain関数ですがdo以降の最初の３行はWebサーバーのポートなど基本設定を行っています。次の`ref <- newIORef []`は接続してきたユーザーを管理するためのIORefを作っています。`ref`の型は`IORef [Client]`です。`Client`は上の方で

```haskell
type Client = (Int, WS.Connection)
```

と定義されており、識別子とコネクションの組になっています。

```haskell
Warp.runSettings setting $ websocketsOr WS.defaultConnectionOptions (chat ref) app
```

でいよいよサーバーを起動しています。`websocketsOr :: ConnectionOptions -> ServerApp -> Application -> Application`はWebSocketサーバーとWebサーバーを同時に建てる時に使う関数で`chat ref`がWebSocketサーバー、`app`がWebサーバーになっています。まず`app`を見てみましょう。

```haskell
app :: Application
app req respond = respond $ responseFile status200 [(hContentType, "text/html")] "index.html" Nothing
```

これはどんなリクエストが来ても`index.html`を返すだけのサーバーです。`index.html`は後で作っていきます。

```haskell
chat :: IORef [Client] -> WS.ServerApp
chat ref pending = do
    conn <- WS.acceptRequest pending
    identifier <- atomicModifyIORef ref (addClient conn)
    flip finally (disconnect identifier) $ forever $ do
        msg <- WS.receiveData conn
        conns <- readIORef ref
        broadcast msg conns
    where
    disconnect identifier = atomicModifyIORef ref (removeClient identifier)
```

これがチャットサーバーの本体です。`acceptRequest :: PendingConnection -> IO Connection`はクライアントからの接続を待つ関数で、クライアントが接続してきたら`atomicModifyIORef`を使って部屋情報のIORefにクライアントを登録しています。

```haskell
addClient :: WS.Connection -> [Client] -> ([Client], Int)
addClient conn cs = let i = if null cs then 0 else maximum (map fst cs) + 1
                    in  ((i, conn):cs, i)
```

`addClient`の実装はこのようになってて、識別子は最大値+1にしています。クライアントを登録したら`flip finally (disconnect identifier)`でユーザーが離脱した時に終了処理をすることを保証した後に`forever`を使って受け取ったメッセージをひたすらブロードキャストしています。

```haskell
broadcast :: Text -> [Client] -> IO ()
broadcast msg = mapM_ (flip WS.sendTextData msg) . map snd
```

ブロードキャストの関数は全てのクライアントにメッセージを送っているだけです。ユーザー離脱時の処理は

```haskell
removeClient :: Int -> [Client] -> ([Client], ())
removeClient i cs = (filter (\c -> fst c /= i) cs, ())
```

この`removeClient`を`atomicModifyIORef`で実行しています。

`index.html`

```markup
<!DOCTYPE>
<html>
    <head>
        <script src="//ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"></script>
    </head>
    <body>
        <form><input type="text"/></form>
        <div></div>
        <script>
            try {
              var ws = new WebSocket('ws://localhost:3000/');
            } catch (err) {
              console.error(err);
            }

            $("form").submit(function(){
                ws.send($('input').val());
                $('input').val('');
                return false;
            });

            ws.onmessage = function (msg) {
                $('div').prepend(msg.data + '<br>'); 
            }
        </script>
    </body>
</html>
```

`index.html`はこんな感じです。さっそく実行してみましょう。

```bash
$ runhaskell chat.hs
```

![](http://i.gyazo.com/502b6c72aea5244fa6764ce8395d096d.png)

別々にブラウザを開いてリアルタイムにチャットが出来てることが確認できました！

##参考にしたサイト
* [Haskellでwebsocketサーバを作る](http://qiita.com/asukamirai/items/522cc3c07d7d9ad21dfa)
* [Node.js + Socket.IO + jQuery で最小構成チャット](http://qiita.com/naga3/items/bdf6176537a5ac77a9b5)
