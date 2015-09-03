---
title: Haskell から MySQL を使う
date: 2014-12-01
---

ひょんなことからHaskellで簡単な掲示板を作る機会があったので
その時に覚えた[mysql-simple](https://hackage.haskell.org/package/mysql-simple)の使い方を書き留めておこうと思います。

##環境構築
テスト用のディレクトリとcabalファイルを作ってmysql-simpleを入れます

```bash
$ mkdir mysql-test && cd mysql-test
$ cabal sandbox init
```
mysql-test.cabal を作ります。これはライブラリの依存関係を記述するファイルです。

```bash
$ vim mysql-test.cabal
```

```markup
name: mysql-test
version: 0.0.0.1
build-type: Simple
cabal-version: >=1.20

executable app
  main-is: app.hs
  build-depends: base, mysql-simple
  ghc-options: -Wall
  default-language: Haskell2010
```

依存ライブラリをインストールしましょう。

```bash
$ cabal install --only-dependencies
```

実際にアプリケーションを書いていきます

```bash
$ vim app.hs
```

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple

main :: IO ()
main = do
  conn <- connect defaultConnectInfo
  [Only i] <- query_ conn "select 2 + 2"
  print (i :: Int)
```

実行します。このときMySQLサーバーが立ち上がってることを確認して下さい。

```bash
$ cabal run app
4
```

`4` が表示されれば成功です！

テスト用のDBを作っておきましょう。

```bash
$ mysql -uroot
mysql> CREATE DATABASE mysql_test;
mysql> USE mysql_test;
mysql> CREATE TABLE user (id int unsigned, name varchar(255), PRIMARY KEY (id));
```

##CRUD
###CREATE

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple

main :: IO ()
main = do
  conn <- connect defaultConnectInfo { connectDatabase = "mysql_test" }
  execute conn "INSERT INTO user VALUES (?, ?)" (1 :: Int, "a" :: String)
  return ()
```

実行すると確かにデータが挿入されているはずです

```bash
mysql> SELECT * FROM user;
+----+------+
| id | name |
+----+------+
|  1 | a    |
+----+------+
```

main関数を一行ずつ見て行きましょう
最初の行はMySQLとの接続をしています

```haskell
conn <- connect defaultConnectInfo { connectDatabase = "mysql_test" }
```

`connect`と`defaultConnectInfo`の情報を調べてみましょう。

```bash
$ ghci
Prelude> :m Database.MySQL.Simple

Prelude> :t connect
connect :: ConnectInfo -> IO Connection

Prelude> :t defaultConnectInfo
defaultConnectInfo :: ConnectInfo

Prelude> :i ConnectInfo
data ConnectInfo
  = ConnectInfo {connectHost :: String,
                 connectPort :: GHC.Word.Word16,
                 connectUser :: String,
                 connectPassword :: String,
                 connectDatabase :: String,
                 connectOptions :: [Database.MySQL.Base.Types.Option],
                 connectPath :: FilePath,
                 connectSSL :: Maybe Database.MySQL.Base.SSLInfo}

Prelude> defaultConnectInfo
ConnectInfo {connectHost = "localhost", connectPort = 3306, connectUser = "root", connectPassword = "", connectDatabase = "test", connectOptions = [CharsetName "utf8"], connectPath = "", connectSSL = Nothing}
```

ここまで見れば明らかですが[defaultConnectInfo](https://hackage.haskell.org/package/mysql-simple/docs/Database-MySQL-Simple.html#v:defaultConnectInfo)は[ConnectInfo](https://hackage.haskell.org/package/mysql-simple-0.2.2.4/docs/Database-MySQL-Simple.html#t:ConnectInfo)型でこれはMySQLに接続するために必要な情報を格納するレコードです。[connect](https://hackage.haskell.org/package/mysql-simple/docs/Database-MySQL-Simple.html#v:connect)はこれを引数にとってMySQLと接続するための関数です。`defaultConnectInfo`のままでは`test`のDBを見に行ってしまうのでレコード構文を使って先ほど作ったDBを見に行くように変えています。

```haskell
defaultConnectInfo { connectDatabase = "mysql_test" }
```

次の行は実際にデータを挿入しているところです

```haskell
execute conn "INSERT INTO user VALUES (?, ?)" (1 :: Int, "a" :: String)
```

mysql-simpleには[query](https://hackage.haskell.org/package/mysql-simple/docs/Database-MySQL-Simple.html#v:query)と[execute](https://hackage.haskell.org/package/mysql-simple/docs/Database-MySQL-Simple.html#v:execute)の２つのクエリを実行する関数があります。queryはSELECT文のように何か結果を返すようなクエリの実行に、executeはINSERTやUPDATEのような何も結果を返さないようなクエリの実行に使います。今回はINSERTなのでexecuteを使っています。
クエリの中の"?"は後の引数のタプルの要素がサニタイジングされて代入されます。クエリ中の"?"の個数とタプルの要素数は一致していないといけません。
`(1 :: Int, "a" :: String)`の部分は少し見にくいですが本質的には(1,"a")のタプルで、リテラルだと型がわからないので型注釈が入った形になっています。

最後の

```haskell
return ()
```

の行はmain関数の型を合わせるために入っています。

一気に十人ぐらいのデータを入れてみましょう。

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Database.MySQL.Simple

main :: IO ()
main = do
  conn <- connect defaultConnectInfo { connectDatabase = "mysql_test" }
  forM (zip [2..10] ['b'..]) $ \(n, c) -> do
    execute conn "INSERT INTO user VALUES (?, ?)" (n :: Int, [c])
  return ()
```

###READ
先ほど入れたデータを今度は読んでみましょう

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple

main :: IO ()
main = do
  conn <- connect defaultConnectInfo { connectDatabase = "mysql_test" }
  rs <- query_ conn "SELECT * FROM user"
  mapM_ putStrLn $ map show (rs :: [(Int, String)])
```

実行すると

```bash
$ cabal run app
(1,"a")
(2,"b")
(3,"c")
(4,"d")
(5,"e")
(6,"f")
(7,"g")
(8,"h")
(9,"i")
(10,"j")
```

ちゃんと表示されると思います。

[query_](https://hackage.haskell.org/package/mysql-simple-0.2.2.4/docs/Database-MySQL-Simple.html#v:query_), [execute_](https://hackage.haskell.org/package/mysql-simple-0.2.2.4/docs/Database-MySQL-Simple.html#v:execute_)はquery, executeとほとんど同じですが、クエリ中に"?"が無くタプルを引数としてとらない場合に使います。
実行結果は`(rs :: [(Int, String)])`のように正しく型付けしてやる必要があります。

```haskell
type User = (Int, String)
```

のようにしておけば`(rs :: [User])`のように書くことも出来ます。

###UPDATE, DELETE
ここまでくればUPDATEとDELETEは自然に書けると思います。

####UPDATE

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple

main :: IO ()
main = do
  conn <- connect defaultConnectInfo { connectDatabase = "mysql_test" }
  execute_ conn "UPDATE user SET name = 'aa' WHERE id = 1"
  return ()
```

####DELETE

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Database.MySQL.Simple

main :: IO ()
main = do
  conn <- connect defaultConnectInfo { connectDatabase = "mysql_test" }
  execute_ conn "DELETE FROM user WHERE id = 10"
  return ()
```

##簡単なアプリを作ってみる
最後にさっき作った`user`テーブルを読み書きするだけの簡単なアプリを作ってみます

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Database.MySQL.Simple

type User = (Int, String)

getConnection:: IO Connection
getConnection = connect defaultConnectInfo { connectDatabase = "mysql_test" }

mySelect :: IO ()
mySelect = do
  conn <- getConnection
  rs   <- query_ conn "SELECT * FROM user"
  mapM_ putStrLn $ map show (rs :: [User])

myInsert :: IO ()
myInsert = do
  putStrLn "Data: "
  input <- getLine
  let user = (read $ concat ["(", input, ")"]) :: User
  conn <- getConnection
  execute conn "INSERT INTO user VALUES (?, ?)" user
  return ()

myUpdate :: IO ()
myUpdate = do
  putStrLn "Data: "
  input <- getLine
  let (n, name) = (read $ concat ["(", input, ")"]) :: User
  conn <- getConnection
  execute conn "UPDATE user SET name = ? WHERE id = ?" (name, n)
  return ()

myDelete :: IO ()
myDelete = do
  putStrLn "ID: "
  input <- getLine
  let n = read input :: Int
  conn <- getConnection
  execute conn "DELETE FROM user WHERE id = ?" (Only n)
  return ()

main :: IO ()
main = do
  putStrLn "Command: "
  command <- getLine
  case command of
    "show"   -> mySelect >> main
    "insert" -> myInsert >> main
    "update" -> myUpdate >> main
    "delete" -> myDelete >> main
    "exit"   -> putStrLn "Bye." >> return ()
    _        -> putStrLn "Command: show, insert, update, deletem or exit" >> main
```

実際にコピペして実行してみてください。
以下のような感じで遊ぶことが出来ます

```haskell
$ cabal run app
Command:
show
(1,"aa")
(2,"b")
(3,"c")
(4,"d")
(5,"e")
(6,"f")
(7,"g")
(8,"h")
(9,"i")
Command:
insert
Data:
10, "haskell"
Command:
delete
ID:
8
Command:
show
(1,"aa")
(2,"b")
(3,"c")
(4,"d")
(5,"e")
(6,"f")
(7,"g")
(9,"i")
(10,"haskell")
Command:
exit
Bye.
```
