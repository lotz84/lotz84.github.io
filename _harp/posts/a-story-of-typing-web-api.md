みなさん型付けしていますか？  
Haskellのような柔軟な型に慣れてくると何でもかんでも型で表現できないと気がすまなくなってきますよね💃

今日はservantというWeb Application Framework(WAF)を紹介します。  
特徴として[チュートリアル](http://haskell-servant.github.io/tutorial/)では以下の4つが挙げられています

* 簡潔である
* 汎用的・柔軟性がある
* 関心の分離
* 型安全である

簡潔に書けたり汎用的であることはWAFであれば当然持っていて欲しい特徴でありますが、**関心の分離**と**型安全である**というのはどういうことでしょうか？

**関心の分離**というのは何と何を分離しているかというと、APIのリクエストとレスポンスに関する処理と内部のロジックを分離しようという話です。例えば一般的なWAFであればリクエストで送られてきたbodyをパースして目当てのデータを作る必要があったり、ヘッダーに含まれるデータを検索して取ってきたり、データをシリアライズしてレスポンスに詰めてやったりする必要がありますが、servantではそういったある意味冗長な部分もWAFが担当してくれるので書く必要がありません。

なぜそれが可能なのかというと**型安全である**ことに関係しています。servantはそのAPIがどういうものであるのかを型によって記述します。ルーティングはどうなっているのか、どういうパラメータが送られてくるのか、どういうレスポンスが返されるのか、などをすべて型で表現します。例えば以下のようにです。

```haskell
             -- POST /todo/:id
type TodoAPI = "todo" :> Capture "id" Int :> ReqBody '[JSON] Todo :> Post '[JSON] [Todo]
```

コメントは見やすいようにつけているだけです。この型をなんとなく読んでみるとと`TodoAPI`では**/todo/:id**にPOSTでbodyにJSON形式のTodoを入れてリクエストを送るとJSON形式でTodoのリストが返ってくるという風に読めそうです。Todoは独自に定義したデータ構造です。さらにこのAPIの実装例を見てみましょう。

```haskell
server :: Server TodoAPI
server = postTodo
    where
    postTodo targetId todo = return todoList
```

postTodo の型は`postTodo :: Int -> Todo -> EitherT ServantErr IO [Todo]` のようになっててAPIの一部であることを意識させない普通の関数になっています。戻り値の型は少し複雑ですがServantのエラーハンドリングを無視すればほとんど`IO [Todo]`に等しいでしょう。このように型に十分な情報を持たせることでリクエストのパースなどの冗長な処理を無くし、今までのWAFには無かったような簡潔な実装を実現しているのです。

##実際に使ってみる
servantの感触を掴んでもらったところで早速具体的なWebサービスを作ってみましょう！今回はよくある簡単なTodoアプリを作っていきます。

まずはTodoのリストを返すAPIを作ってみましょう

```haskell
{-# LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}

import Servant
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp

data Todo = Todo
    { todoId    :: Int
    , todoDone  :: Bool
    , todoTitle :: String
    }

instance ToJSON Todo where
    toJSON todo = object [ "id"    .= todoId    todo
                         , "done"  .= todoDone  todo
                         , "title" .= todoTitle todo
                         ]

             -- GET /todo/all
type TodoAPI = "todo" :> "all" :> Get '[JSON] [Todo]

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

server :: Server TodoAPI
server = todoAll
    where
    todoAll = return [Todo 1 False "Sleeping", Todo 2 True "Dinner"]

main :: IO ()
main = do
    putStrLn "Listening on port 3000"
    run 3000 $ serve todoAPI server
```

まず`Todo`をレスポンスで返すために`ToJSON`のインスタンスにしてやる必要があります。そしてservant唯一の冗長な記述なのですが`todoAPI`のようにProxyのデータを作ってやる必要があります。これがないと`serve`の時にうまく型をあわせることができません。

実行して <http://localhost:3000/todo/all> にアクセスするとTodoのリストがJSON形式で返ってきてるのがわかると思います。

次にTodoの一覧をWebページにちゃんと表示してみましょう。差分だけ書いていきます

```haskell
import Servant.HTML.Lucid
import Data.Text (empty)
import Lucid

             -- GET /
type TodoAPI = Get '[HTML] (Html ())
             -- GET /todo/all
          :<|> "todo" :> "all" :> Get '[JSON] [Todo]
             -- static files /public/*
          :<|> "public" :> Raw

server :: Server TodoAPI
server = index
    :<|> todoAll
    :<|> serveDirectory "public"
    where
    index   = return indexHtml
    todoAll = return [Todo 1 False "Sleeping", Todo 2 True "Dinner"]

indexHtml :: Html ()
indexHtml = do
    doctype_
    html_ $ do
        head_ $ title_ [] "Servant Todo Example"
        body_ $ do
            h1_ [] "Servant Todo Example"
            table_ [id_ "todo-list"] ""
            script_ [src_ "//ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js"] empty
            script_ [src_ "/public/main.js"] empty

-- 以下同じ
```

`TodoAPI`にエンドポイントが2つ追加されています。複数のエンドポイントを持つAPIは型の定義を`:<|>`を使って結合していけば大丈夫です。まず`Get '[HTML] (Html ())`です。これはルート`/`にアクセスした時になんらかのWebページを返すことを表しています。`HTML`はservant-lucidのパッケージの中に入っている型です。ここではHTMLをLucidを使って作っています。`indexHtml`がそれです。

HTMLの中で`/public/main.js`にJavaScriptのファイルを読みに行っています。これを配信するために`"public" :> Raw`という型を使っています。こうすることで**/public/(path)**でアクセスされた時にpathのファイルを直接配信することができます。今のディレクトリ構造は以下のようにしています。

```bash
$ tree .
.
├── Main.hs
└── public
    └── main.js
```

`main.js`の中身です。

```javascript
var updateTodoList = function(list){
    $("#todo-list .todo-item").remove();
    list.sort(function(a, b){return a.id - b.id;})
    list.forEach(function(todo){
        var doneCheckbox = $("<input />")
                           .attr("type", "checkbox")
                           .prop("checked", todo.done);
        var deleteButton = $("<button />")
                           .text("Delete");
        var tr = $("<tr />")
                 .addClass("todo-item")
                 .append($("<td />").text(todo.id))
                 .append($("<td />").html(doneCheckbox))
                 .append($("<td />").text(todo.title))
                 .append($("<td />").html(deleteButton));
        $("#todo-list").append(tr);
    });
}

$.get("/todo/all", updateTodoList);
```

実行して <http://localhost:3000/> にアクセスしてみてください。Todoの一覧が表示されているはずです！

それではTodoを自分で登録できるようにしてみましょう。

```haskell
import Data.Text (empty, unpack)
import Data.IORef
import Control.Monad.IO.Class (liftIO)

instance FromFormUrlEncoded Todo where
    fromFormUrlEncoded form = let maybeTodo = do
                                      let id' = maybe (-1) (read . unpack) $ lookup "id" form
                                      let done   = maybe False (=="on") $ lookup "done" form
                                      title <- lookup "title" form
                                      return $ Todo id' done (unpack title)
                              in  maybe (Left "Todo parse error from Form") Right maybeTodo
             -- GET /
type TodoAPI = Get '[HTML] (Html ())
             -- GET /todo/all
          :<|> "todo" :> "all" :> Get '[JSON] [Todo]
             -- POST /todo
          :<|> "todo" :> ReqBody '[FormUrlEncoded] Todo :> Post '[JSON] [Todo]
             -- static files /public/*
          :<|> "public" :> Raw

server :: IORef [Todo] -> Server TodoAPI
server todosRef = index
             :<|> todoAll
             :<|> createTodo
             :<|> serveDirectory "public"
    where
    index   = return indexHtml
    todoAll = liftIO $ readIORef todosRef
    createTodo todo = do
        todos <- liftIO $ readIORef todosRef
        let nextId = if null todos
                         then 1
                         else (+1) . maximum . fmap todoId $ todos
        let todos' = todo {todoId = nextId} : todos
        liftIO $ writeIORef todosRef todos'
        return todos'

main :: IO ()
main = do
    putStrLn "Listening on port 3000"
    todosRef <- newIORef []
    run 3000 $ serve todoAPI (server todosRef)

indexHtml :: Html ()
indexHtml = do
    doctype_
    html_ $ do
        head_ $ title_ [] "Servant Todo Example"
        body_ $ do
            h1_ [] "Servant Todo Example"
            table_ [id_ "todo-list"] ""
            form_ [id_ "todo-form", method_ "POST", action_ "/todo"] $ do
                input_ [type_ "text", name_ "title"]
                input_ [type_ "submit", value_ "Add"]
            script_ [src_ "//ajax.googleapis.com/ajax/libs/jquery/2.1.4/jquery.min.js"] empty
            script_ [src_ "/public/main.js"] empty

-- 以下同じ
```

FormでTodoを送信するので`FromFormUrlEncoded`のインスタンスにしています。登録されたTodoはIORefで管理していて`todoAll`はその中身を返すように修正しました。

```javascript
$("#todo-form").on("submit", function(e){
    e.preventDefault();
    var $form = $(this);
    $.ajax({
        url: $form.attr('action'),
        type: $form.attr('method'),
        data: $form.serialize()
    })
    .then(updateTodoList);
});

-- 以下同じ
```

これで実行すると自分でTodoをどんどん追加することが出来るはずです！

あとはTodoの状態をあとから変更できるようにしたり、Todoを削除したりできるようにしたいですね。それらを実装したものを[lotz84/servant-todo-example](https://github.com/lotz84/servant-todo-example)に置いておきましたので是非cloneしてさわってみてください。
