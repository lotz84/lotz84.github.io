4月18日に京都で[CAMPHOR-](https://camph.net/)が主催する[Unityハンズオン勉強会](https://atnd.org/events/64386)に参加しました

Unityは何度も入門しては続かず入門しては続かずを繰り返しているのですが今回もめげずに再チャレンジしてみました。

遅刻してしまったので全部聞けたわけではないのですが、最初は丁寧なチュートリアルから始まり森のなかで人をWASDで動かし簡単なゲームを作るところまでやりました。

![](http://i.gyazo.com/fb26bcf6bca256c1db82d53ca2070a99.png)

備忘メモですが、簡単なサンドボックス環境を作るときは`GameObject > 3D Object > Terrain`を配置してカメラをいい感じの位置に移動してTerrainのInspectorにある筆のアイコン![](http://i.gyazo.com/6c26ffd5cc0ad24f59aa74fe808e8bcc.png)をクリック。先に`Assets > Import Packages > Environment`からアセットをインポートしておくとTextureとして草原が選べるのでこれを選択。次に![](http://i.gyazo.com/d7e2afc6c57cf73df0e239e45ca896c4.png)ボタンを押してSceneのTerrainをクリックしてなぞると山が出来上がります。あとは山で囲んでしまってカメラを中に入れれば遊び場の出来上がり！

あと人物のモデルを利用するときは`Assets > Import Packages > Characters`からアセットをインポートしてProjectパネルの`Assets > Standard Assets > Characters > ThirdPersonCharacter > Prefabs > ThirdPersonController.prefab`をドラッグ&ドロップすればWASDでしかもアニメーション付きで移動できるモデルが簡単に使えます

お昼は[たく味](http://tabelog.com/kyoto/A2601/A260302/26002471/)でラーメンを食べ、午後からはいよいよハッカソンです！

自分は[往年のWindowsに付属していたピンボール](https://www.google.co.jp/search?q=space+cadet+windows&tbm=isch)を再現してみようと頑張りました。出来上がったのがこれ

![](http://i.gyazo.com/0f328c6334b46f970af47370a8d4984a.png)

惜しい！！実行するとちゃんとタマも飛んでいきます。苦労した点は上方にあるカーブを全てCubeで作ったところでしょうか… 本当ならBlenderなどでモデリングできたらいいのでしょうか。Unityまだまだ難しいです。
