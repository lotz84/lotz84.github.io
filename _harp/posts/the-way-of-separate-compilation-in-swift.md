コマンドラインからswiftを分割コンパイルする方法を試行錯誤した結果を残しておきます

##ケース1

`a.swift`

```swift
func hello() {
    println("hello")
}
```

`main.swift`

```swift
hello()
```

```bash
$ swiftc a.swift main.swift
$ ./main
hello
```

###ポイント
* 実行するファイルはファイル名を`main.swift`にする
* 実行ファイルでないファイルはトップレベルにロジックを書かない

##ケース2

`a.swift`

```swift
func helloA1() {
    println("hello")
}

func helloA2() {
    helloB()
    println("hello")
}
```

`b.swift`

```swift
func helloB() {
    helloA1()
    println("hello")
}
```

`main.swift`

```swift
hello()
```

```bash
$ swiftc a.swift b.swift main.swift
$ ./main
hello
hello
```
