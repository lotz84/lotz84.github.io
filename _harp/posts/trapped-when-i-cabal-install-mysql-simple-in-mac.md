依存ライブラリである pcre-light のインストール時に`pcre.h`が無いと怒られる

<https://github.com/bos/mysql-simple/issues/15>

##対処法
まずは`pcre.h`をいれる

```bash
brew install pcre
```

次にcabalにpcreが入ってる場所を指定して cabal install する

```bash
$ cabal install mysql-simple --extra-include-dirs=/usr/local/include/
```

これで正常に入った
