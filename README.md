# Lesxe - A Lisp

For my learning purpose.

- Interpreter
- Copy GC
- Tail call optimized

## THEME SONG

[![なまはげ/人間椅子](http://img.youtube.com/vi/CLoUY1kA4ZY/0.jpg)](http://www.youtube.com/watch?v=CLoUY1kA4ZY "")

## WHY?

CやアセンブリでSECDマシンを書いたことはあったけど、インタプリタはLispやJSでしか書いたことが無く、Cで書いた場合にどれぐらいVM方式と比べて楽なのか知っておきたかったので作った。

以下は個人的な感想です。

インタプリタの方がよかったところ

- ブートストラップ問題で頭がおかしくならない。VM方式でコンパイラをセルフホスティングするとき、マクロ展開あたりから自分が書いたコードに自信が持てなくなる
- コンパイラを書く必要がないので動き始めるまでが早い
- 開発初期に若干デバッグがしやすい。現在evalしているコードを一応知ることができるので。VMだとコンパイル前の情報を持たせるところから作りこまないといけない

VM方式の方が良かったところ

- Scheme等別のLispでコンパイラを書けば、開発初期にライブラリ用Lispコードが楽に書ける
- 最適化がしやすいしわかりやすい。特に末尾呼び出し最適化
- コンパイラで生成された大量のS式を見るのが楽しい

## FUTURE?

- Conservative Mark&Sweep GCを書いて移行してみて、どれぐらい楽になるのか（またはならないのか）知りたい
- 3impのStackVMをCで書いてみたい（ゼロからやったほうがマシかも）
- Lisp to x86コンパイラを書いてみたい（別プロジェクトになるかも）

## DEPENDENCY

- gcc
- make

### optional

- valgrind (for `make memcheck`)

## BUILD AND RUN

build

```
make
```

run repl

```
./bin/lesxe
```

run test

```
make test
```

## LICENSE

MIT License

