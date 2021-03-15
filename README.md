# Lesxe - A Lisp

For my learning purpose.

- Interpreter
- Copying GC
- Tail call optimized

## THEME SONG

[![なまはげ/人間椅子](http://img.youtube.com/vi/CLoUY1kA4ZY/0.jpg)](http://www.youtube.com/watch?v=CLoUY1kA4ZY "")

## FUTURE?

- 3impのStackVMをCで書いてみたい（ゼロからやったほうがマシかも）
- Lisp to x86コンパイラを書いてみたい（別プロジェクトになるかも）

## SUSPENDED

- Conservative Mark&Sweep GCを書いてみる→ブランチconservative-gcでバグにより行き詰まり中

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

