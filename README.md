min-caml for Carina
====

## Usage

1. execute ./to\_carina
2. make min-caml
3. ./min-caml [ml program source]

### option

This min-caml have -debug option for debugging. The usage is

```
./min-caml -debug [option] [source]
```

and it has following options now.

* parser: debug print parser output (AST)
* knormal: debug print k-normalized expression
* asm: debug print assembly program just after allocating registers

## Known Issues

* cannot define function which has more than 11 integer arguments, 33 float arguments
  because Carina-min-caml only use register to pass arguments at the moment
