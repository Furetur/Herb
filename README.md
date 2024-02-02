# The Herb Programming Language

> A programming language that I am developing as a pet-project

## Build from sources

These instructions describe how to create an empty OCaml environment and build the Herb Compiler. If you are a pro you can skip certain steps

1. Create an empty OCaml environment

```
opam switch create 5.1.1
```

2. Install all dependencies

```
opam install . --deps-only
```

3. Build

```
dune b
```

4. The binary can be found at `./_build/default/bin/herbc.exe`
