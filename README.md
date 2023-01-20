# Crisp

`Crisp` is a curried lisp with its syntax being an amalgamation of `Scheme` and `Fennel`.

## Building

The project depends on `GHC 9.0.2` which can be installed by either `GHCUP`, you or `Nix`:

```bash
# Enter an evnironment with the correct `ghc` version
nix develop

# Build the project
stack build

# Run the project
stack run
```

## File Structure

The project is split in two parts:
- [`./src/ParserCombinators/`](./src/ParserCombinators/) - small Parser Combinator library with parser for `Crisp` itself
- [`./src/Crisp/`](./src/Crisp/) - repl and interpreter for the already parsed `Crisp`

## Design

The project is built upon `mtl`'s `Monad___` typeclasses. Unfortunately, enforcing the `MonadError String m` constraint on the interpreter (together with `MonadState m` and `MonadFix m`) leads to infitite loops in most `let`-expressions, so, currently, interpreter errors are handled with the `error` function but the code is written in a way to easily enable the refactoring of using `MonadError` for errors handling when the looping error is (eventually) resovled.
Other than that, at least, parsing errors are caught early enough.

## Main functions

- `parse` - Used to run a `Parser` on some input and (optionally) get a parsed AST back
- `eval` - Used to evaluate already parsed code and aggregate the effect it induces (like `define`) using the monadic properties of `State`

## Example code

Example code can be found at [`./crisp/std.crisp`](./crisp/std.crisp)
