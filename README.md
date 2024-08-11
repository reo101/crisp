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

## Example code

Example code can be found at [`./crisp/std.crisp`](./crisp/std.crisp)
