# Effects-Example
Example for INF221 class on effects

## How to run

```
cabal run
```

If you want to see the command line options write:
```
cabal run -- Example -h
```

When using `cabal run`, you can add `-- PROG_NAME` and then what follows is as if
you are passing command line arguments to the executable itself. So for example
`cabal run -- foo -h` is equivilent to finding the executable, `foo`, in the 
`dist-newstyle` directory and running `./foo -h`

## How to use the chat server
```
telnet <IP> <PORT>
```
