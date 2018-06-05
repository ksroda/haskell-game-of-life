# Game of life

## Program modes
  - game - watch automata evolution in console
  - calculations - get board state after defined number of iterations

## In code

### Choosing mode by commenting one of the modes:
```
  main = do
    [...]
    -- result <- runGameMode 20 50
    result <- runCalculateFinalMode 20 5000
    [...]
```
### Modes' parameters
```
runGameMode [boardSize] [iterations]
runCalculateFinalMode [boardSize] [iterations]
```

## Sample execution

```
ghc -threaded -eventlog -rtsopts --make main.hs
./main +RTS -ls -N4
threadscope main.eventlog
```