# haskell-from-stratch

## Running some command
`stack ghci`

## Running project
`stack run`

## Setup Multiple Cradle
- Add hie.yaml file in root directory
- My yaml config:

```yaml
cradle:
   multi:
    - path: "./"
      config:
        cradle:
          stack:
            - path: "./app/Main.hs"
```