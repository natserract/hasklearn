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
            stackYaml: stack.yaml
              - path: "./app/Main.hs" 
```

## Resources for learning:
- Compiler, interpreter, assembler (indo version) [http://www.infomugi.com/2013/04/pengertian-compiler-interpreter.html](http://www.infomugi.com/2013/04/pengertian-compiler-interpreter.html)


Haskell Fact:
- Haskel is sensitive to whitespace
- Begin with a lower-case letter is a **variable identifiers**, and those that begin with an upper-case letter is **constructor identifiers**.