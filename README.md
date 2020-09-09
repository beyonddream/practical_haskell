# practical_haskell
My worked out exercises from practical haskell book

# hie + coc config
1. setup

```
 git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
```
```
 cd haskell-ide-engine
```
```
 stack ./install.hs hie-<ghc_version>
```

2. Below is the coc config

`{
"languageserver": {
  "haskell": {
    "command": "hie-wrapper",
    "args": ["--lsp"],
    "rootPatterns": [
      "*.cabal",
      "stack.yaml",
      "cabal.project",
      "package.yaml"
    ],
    "filetypes": [
      "hs",
      "lhs",
      "haskell"
    ],
    "initializationOptions": {
      "haskell": {
      }
    }
  }
}
}`

