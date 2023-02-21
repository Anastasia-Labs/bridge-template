# bridge-template

## Dev environment

```
$ nix develop
```

## Running executable

```
[nix develop:~/bridge-template]$ cabal run
```

## Running test
```
[nix develop:~/bridge-template]$ cabal run bridge-template-test
```

## Running hoogle

```
[nix develop:~/bridge-template]$ hoogle server --local --port=8085
```

## Precommits

### Run `,format` before commits cabal *.hs *.nix *.cabal

```
[nix develop:~/bridge-template]$ ,format 
```

### Format check
```
[nix develop:~/bridge-template]$ ,format check
```

## Using HLS

- Install [Nix Environment Selector](https://marketplace.visualstudio.com/items?itemName=arrterian.nix-env-selector)

- Open Command Palette (Ctrl + Shift + P) and run Nix-Env: Select Environment command.

- Select `shell.nix`
- Wait and Restart VSCode