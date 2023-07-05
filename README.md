# chatsocket

Multi language implementation of simple chat with websockets for educational purposes

## Features

1. Many clients can be run
2. Time measurement for every roundtrip (message - acknowledgement)
3. Server send messages individually to every client with format: user # message

## Roadmap

- [x] Haskell
- [ ] PureScript
- [ ] Ocaml
- [ ] Python
- [ ] TypeScript
- [ ] Rust
- [ ] Kotlin

## How to use

- Haskell

```shell
stack run server
```
in another tab of the console:

```shell
stack run client
```
In client

    Hi! I am <name>
