# chatsocket

Multi language implementation of simple chat with websockets for educational purposes

## Features

1. Many clients can be run
2. Time measurement for every roundtrip (message - acknowledgement)
3. Server send messages individually to every client with format: user # message

## Roadmap

- [x] Haskell
- [ ] PureScript
- [x] Ocaml
- [ ] Python
- [ ] TypeScript
- [ ] Rust
- [ ] Kotlin

## How to use

### Haskell

```shell
stack run server
```
in another tab of the console:

```shell
stack run client
```
In client type further message to register client:

    Hi! I am <name>

### Ocaml

Run server:

```shell
 dune exec -- chatsocket -s
 ```
Run client in another tab of the console:

```shell
 dune exec -- chatsocket
```

## Functionality

### Haskell

1. Sending p2p messages
2. Counting time of roundtrip for common messages

### Ocaml

1. Sending p2p messages
2. Counting time of roundtrip for common messages
3. Command #ping return pong from opponent
4. Command #close on server closes client, on client asks server to send Close signal and close client.
