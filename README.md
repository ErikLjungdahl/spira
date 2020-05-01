# Spira, a Board Game DSL


## Getting Started

### Prerequisites

Spira compiles to [Ceptre](https://github.com/chrisamaphone/interactive-lp), get the binary to play the games.

We recommend putting the binary in your path

### Using Spira

Spira will soon be available as a Cabal package
```
cabal update
cabal install spira
```

### Testing Spira

Clone this repository and `stack build`

Compile a game to Ceptre-code, which currently is put in "game.cep" with `stack exec <game-executable>`

Examples:
```
stack exec tic-tac-toe
stack exec connect-four
stack exec othello
```

### Running Ceptre with our Python-script

Our Python-script makes the user experience of Ceptre better and supports visualization of rectangular boards.

```
python2 UI.py <ceptre-bin> game.cep
```

Player colours are only supported on Linux and MacOS

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Trivia

Spira was made as a bachelor thesis on Chalmers University of Technology in 2020
