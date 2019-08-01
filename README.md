# lac

## Usage

Load program and enter REPL

    $ lac -i <path/to/program.ml>

Show declarations of program

    > :decls
    delete a t = ...
    insert a t = ...
    splay a t = ...
    splay_max t = ...

Evaluate expression

    > insert 42 nil
    {nil, 42, nil}

Quit

    > :quit

Analyze program

    $ lac <path/to/program.ml>

Example run

    $ lac examples/splay.ml
    > insert 42 (insert 0 (insert 2 nil))
    {{{nil, 0, nil}, 2, nil}, 42, nil}
    > delete 0 (insert 42 (insert 0 (insert 2 nil)))
    {nil, 2, {nil, 42, nil}}
    > :quit

## Running/compiling `lac`

Run the `lac` tool using [Stack](https://haskellstack.org/)

Interpret using `ghci`

    $ stack ghci

Compile using `ghc`

    $ stack build

Install into `$HOME/.local/bin`

    $ stack install
