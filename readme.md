# A Template Haskell powered AoC template.

How does this work?

The TemplateHaskell code in Main.hs looks for any functions of the form `dayXXY :: s -> t` such that:

* `XX` is a two-digit number from 01 to 25;
* `Y` is either `a` or `b`;
* There is an instance of `From String s` in scope;
* There is an instance of `From t String` in scope.

You can define instances for `From String s` and `From t String` however you like; the former represents your parser for that program and the latter how you want to display the result. For simple types (`String`, `Text`) implementations will exist; for others (`Int`,` Integer`) it's obvious (`read`/`show`).

Compiling the program will build an executable called `aoc` which can be called like:

```sh
# To run part a of day 1
cabal run aoc -- 1a

# To run both parts of day 20
cabal run aoc -- 20
```

It will tell you at runtime if you try to run a part that hasn't been written in the program yet. It will automatically run the file `input/dayXX` through your `From` instance, run the dayXX part(s), run the `From` instance and output the resulting string.

Have fun!