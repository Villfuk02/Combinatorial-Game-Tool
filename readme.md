# Combinatorial Game Tool

This Project is a simple tool for work with combinatorial games. It is based on the theory from the book *Winning Ways* by Berlekamp, Conway, and Guy. To get started with this tool, no prior knowledge apart from Haskell is required. Note, that this tool has only a few basic functions and features. The theory of combinatorial games is much deeper and richer.

## What is a game

For purposes of this tool, a game is played by two players. Players alternate in taking turns and when a player can't make any move, they lose. The games don't contain any randomness and both players have complete information.

## Making moves

The simplest game is `zero`. In it, no player has any moves, so whoever goes first, loses. To confirm this, you can run `options LeftPlayer zero` and `options RightPlayer zero`. Both return an empty list. To get both at once, you can use `toPosition zero`. It will return a `Position [] []` displayed as `{ | }`. We will say this game has value `0`. A game has value `0` exactly when the first player to move always loses, assuming perfect strategy from their opponent.

Another very simple and very abstract game is `NumberGame`. For example `NumberGame 1` has value `1`. This means that the left player has 1 move advantage and if they play optimally, they will win. You can observe, that in this game, the left player has one possible move - to `zero`, whereas the right player has no available moves.

## Every move counts

Games can be added together. When you play a sum of games, you can make a move in either of them. Let's look at `numbersExample = g 2 + g (-4)`. `g` is just an abbreviation for `NumberGame`. In the first game, the left player can make 2 moves and right can make none. In the second one, right can make 4 moves and left can make none. In their sum, right will always have moves to spare, and so they will win. The first game has a value of `2` and the second one `-4`. The total value is `-2`, so right always wins. You can check this by running `simplify numbersExample`. `simplify` will try to find the simplest form of the game.

Games can also be multiplied by integers. For example for game `g`, `Multiple 3 g` is equivalent to `g + g + g`. You can also use negative numbers: `Multiple (-1) (g 3)` = `negate (g 3)` = `g (-3)`. A negated game has the roles of left and right player swapped.

## Playing Cutcake

A less abstract game is Cutcake. Here, we have a rectangular cake with integer dimensoions width x height. Left can slice it vertically into two smaller integer-sized rectangles whereas right can slice horizontally. Left cannot slice cakes of witdth 1 any further and right cannot slice cakes of height 1. Cakes of size 1 x 1 can thus be ignored. This is a cake of size 4 x 4: `cutcakeExample = gamify (Cutcake 4 4)` (when in doubt, add `gamify`). You can again check what moves can which player make if you're not sure. This cake is a square, so neither player has the advantage and it has the value `0`. You can try to play the game yourself by running `play p cutcakeExample`. Instead of `p`, substitute which player starts. You can see for yourself that no matter who starts, the other player can make them lose.

## Linear Col

Linear col is a simple game, played on a line of dashes (**-**) (i.e. `linearColFromString "----"`). As a move, left player can color any of the dashes **L**ime and right can color them **R**ed. When you color a dash, its neighbors get slightly tinted with the opposite color (**r**ed or **l**ime). When a dash it tinted, it can only be colored in with the matching color. If a dash ever gets tinted by both colors (**x**) it cannot be colored by either plyer.

## Comparing games and fractional moves

Consider the Col position `l-`. Right can make one move, but left has the advantage. They can take the move away from right by playing first (`xL`), but right still allows left to make one turn after theirs (`lR`). This game is positive, its value is greater than 0. You can check that `linearColFromString "l-" >: zero` return `True`. All game comparison operatos have a colon after them. How big is left's advantage, though? It's exactly `1/2` a move. We can prove this by showing the game `fractionExample = linearColFromString "l-" + linearColFromString "l-" + g (-1)` has value `0` (for example with `fractionExample ==: zero`). A nice shorthand for fractional games is `/:` i.e. `3 /: 4`. Note that only powers of two as denominators are supported.

## Nim and nimbers

In the game ClassicNim, there is a heap of stones. On each turn, a player must take 1 to 3 stones from the heap. As always a player loses, when no moves remain. Consider the game `gamify (ClassicNim 5)`. In Nim, neither player has the advantage, but in this specific instance, the first player wins! This game has the value `star` shown as `*`. It is not less than zero, not greater than zero, not equal to zero, but rather *Confused with* zero (try `gameCompare star zero` or `star ||: zero`).

When you try to play `Multiple 2 (gamify (ClassicNim 1))`, representing two heaps, each with one stone, you'll find out that this game is equal to zero again. So `*` + `*` = `0`. The game `gamify (ClassicNim 2)` however, has value `*2` (you can get it as `s 2`) which is still a win for the first plyer, but it's not equal to `*`. This is relevant because `nimAdditionExample = gamify (ClassicNim 1) + gamify (ClassicNim 2) + gamify (ClassicNim 3)` equals `0`.

Then there is UnlimitedNim, where a player can take any number of stones, greater than 0, and FibonacciNim, where a player can take a number of stones equal to some Fibonacci number. You can try to spot some patterns in FibonacciNim games of size 1 to 100: `fibonacciNimAnalysis = map (simplify . gamify . FibonacciNim) [1 .. 100]`.

## Toads and Frogs

Another game is ToadsAndFrogs (for example `upExample = toadsAndFrogsFromString ">_><<"`). It is played on a strip where left controls toads (**>**) that can only go right and right controls frogs (**<**) that can only go left. A player may move one of their amphibians one space forward if it is empty (**\_**), or jump over the opponent's one if it is directly in front of it and the next space is empty.

## Ups and downs

What's the value of `upExample`? It is `up` (shown as `^`) of course! It is positive, but very tiny. Less than any positive fraction. And it is also confused with `*`. It's opposite is `down`, shown as `v`. If you want to know ups and downs, you can experiment with them yourself. However, we skipped a lot of important theory, so if you want to understand them, I highly recommend reading through it and experimenting along your journey.

## Heating up

If it seems the games we've explored are rather boring, it is because they are. Or *cold* by definition. Each player tries very hard not to play, conserving every move. Then there are hot games, where players are incentivised to play. For example the game `hotGameExample = toadsAndFrogsFromString "_>>_<<_"` has the value `{ 1/4 | -1/4 }`. This means that when either player makes their move, it's basically free and they get one quarter of a move as a bonus!

## Beaten by TicTacToe

The tool doesn't know hot games and can't really efficiently work with them. It also has only the simplest understanding of cold games, so many simplifiable positions won't get simplified.

You probably know tic tac toe, but in this version instead drawing when the grid is full, you lose when unable to make a move. You can get it with `blankTicTacToe`. Here the limitations of this tool become noticeable. It can't find the most simplified value of this game, partly because some positons are hot, partly because it can't simplify more advanced cold positions.
