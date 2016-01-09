# GCHQ 2015 Christmas Puzzle

I hadn't seen puzzles like this before. Kinda fun. Good chance to have a first play with Scala.js.

Scala.js - awesome.

Basic idea was to work out all the possible solutions for a row/column as a bit pattern. I think you get 50 odd bits of number in JS but the shifts only work on 32. Luckily we only need 25. Once you have them as bits, you can and/or/not them together to work out which bits are always on/off across all solutions. Obviously solutions for rows and columns affect each other. Use that to discard solutions that now don't fit. Rinse/repeat.

I wasn't sure if it would work but it works for this puzzle. Clicking things is more interesting than a computer just giving you the answer.

This solution - Hammer, not scalpel.

Use sbt to build and fastOptJS or fullOptJS to build the target js.
