# 2023

[Gallery (spoilerish) here](./gallery-2023.md)

All this ascii art and elves makes me want to play nethack...(coffeebug on nethack.alt.org)

```
You consume deep fried Elvenking. Eating deep fried food made your
fingers very slippery. Your weapon slips from your hands. You feel wide awake!
```

Language: R. Been a while.

Started a day late. Let's see if I can keep it up.

Struggling a bit remembering how to be an R-soul, very different to being Pythonic.
The data manipulation is stupidly easy once you construct the dataframe (`data.table` is an amazing package, far better
than Pandas). But where in Python you'd just write for-loops and they would be easy to read, the R way is to do
everything vectorised. The nested `*apply` are quite difficult to read.

**Hard so far**: day 12, day 16. The common theme is recursive functions/efficiency in/managing R's "node stack" limit (whatever that is).
And instead of trying to be efficient in the recursion, write the dumbest version first (e.g. move one character at a time rather than
 fast-forwarding).
 

| Day | Comment                                                                                                                                      |
|----:|----------------------------------------------------------------------------------------------------------------------------------------------|
|  25 | Was gonna do it properly but visualising my data just so happened to point out the solution, so ... ¯\\_(ツ)_/¯              |
|  24 | Very angry at this day, don't think it was a coding problem so much as a "use a library to do it for you"                                       |
|  23 | Frustrating - turns out I had the correct approach very early on but ran into the slowness of R - tried Rcpp hours later and it ran in seconds. |
|  22 | A nice breather (thank goodness)                                                                                                             |
|  21 | I'm a dirty dirty statistician and I love it.                                                                                                |
|  20 | Had a lot of difficulty reading this one, part 1 took me AGES. Part 2 was done by inspection and by hand.                                    |
|  19 | Picked the wrong direction to scale in for part 1 and had to abandon it entirely for part 2. Think like a programmer not a statistician :(   |
|  18 | Should have got that way sooner. Gauss' shoelace formula is AMAZING                                                                          |
|  17 | Wasted time going down the recursive function train of thought (because of last few days) before realising.                                  |
|  16 | Frustrating - had to re-write the nature of the recursion because it got too many frames deep for R? (but only 10,282 function calls?)       |
|  15 | The difficulty was in reading the instructions.                                                                                              |
|  14 | Cheesed it with visual insepction and a calculator. "Identified patterns" ;) because that implementation ain't fast                          |
|  13 | Took way too long due to staying up til midnight trying to do day12 part2 lol                                                                |
|  12 | AAAAAAAAAAAAAAAAHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH                                                                             |
|  11 | I thought I was being smart but [@Steven's](https://github.com/stevetr14/aoc2023/blob/bbfaf5b7c4cba6537cbe3385b702a6fed53ab365/day_11.py#L56) observation was galaxy-brain level |
|  10 | Quite fun. First time both answers were correct on the first try. Writing the intermediate outputs to file was surprisingly helpful.         |
|  09 | I see the way they want us to do it and laugh _fits order20 polynomials instead_ what's the point of using R if you can't overkill with it?  |
|  08 | Next time I'll try my "naive" part1 solution on part2 while programming up the more general part2 case, in case it turns out to be right :(  |
|  07 | That was fun, though I had a brainfart moment and forgot that I should just multisort in "wide" format rather than long                      |
|  06 | FUCK YEAH MATHS (after the morale-whooping received yesterday)                                                                               |
|  05 | Just cannot work out the smart way to do it, but I'm so close to conceptualising it. Very frustrating. R is not suited to brute-forcing.     |
|  04 | I feel like there is a sweet mathsy way to do part 2 but oh well, bring out the for-loop. Betraying everything R.                            |
|  03 | "Overlaps" logic does my head in for some reason. Took much longer than it should. Part 2 was pleasingly trivial from the part 1 data.table. |
|  02 | I LOVE `data.table`. But also, R lets you do `"5" < 15` without raising exception?!?! (and returns a bool)                                   |
|  01 | imagine that - the function that said it "extracts the last match" doesn't. ✟ ℝ.𝕀.ℙ. ✟                                                       |


Todo:

* day 17
  - optimise the creation of the adjacency-matrix (pre-allocate the sparse matrix OR don't make a matrix at all)
  - write Dijkstra myself
