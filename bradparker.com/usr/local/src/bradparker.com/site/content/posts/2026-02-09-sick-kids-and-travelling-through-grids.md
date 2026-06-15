---
title: Sick kids and travelling through grids
published: 2026-06-15
thumbnail: /content/posts/sick-kids-and-travelling-through-grids/assets/images/ETH-BIB-Ruinen_von_Timgad-Mittelmeerflug_1928-LBS_MH02-04-0169.tif.jpg
description: |
  In solving a Project Euler puzzle I found out about binomial coefficients. Years later my kid spent a sick-day on the couch binging TV and in joining in, ostensibly to take care of him, my mind wandered off a bit.

  ![The ruins of Timgad](/content/posts/sick-kids-and-travelling-through-grids/assets/images/ETH-BIB-Ruinen_von_Timgad-Mittelmeerflug_1928-LBS_MH02-04-0169.tif.jpg)
tags:
  - Mathematics
---

Some time last year, during one of the many bouts of minor illness which strike children who attend daycare or kindergarten, ours was laid up on the couch watching episode after episode of [Go Jetters](https://en.wikipedia.org/wiki/Go_Jetters). We must've watched everything then hosted on ABC iView at least three or four times. I wasn't feeling too well myself so joined in on the binge and after a while my head started wandering off down funny furrows.

The show centres around visiting famous places. Somehow the hapless antagonist (he's not intentionally malicious) is always there and shenanigans ensue. One episode features the ancient city of Timgad, in modern-day Algeria, and some time during I-don't-know-which re-watch I wondered if Timgad might help me scaffold some notes about [binomial coefficients](https://en.wikipedia.org/wiki/Binomial_coefficient). So here we are.

## Right, togas on, time for the question

Here is a map of archaeological sites at Timgad, you can click through for the full version which includes a key for all the numbered locations.

<figure>

[![A map of the ancient city of Timgad with colour-coded and numbered regions indicating buildings and sites which have been studied. The main part of the city is, for the most part, a near perfectly square eleven by twelve grid. In the top right ](/content/posts/sick-kids-and-travelling-through-grids/assets/images/Timgad_archaeological_sites_map-en-reduced.svg)](/content/posts/sick-kids-and-travelling-through-grids/assets/images/Timgad_archaeological_sites_map-en.svg)

<figcaption>

Source: [Timgad archaeological sites map - Wikimedia Commons](https://commons.wikimedia.org/wiki/File:Timgad_archaeological_sites_map-en.svg). Used under the conditions of the [Creative Commons](https://en.wikipedia.org/wiki/en:Creative_Commons) [Attribution-Share Alike 3.0 Unported](https://creativecommons.org/licenses/by-sa/3.0/deed.en) licence.

</figcaption>

</figure>

In my Go Jetters induced haze I could've imagined myself in Timgad about 1,500 years ago. Let's say I've just stepped out of the Nothern-East Baths (top right, blue, number 23), feeling very refreshed. I find myself at the intersection on the south west corner of the baths and I decide I'd like to spend some time at the Public Library (roughly in the middle of the grid, purple, number 46). It's not far away, but every direct, shortest, way I might think of to get there is essentially the same distance. How many of those roughly equal, non-meandering, paths are there? How many different routes could I amble to arrive at the north east corner of the library?

## To turn or not?

The first time I encountered [a question like this one](https://projecteuler.net/problem=15) I stared into the middle distance for a very long time before eventually figuring out that not only is every path the same in length, but they're also, abstractly, the same in content. At each intersection you can either head south, or west, do anything else and you're wandering off course. Furthermore, you must make the same number of westward (4) and southward (3) moves for any path you choose, otherwise you'll over or undershoot.

This turns the problem into something that doesn't mention ancient Roman cities, roads, grids, distances, any of that. It's a question of counting subsets of a certain length. To see how, here's an example choice of path that'll get you from the baths to the library:

1. West
2. West
3. South
4. West
5. South
6. South
7. West

This can be simplified and described as only: by default head west, but turn South at moves 3, 5, and 6. In this way any path can be described entirely by listing only the three intersections where you headed south instead of west. Said another way, given the set $\{1 ... 7\}$ we can choose any three unique elements (the moves where we'd turn south) and we get a path. The set of all unique choices of three elements represents all the possible paths.

$$
\begin{Bmatrix}
\{1, 2, 3\} \\
\{1, 2, 4\} \\
\{1, 2, 5\} \\
...
\end{Bmatrix}
$$

Right, so how many elements does this set have? How many unique subsets of three unique elements are there? To find the answer I ended up stumbling into binomial coefficients, the below represents my back-tracking after I read a bit about them, to try and explain it to myself step by step.

## K-permutations

To find the number of unique 3-element subsets, I'd first need to figure out how many not-necessarily unique sets of unique elements there are. Figuring this out builds on [my last post about permutations and Lehmer codes](/posts/books-badgers-and-big-numbers). To do that I can take the full set of permutations...

$$
\begin{Bmatrix}
\{1, 2, 3, 4, 5, 6, 7\} \\
\{1, 2, 3, 4, 5, 7, 6\} \\
\{1, 2, 3, 4, 6, 5, 7\} \\
...
\end{Bmatrix}
$$

And then chop off however many elements I need to in order to trim each permutation down to three elements each...

$$
\begin{Bmatrix}
\{5, 6, 7\} \\
\{5, 7, 6\} \\
\{6, 5, 7\} \\
...
\end{Bmatrix}
$$

If I remember (from the aforementioned previous post) that the count of all permutations of a set is equal to the factorial of the number of elements in the set...

$$
\begin{align*}
S &= \{1, 2, 3, 4, 5, 6, 7\} \\
n &= |S| \\
p &= n! \\
  &= 7! \\
  &= 7 \times 6 \times 5 \times 4 \times 3 \times 2 \times 1
\end{align*}
$$

Then the effect on the count of permutations of chopping those elements off is the same as ...

$$
\begin{align*}
k &= 3 \text{ (The number of moves.)} \\
p_k
  &= \frac
    {n!}
    {(n - k)!} \\
  &= \frac
    {7!}
    {(7 - 3)!} \\
  &= \frac
    {7 \times 6 \times 5 \times 4 \times 3 \times 2 \times 1}
    {4 \times 3 \times 2 \times 1} \\
  &= 7 \times 6 \times 5
\end{align*}
$$

This is the count of k-permutations, the count of permutations of length 'k'.

## Binomial coefficients

The above counts permutations of some length, but that includes too many things, after all saying "turn south at moves 5, 6, and 7" is the same as saying "turn south at moves 5, 7, and 6", albeit more obliquely. To trim it down I'd need to figure out how many copies there are of each unique set of moves. That answer is come to by recalling that for every unique group of 3 moves all the permutations are there, so there are $3!$ copies of each unique set. To remove them from the total count I can divide it by $3!$.

$$
\begin{align*}
  m &= \frac
      {
        \left(
          \frac
            {n!}
            {(n - k)!}
        \right)
      }
      {k!} \\
    &= \frac
      {n!}
      {k!(n - k)!} \\
    &= \frac
      {7!}
      {3!(7 - 3)!} \\
    &= \frac
      {7 \times 6 \times 5 \times 4 \times 3 \times 2 \times 1}
      {3 \times 2 \times 1 \times 4 \times 3 \times 2 \times 1} \\
    &= \frac
      {7 \times 6 \times 5}
      {3 \times 2 \times 1} \\
    &= \frac
      {210}
      {6} \\
    & = 35
\end{align*}
$$

## And here we have it

If imaginary 1,500 years-ago me made a bit of a daily routine of casually, but directly, strolling from the baths to the library I could take a unique route each day without repeating for 35 days. There's a thought.

As it stands I reckon I'm happy here in the present day, where me and my kid get to enjoy an ample supply of [Ubercorn's](https://gojetters.fandom.com/wiki/Ubercorn) "Funky Facts".
