---
title: Books, Badgers and Big Numbers
tags:
  - mathematics
  - books
description: |
  TBD
---

I stumbled into what might be some combinatorial esoterica and I can't stop thinking about it.

## Books

Up until April 2023 we had a pretty special second hand book shop up the road, next to the local café[^1]. While it was there my mid morning coffee runs usually involved having a browse and I often picked something up.

<ul class="list-none grid grid-cols-2 grid-rows-2 gap-3">
  <li>

  ![The cover the book Chaos by James Gleick](/content/posts/books-badgers-and-big-numbers/assets/images/finds/chaos.webp)

  </li>
  <li>

  ![The cover the book Linear Algebra and Its Applications by Gilbert Strang](/content/posts/books-badgers-and-big-numbers/assets/images/finds/linear-algebra-and-its-applications.webp)

  </li>
  <li>

  ![The cover of the book The Wonderful World of Steam Locomotives by P. B. Whitehouse](/content/posts/books-badgers-and-big-numbers/assets/images/finds/the-wonderful-world-of-steam-locomotives.webp)

  </li>
  <li>

  ![The cover of the book Dancing With Cats by Burton Silver and Heather Busch](/content/posts/books-badgers-and-big-numbers/assets/images/finds/dancing-with-cats.webp)

  </li>
</ul>

A lot of the books I was buying were non-fiction, which led to me over thinking their organisation. I wanted everything grouped by subject, rather than author or title or year or anything like that. So in the end I signed up for a [LibraryThing](https://www.librarything.com) account and used their codes to sort everything.

## Badgers

Some time last year LibraryThing let me know they'd made me a Year In Review. You know, like Spotify does. It's not really my jam, but I clicked through anyway and ended up reading this sentence:

> And make sure your floors will support the added weight of 4.90 adult badgers!

I'd scanned in all our books at once, so this doesn't represent how many books we added to our collection in 2024, but rather the whole lot. At any rate, now whenever I look at our bookshelves I find myself trying to imagine five adult badgers somehow sitting in them. Which is hard, given that I have no idea how big an adult badger is.

## Big Numbers

It's not all that many books, but even still I feel the need to choose _some_ sort of order. Without worrying about the usefulness of any particular ordering, I wonder how many options there really are? How many different ways could I sort all these books?

In [my last post](/posts/crossing-paths-with-descartes) I talked about enumerating possibilities, but in all the examples I used I permitted every possible option. Nothing was excluded, even if two coin tosses or dice throws resulted in the same outcome I still counted them as a unique possibility. What I was talking about there were Cartesian Products. That is: given a set of options $A$ and a set of options $B$ the total range of possibilities of combing both is $A \times B$, the Cartesian Product of sets $A$ and $B$.

<figure class="wide">

$$
\begin{align*}
A &= \{1, 2, 3\} \\
B &= \{a, b, c\} \\
A \times B &= \begin{Bmatrix}
  (1, a) & (1, b) & (1, c) \\
  (2, a) & (2, b) & (2, c) \\
  (3, a) & (3, b) & (3, c)
\end{Bmatrix}
\end{align*}
$$

</figure>

The total number of possibilities is the number of each input set of possibilities multiplied together.

<figure class="wide">

$$
\begin{align*}
|A| &= 3 \\
|B| &= 3 \\
|A \times B|
  &= |A| \times |B| \\
  &= 9
\end{align*}
$$

</figure>

What if we imagine that orderings are the same as combined sets of possibilities? That we choose an element (any element) from $A$ _number of elements in $A$_ times. We might then say that the total number of orderings for some collection of things, $A$, is $|A^{|A|}|$.

<figure class="wide">

$$
\mathit{\text{No. orderings of } A} = \overbrace{|A| \times |A| \times ... \times |A|}^{|A|}
$$

</figure>

More concretely.

<figure class="wide">

$$
\begin{align*}
A &= \{1, 2, 3\} \\
A^{|A|}
  &= A \times A \times A \\
  &= \begin{Bmatrix}
    (1, 1, 1) & (1, 1, 2) & (1, 1, 3) \\
    (1, 2, 1) & (1, 2, 2) & (1, 2, 3) \\
    (1, 3, 1) & (1, 3, 2) & (1, 3, 3) \\
    (2, 1, 1) & (2, 1, 2) & (2, 1, 3) \\
    (2, 2, 1) & (2, 2, 2) & (2, 2, 3) \\
    (2, 3, 1) & (2, 3, 2) & (2, 3, 3) \\
    (3, 1, 1) & (3, 1, 2) & (3, 1, 3) \\
    (3, 2, 1) & (3, 2, 2) & (3, 2, 3) \\
    (3, 3, 1) & (3, 3, 2) & (3, 3, 3)
  \end{Bmatrix} \\
|A^{|A|}|
  &= |A| \times |A| \times |A| \\
  &= 27
\end{align*}
$$

</figure>

But we know that's not right. There are many things in there which aren't _orderings_. Sorting a collection shouldn't duplicate any of its elements, so things like $(1, 1, 1)$ and $(3, 2, 2)$ shouldn't be counted.

What we need is a method to count orderings that accounts for elements being used only once. Because that's how we might lay out an ordering of some collection. We'd choose and item to start with, then choose the next item from what's left, then the next from what's left, and so on until we run out of items.

For example, given the following collection of items.

![The letters, 'a', 'b' and 'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/abc-large.webp)

We could come up with an ordering like this.

| Step | Available choices | Choice | Result |
|------|-------------------|--------|--------|
| 1    | ![The letters 'a', 'b' and 'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/abc.webp) | ![The letter 'b'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/b.webp) | ![The letter 'b'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/b.webp) |
| 2    | ![The letters 'a' and 'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ac.webp) | ![The letter 'a'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/a.webp) | ![The letter 'b'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ba.webp) |
| 3    | ![The letter  'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/c.webp) | ![The letter 'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/c.webp) | ![The letter 'b'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bac.webp) |

At each step the set of available choices reduces by one. Once 'b' is put down, only 'a' and 'c' are left. On the next step once 'a' is put down only 'c' is left. So, in general, rather than this.

<figure class="wide">

$$
\mathit{\text{No. orderings of } A} = \overbrace{|A| \times |A| \times ... \times |A|}^{|A|}
$$

</figure>

What we have instead is this.

<figure class="wide">

$$
\mathit{\text{No. orderings of } A} = |A| \times (|A| - 1) \times (|A| - 2) ... \times 1
$$

</figure>

For the 'a', 'b', 'c' example then we'd have.

<figure class="wide">

$$
\begin{align*}
A &= \{a, b, c\} \\
\mathit{\text{No. orderings of } A}
  &= 3 \times 2 \times 1 \\
  &= 6
\end{align*}
$$

</figure>

And here they all are.

<ul class="list-none grid grid-cols-3 grid-rows-2 gap-3">
  <li>
    <img alt="The letters 'a', 'b' and 'c'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/abc.webp" />
  </li>
  <li>
    <img alt="The letters 'a', 'c' and 'b'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/acb.webp" />
  </li>
  <li>
    <img alt="The letters 'b', 'a' and 'c'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bac.webp" />
  </li>
  <li>
    <img alt="The letters 'b', 'c' and 'a'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bca.webp" />
  </li>
  <li>
    <img alt="The letters 'c', 'a' and 'b'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/cab.webp" />
  </li>
  <li>
    <img alt="The letters 'c', 'b' and 'a'"src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/cba.webp" />
  </li>
</ul>

The result you get from multiplying all the numbers less than some number is called the _factorial_ of that number. It's written like this.

<figure class="wide">

$$
!n = n \times (n - 1) \times (n - 2) \times ... \times 2 \times 1
$$

</figure>

Right now we've got 419 books with codes I can sort on. So how many ways could I have arranged them? What's the factorial of 419?

<figure class="wide">

$$
419 \times 418 \times 417 \times ... \times 3 \times 2 \times 1
$$

</figure>

It's this extremely large number.

2,<wbr/>908,<wbr/>421,<wbr/>057,<wbr/>896,<wbr/>340,<wbr/>474,<wbr/>361,<wbr/>403,<wbr/>432,<wbr/>093,<wbr/>534,<wbr/>061,<wbr/>335,<wbr/>411,<wbr/>576,<wbr/>803,<wbr/>237,<wbr/>495,<wbr/>067,<wbr/>620,<wbr/>711,<wbr/>879,<wbr/>592,<wbr/>887,<wbr/>139,<wbr/>553,<wbr/>095,<wbr/>110,<wbr/>943,<wbr/>225,<wbr/>026,<wbr/>262,<wbr/>406,<wbr/>188,<wbr/>778,<wbr/>320,<wbr/>680,<wbr/>189,<wbr/>451,<wbr/>711,<wbr/>755,<wbr/>153,<wbr/>962,<wbr/>269,<wbr/>093,<wbr/>225,<wbr/>791,<wbr/>463,<wbr/>391,<wbr/>282,<wbr/>309,<wbr/>784,<wbr/>673,<wbr/>974,<wbr/>891,<wbr/>888,<wbr/>521,<wbr/>144,<wbr/>472,<wbr/>954,<wbr/>435,<wbr/>530,<wbr/>212,<wbr/>518,<wbr/>938,<wbr/>132,<wbr/>427,<wbr/>719,<wbr/>386,<wbr/>733,<wbr/>483,<wbr/>011,<wbr/>170,<wbr/>577,<wbr/>979,<wbr/>186,<wbr/>407,<wbr/>238,<wbr/>784,<wbr/>514,<wbr/>071,<wbr/>038,<wbr/>335,<wbr/>200,<wbr/>586,<wbr/>427,<wbr/>023,<wbr/>669,<wbr/>168,<wbr/>853,<wbr/>348,<wbr/>715,<wbr/>855,<wbr/>934,<wbr/>206,<wbr/>590,<wbr/>666,<wbr/>886,<wbr/>642,<wbr/>210,<wbr/>446,<wbr/>229,<wbr/>488,<wbr/>444,<wbr/>697,<wbr/>075,<wbr/>683,<wbr/>231,<wbr/>304,<wbr/>763,<wbr/>674,<wbr/>216,<wbr/>898,<wbr/>857,<wbr/>425,<wbr/>205,<wbr/>633,<wbr/>114,<wbr/>990,<wbr/>510,<wbr/>459,<wbr/>902,<wbr/>307,<wbr/>876,<wbr/>760,<wbr/>425,<wbr/>583,<wbr/>148,<wbr/>615,<wbr/>724,<wbr/>632,<wbr/>751,<wbr/>661,<wbr/>133,<wbr/>203,<wbr/>237,<wbr/>905,<wbr/>432,<wbr/>668,<wbr/>028,<wbr/>736,<wbr/>816,<wbr/>553,<wbr/>087,<wbr/>322,<wbr/>961,<wbr/>245,<wbr/>249,<wbr/>766,<wbr/>243,<wbr/>189,<wbr/>634,<wbr/>620,<wbr/>342,<wbr/>650,<wbr/>104,<wbr/>310,<wbr/>228,<wbr/>160,<wbr/>720,<wbr/>464,<wbr/>432,<wbr/>084,<wbr/>583,<wbr/>406,<wbr/>587,<wbr/>932,<wbr/>793,<wbr/>842,<wbr/>882,<wbr/>817,<wbr/>312,<wbr/>083,<wbr/>420,<wbr/>305,<wbr/>011,<wbr/>614,<wbr/>174,<wbr/>094,<wbr/>373,<wbr/>347,<wbr/>653,<wbr/>327,<wbr/>314,<wbr/>151,<wbr/>733,<wbr/>173,<wbr/>716,<wbr/>990,<wbr/>349,<wbr/>453,<wbr/>642,<wbr/>618,<wbr/>332,<wbr/>498,<wbr/>813,<wbr/>131,<wbr/>703,<wbr/>848,<wbr/>734,<wbr/>139,<wbr/>461,<wbr/>889,<wbr/>356,<wbr/>833,<wbr/>404,<wbr/>468,<wbr/>988,<wbr/>162,<wbr/>294,<wbr/>586,<wbr/>549,<wbr/>217,<wbr/>704,<wbr/>913,<wbr/>958,<wbr/>653,<wbr/>349,<wbr/>142,<wbr/>669,<wbr/>023,<wbr/>627,<wbr/>237,<wbr/>073,<wbr/>393,<wbr/>370,<wbr/>291,<wbr/>667,<wbr/>739,<wbr/>306,<wbr/>611,<wbr/>350,<wbr/>729,<wbr/>516,<wbr/>327,<wbr/>327,<wbr/>902,<wbr/>034,<wbr/>218,<wbr/>650,<wbr/>157,<wbr/>031,<wbr/>351,<wbr/>111,<wbr/>738,<wbr/>938,<wbr/>367,<wbr/>328,<wbr/>649,<wbr/>342,<wbr/>699,<wbr/>827,<wbr/>177,<wbr/>333,<wbr/>219,<wbr/>328,<wbr/>746,<wbr/>468,<wbr/>751,<wbr/>048,<wbr/>295,<wbr/>766,<wbr/>233,<wbr/>367,<wbr/>259,<wbr/>458,<wbr/>382,<wbr/>317,<wbr/>867,<wbr/>840,<wbr/>592,<wbr/>639,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000.

And here's the sort of question that led to my getting stuck in a combinatorial corner: if we take ordering by title to be the _default_,<wbr/> and we sort all orderings by how close they are to that,<wbr/> then what is the, say, (_deep breath_) 2,<wbr/>844,<wbr/>113,<wbr/>050,<wbr/>416,<wbr/>543,<wbr/>052,<wbr/>622,<wbr/>601,<wbr/>377,<wbr/>724,<wbr/>177,<wbr/>730,<wbr/>794,<wbr/>635,<wbr/>580,<wbr/>558,<wbr/>165,<wbr/>101,<wbr/>289,<wbr/>087,<wbr/>907,<wbr/>211,<wbr/>046,<wbr/>617,<wbr/>413,<wbr/>162,<wbr/>202,<wbr/>727,<wbr/>992,<wbr/>682,<wbr/>826,<wbr/>505,<wbr/>610,<wbr/>602,<wbr/>434,<wbr/>122,<wbr/>752,<wbr/>894,<wbr/>055,<wbr/>956,<wbr/>478,<wbr/>001,<wbr/>246,<wbr/>987,<wbr/>975,<wbr/>654,<wbr/>867,<wbr/>880,<wbr/>927,<wbr/>440,<wbr/>389,<wbr/>088,<wbr/>878,<wbr/>653,<wbr/>260,<wbr/>211,<wbr/>539,<wbr/>374,<wbr/>415,<wbr/>866,<wbr/>208,<wbr/>792,<wbr/>187,<wbr/>943,<wbr/>989,<wbr/>161,<wbr/>515,<wbr/>680,<wbr/>780,<wbr/>226,<wbr/>637,<wbr/>325,<wbr/>041,<wbr/>170,<wbr/>571,<wbr/>521,<wbr/>944,<wbr/>136,<wbr/>857,<wbr/>756,<wbr/>357,<wbr/>610,<wbr/>408,<wbr/>690,<wbr/>480,<wbr/>987,<wbr/>642,<wbr/>359,<wbr/>289,<wbr/>070,<wbr/>142,<wbr/>617,<wbr/>861,<wbr/>100,<wbr/>311,<wbr/>817,<wbr/>255,<wbr/>304,<wbr/>659,<wbr/>710,<wbr/>866,<wbr/>175,<wbr/>534,<wbr/>883,<wbr/>866,<wbr/>241,<wbr/>789,<wbr/>715,<wbr/>155,<wbr/>834,<wbr/>223,<wbr/>460,<wbr/>969,<wbr/>127,<wbr/>343,<wbr/>122,<wbr/>506,<wbr/>553,<wbr/>271,<wbr/>818,<wbr/>293,<wbr/>567,<wbr/>727,<wbr/>306,<wbr/>529,<wbr/>166,<wbr/>052,<wbr/>023,<wbr/>268,<wbr/>045,<wbr/>186,<wbr/>141,<wbr/>937,<wbr/>347,<wbr/>041,<wbr/>818,<wbr/>966,<wbr/>122,<wbr/>038,<wbr/>640,<wbr/>793,<wbr/>796,<wbr/>799,<wbr/>685,<wbr/>516,<wbr/>455,<wbr/>929,<wbr/>549,<wbr/>507,<wbr/>013,<wbr/>186,<wbr/>833,<wbr/>430,<wbr/>265,<wbr/>450,<wbr/>466,<wbr/>044,<wbr/>505,<wbr/>744,<wbr/>178,<wbr/>793,<wbr/>393,<wbr/>254,<wbr/>499,<wbr/>311,<wbr/>497,<wbr/>130,<wbr/>361,<wbr/>821,<wbr/>989,<wbr/>719,<wbr/>697,<wbr/>224,<wbr/>981,<wbr/>301,<wbr/>053,<wbr/>842,<wbr/>088,<wbr/>290,<wbr/>183,<wbr/>796,<wbr/>995,<wbr/>774,<wbr/>643,<wbr/>108,<wbr/>623,<wbr/>128,<wbr/>572,<wbr/>424,<wbr/>596,<wbr/>295,<wbr/>207,<wbr/>864,<wbr/>719,<wbr/>379,<wbr/>479,<wbr/>419,<wbr/>093,<wbr/>894,<wbr/>525,<wbr/>035,<wbr/>709,<wbr/>405,<wbr/>252,<wbr/>305,<wbr/>572,<wbr/>483,<wbr/>849,<wbr/>439,<wbr/>595,<wbr/>150,<wbr/>582,<wbr/>362,<wbr/>925,<wbr/>993,<wbr/>365,<wbr/>553,<wbr/>189,<wbr/>173,<wbr/>069,<wbr/>073,<wbr/>069,<wbr/>239,<wbr/>396,<wbr/>792,<wbr/>666,<wbr/>051,<wbr/>139,<wbr/>882,<wbr/>098,<wbr/>736,<wbr/>318,<wbr/>227,<wbr/>488,<wbr/>496,<wbr/>884,<wbr/>365,<wbr/>414,<wbr/>402,<wbr/>236,<wbr/>391,<wbr/>348,<wbr/>245,<wbr/>541,<wbr/>096,<wbr/>370,<wbr/>960,<wbr/>228,<wbr/>964,<wbr/>587,<wbr/>593,<wbr/>160,<wbr/>349,<wbr/>264,<wbr/>902,<wbr/>263,<wbr/>727,<wbr/>302,<wbr/>169,<wbr/>421,<wbr/>863,<wbr/>892,<wbr/>154,<wbr/>866,<wbr/>370,<wbr/>306,<wbr/>041,<wbr/>614,<wbr/>127,<wbr/>727,<wbr/>830,<wbr/>967,<wbr/>236,<wbr/>995,<wbr/>581,<wbr/>667,<wbr/>801,<wbr/>392,<wbr/>480,<wbr/>447,<wbr/>679,<wbr/>422,<wbr/>359,<wbr/>386,<wbr/>205,<wbr/>525,<wbr/>674,<wbr/>979,<wbr/>615,<wbr/>059,<wbr/>536,<wbr/>733,<wbr/>032,<wbr/>646,<wbr/>807,<wbr/>296,<wbr/>164,<wbr/>119,<wbr/>104,<wbr/>440,<wbr/>573,<wbr/>184,<wbr/>560,<wbr/>777,<wbr/>118,<wbr/>240th ordering?

## Lehmer codes

...

[^1]: We were very sad to see Logical Unsanity close. The ABC did a nice feature at the time: [_Quirky tin shed bookshop 'born out of laziness' offers booklovers' sanctuary_](https://www.abc.net.au/news/2018-04-08/quirky-tin-shed-bookshop-offers-booklovers-sanctuary-brisbane/9618992)
