---
title: Books, Badgers and Big Numbers
published: 2025-06-21
tags:
  - mathematics
  - books
thumbnail: /content/posts/books-badgers-and-big-numbers/assets/images/thumbnail.jpeg
description: |
  ![A random collection of images of colourful magnetic letters laid over a picture of stacks of books.](/content/posts/books-badgers-and-big-numbers/assets/images/thumbnail-small.webp)

  I stumbled into what might be some combinatorial esoterica and I can't stop thinking about it. Additionally, a book organising app told me I have a bunch of badgers in my house. I need to sort them out.
---

I stumbled into what might be some combinatorial esoterica and I can't stop thinking about it.

## Books

Up until April 2023 we had a pretty special second hand book shop up the road, next to the local café[^1]. While it was there my mid morning coffee runs usually involved having a browse and I often picked something up.

<div class="list-none grid grid-cols-2 grid-rows-2 gap-3">
  <div>

  ![The cover the book Chaos by James Gleick](/content/posts/books-badgers-and-big-numbers/assets/images/finds/chaos.webp)

  </div>
  <div>

  ![The cover the book Linear Algebra and Its Applications by Gilbert Strang](/content/posts/books-badgers-and-big-numbers/assets/images/finds/linear-algebra-and-its-applications.webp)

  </div>
  <div>

  ![The cover of the book The Wonderful World of Steam Locomotives by P. B. Whitehouse](/content/posts/books-badgers-and-big-numbers/assets/images/finds/the-wonderful-world-of-steam-locomotives.webp)

  </div>
  <div>

  ![The cover of the book Dancing With Cats by Burton Silver and Heather Busch](/content/posts/books-badgers-and-big-numbers/assets/images/finds/dancing-with-cats.webp)

  </div>
</div>

A lot of the books I was buying were non-fiction, and it's this I'd like to blame on my over-thinking their organisation. I wanted everything grouped by subject, rather than author or title or year or anything like that. So in the end I signed up for a [LibraryThing](https://www.librarything.com) account and used their codes to sort everything.

## Badgers

Some time last year LibraryThing let me know they'd made me a Year In Review. You know, like Spotify does. It's not really my jam, but I clicked through anyway and ended up reading this sentence:

> And make sure your floors will support the added weight of 4.90 adult badgers!

I'd scanned in all our books at once, so this doesn't represent how many books we added to our collection in 2024, but rather the whole lot. At any rate, it's common now that when I look at our bookshelves I find myself trying to imagine five adult badgers somehow sitting in them. Which is hard, given that I have no idea how big an adult badger is.

## Big Numbers

Despite the badgers, it's not all that many books really. But even still I felt the need to choose _some_ sort of ordered arrangement. Now, without worrying about the usefulness of any particular arrangement, I wonder how many options there really are? How many ways could I have arranged these books?

In [my last post](/posts/crossing-paths-with-descartes) I talked about enumerating possibilities, but in all the examples I used I permitted every possible option. Nothing was excluded, even if two coin tosses or dice throws resulted in the same outcome I still counted them as a unique possibility. What I was talking about there were Cartesian Products. That is: given a set of options $A$ and a set of options $B$ the total range of possibilities of combing both is $A \times B$, the Cartesian Product of the sets $A$ and $B$.

<figure class="wide">

$$
\begin{align*}
A &= \{\text{a}, \text{b}, \text{c}\} \\
B &= \{1, 2, 3\} \\
A \times B &= \begin{Bmatrix}
  (\text{a}, 1) & (\text{a}, 2) & (\text{a}, 3) \\
  (\text{b}, 1) & (\text{b}, 2) & (\text{b}, 3) \\
  (\text{c}, 1) & (\text{c}, 2) & (\text{c}, 3)
\end{Bmatrix}
\end{align*}
$$

</figure>

The total number of possibilities is the size of each input set multiplied together.

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

What if we imagine that arrangements are the same as combined sets of possibilities? That we can choose an element (any element) from the set $A$ _'number of elements in $A$'_ (AKA $|A|$) times? We might then say that the total number of arrangements of the elements in $A$, is <nobr>$|A^{|A|}|$.</nobr>

<figure class="wide">

$$
\mathit{\text{No. arrangements of } A} = \overbrace{|A| \times |A| \times ... \times |A|}^{|A|}
$$

</figure>

More concretely.

<figure class="wide">

$$
\begin{align*}
A &= \{\text{a}, \text{b}, \text{c}\} \\
A^{|A|}
  &= A \times A \times A \\
  &= \begin{Bmatrix}
    (\text{a}, \text{a}, \text{a}) & (\text{a}, \text{a}, \text{b}) & (\text{a}, \text{a}, \text{c}) \\
    (\text{a}, \text{b}, \text{a}) & (\text{a}, \text{b}, \text{b}) & (\text{a}, \text{b}, \text{c}) \\
    (\text{a}, \text{c}, \text{a}) & (\text{a}, \text{c}, \text{b}) & (\text{a}, \text{c}, \text{c}) \\
    (\text{b}, \text{a}, \text{a}) & (\text{b}, \text{a}, \text{b}) & (\text{b}, \text{a}, \text{c}) \\
    (\text{b}, \text{b}, \text{a}) & (\text{b}, \text{b}, \text{b}) & (\text{b}, \text{b}, \text{c}) \\
    (\text{b}, \text{c}, \text{a}) & (\text{b}, \text{c}, \text{b}) & (\text{b}, \text{c}, \text{c}) \\
    (\text{c}, \text{a}, \text{a}) & (\text{c}, \text{a}, \text{b}) & (\text{c}, \text{a}, \text{c}) \\
    (\text{c}, \text{b}, \text{a}) & (\text{c}, \text{b}, \text{b}) & (\text{c}, \text{b}, \text{c}) \\
    (\text{c}, \text{c}, \text{a}) & (\text{c}, \text{c}, \text{b}) & (\text{c}, \text{c}, \text{c})
  \end{Bmatrix} \\
|A^{|A|}|
  &= |A| \times |A| \times |A| \\
  &= 27
\end{align*}
$$

</figure>

But we know that's not right. There are many things in there which aren't _arrangements_. Arranging a collection won't duplicate any of its elements, so things like $(\text{a}, \text{a}, \text{a})$ and $(\text{c}, \text{b}, \text{b})$ shouldn't be counted.

What we need is a method to count arrangements that accounts for elements being used only once. Because that's how we might lay out an arrangement of some collection. We'd select an item to start with, then select the next item from what's left, then the next from what's left, and so on until we've placed every item and run out of options to select from.

For example, given the following collection of items.

![The letters, 'a', 'b' and 'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/abc-large.webp){ loading=lazy width=630 height=378 }

We could arrange them like so.

| Step | Options | Selection | Result |
|:----:|-------------------|--------|--------|
| <span class="founders-grotesk">1</span> | ![The letters 'a', 'b' and 'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/abc.webp){ loading=lazy width=210 height=126 } | ![The letter 'b'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/b.webp){ loading=lazy width=210 height=126 } | ![The letter 'b'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/b.webp){ loading=lazy width=210 height=126 } |
| <span class="founders-grotesk">2</span> | ![The letters 'a' and 'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ac.webp){ loading=lazy width=210 height=126 } | ![The letter 'a'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/a.webp){ loading=lazy width=210 height=126 } | ![The letter 'b'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ba.webp){ loading=lazy width=210 height=126 } |
| <span class="founders-grotesk">3</span> | ![The letter  'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/c.webp){ loading=lazy width=210 height=126 } | ![The letter 'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/c.webp){ loading=lazy width=210 height=126 } | ![The letter 'b'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bac.webp){ loading=lazy width=210 height=126 } |

At each step the set of options reduces by one. Once 'b' is put down, only 'a' and 'c' are left. On the next step once 'a' is put down only 'c' is left. So, in general, rather than this.

<figure class="wide">

$$
\mathit{\text{No. arrangements of } A} = \overbrace{|A| \times |A| \times ... \times |A|}^{|A|}
$$

</figure>

What we have instead is this.

<figure class="wide">

$$
\mathit{\text{No. arrangements of } A} = |A| \times (|A| - 1) \times (|A| - 2) ... \times 1
$$

</figure>

For the 'a', 'b', 'c' example then we'd have.

<figure class="wide">

$$
\begin{align*}
A &= \{\text{a}, \text{b}, \text{c}\} \\
\mathit{\text{No. arrangements of } A}
  &= |A| \times (|A| - 1) \times 1 \\
  &= 3 \times 2 \times 1 \\
  &= 6
\end{align*}
$$

</figure>

And here they all are.

<div class="list-none grid grid-cols-3 grid-rows-2 gap-3">
  <div>
    <img width="210" height="126" loading="lazy" alt="The letters 'a', 'b' and 'c'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/abc.webp" />
  </div>
  <div>
    <img width="210" height="126" loading="lazy" alt="The letters 'a', 'c' and 'b'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/acb.webp" />
  </div>
  <div>
    <img width="210" height="126" loading="lazy" alt="The letters 'b', 'a' and 'c'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bac.webp" />
  </div>
  <div>
    <img width="210" height="126" loading="lazy" alt="The letters 'b', 'c' and 'a'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bca.webp" />
  </div>
  <div>
    <img width="210" height="126" loading="lazy" alt="The letters 'c', 'a' and 'b'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/cab.webp" />
  </div>
  <div>
    <img width="210" height="126" loading="lazy" alt="The letters 'c', 'b' and 'a'"src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/cba.webp" />
  </div>
</div>

The result you get from multiplying all the numbers less than some number is called the _factorial_ of that number. It's written like this: $n!$, and can be defined like this:

<figure class="wide">

$$
n! = n \times (n - 1) \times (n - 2) \times ... \times 2 \times 1
$$

</figure>

Right now we've got 419 books which can be sorted by a code. So how many ways could I have arranged them? What's the factorial of 419?

<figure class="wide">

$$
419 \times 418 \times 417 \times ... \times 3 \times 2 \times 1
$$

</figure>

It's this extremely large number.

2,<wbr/>908,<wbr/>421,<wbr/>057,<wbr/>896,<wbr/>340,<wbr/>474,<wbr/>361,<wbr/>403,<wbr/>432,<wbr/>093,<wbr/>534,<wbr/>061,<wbr/>335,<wbr/>411,<wbr/>576,<wbr/>803,<wbr/>237,<wbr/>495,<wbr/>067,<wbr/>620,<wbr/>711,<wbr/>879,<wbr/>592,<wbr/>887,<wbr/>139,<wbr/>553,<wbr/>095,<wbr/>110,<wbr/>943,<wbr/>225,<wbr/>026,<wbr/>262,<wbr/>406,<wbr/>188,<wbr/>778,<wbr/>320,<wbr/>680,<wbr/>189,<wbr/>451,<wbr/>711,<wbr/>755,<wbr/>153,<wbr/>962,<wbr/>269,<wbr/>093,<wbr/>225,<wbr/>791,<wbr/>463,<wbr/>391,<wbr/>282,<wbr/>309,<wbr/>784,<wbr/>673,<wbr/>974,<wbr/>891,<wbr/>888,<wbr/>521,<wbr/>144,<wbr/>472,<wbr/>954,<wbr/>435,<wbr/>530,<wbr/>212,<wbr/>518,<wbr/>938,<wbr/>132,<wbr/>427,<wbr/>719,<wbr/>386,<wbr/>733,<wbr/>483,<wbr/>011,<wbr/>170,<wbr/>577,<wbr/>979,<wbr/>186,<wbr/>407,<wbr/>238,<wbr/>784,<wbr/>514,<wbr/>071,<wbr/>038,<wbr/>335,<wbr/>200,<wbr/>586,<wbr/>427,<wbr/>023,<wbr/>669,<wbr/>168,<wbr/>853,<wbr/>348,<wbr/>715,<wbr/>855,<wbr/>934,<wbr/>206,<wbr/>590,<wbr/>666,<wbr/>886,<wbr/>642,<wbr/>210,<wbr/>446,<wbr/>229,<wbr/>488,<wbr/>444,<wbr/>697,<wbr/>075,<wbr/>683,<wbr/>231,<wbr/>304,<wbr/>763,<wbr/>674,<wbr/>216,<wbr/>898,<wbr/>857,<wbr/>425,<wbr/>205,<wbr/>633,<wbr/>114,<wbr/>990,<wbr/>510,<wbr/>459,<wbr/>902,<wbr/>307,<wbr/>876,<wbr/>760,<wbr/>425,<wbr/>583,<wbr/>148,<wbr/>615,<wbr/>724,<wbr/>632,<wbr/>751,<wbr/>661,<wbr/>133,<wbr/>203,<wbr/>237,<wbr/>905,<wbr/>432,<wbr/>668,<wbr/>028,<wbr/>736,<wbr/>816,<wbr/>553,<wbr/>087,<wbr/>322,<wbr/>961,<wbr/>245,<wbr/>249,<wbr/>766,<wbr/>243,<wbr/>189,<wbr/>634,<wbr/>620,<wbr/>342,<wbr/>650,<wbr/>104,<wbr/>310,<wbr/>228,<wbr/>160,<wbr/>720,<wbr/>464,<wbr/>432,<wbr/>084,<wbr/>583,<wbr/>406,<wbr/>587,<wbr/>932,<wbr/>793,<wbr/>842,<wbr/>882,<wbr/>817,<wbr/>312,<wbr/>083,<wbr/>420,<wbr/>305,<wbr/>011,<wbr/>614,<wbr/>174,<wbr/>094,<wbr/>373,<wbr/>347,<wbr/>653,<wbr/>327,<wbr/>314,<wbr/>151,<wbr/>733,<wbr/>173,<wbr/>716,<wbr/>990,<wbr/>349,<wbr/>453,<wbr/>642,<wbr/>618,<wbr/>332,<wbr/>498,<wbr/>813,<wbr/>131,<wbr/>703,<wbr/>848,<wbr/>734,<wbr/>139,<wbr/>461,<wbr/>889,<wbr/>356,<wbr/>833,<wbr/>404,<wbr/>468,<wbr/>988,<wbr/>162,<wbr/>294,<wbr/>586,<wbr/>549,<wbr/>217,<wbr/>704,<wbr/>913,<wbr/>958,<wbr/>653,<wbr/>349,<wbr/>142,<wbr/>669,<wbr/>023,<wbr/>627,<wbr/>237,<wbr/>073,<wbr/>393,<wbr/>370,<wbr/>291,<wbr/>667,<wbr/>739,<wbr/>306,<wbr/>611,<wbr/>350,<wbr/>729,<wbr/>516,<wbr/>327,<wbr/>327,<wbr/>902,<wbr/>034,<wbr/>218,<wbr/>650,<wbr/>157,<wbr/>031,<wbr/>351,<wbr/>111,<wbr/>738,<wbr/>938,<wbr/>367,<wbr/>328,<wbr/>649,<wbr/>342,<wbr/>699,<wbr/>827,<wbr/>177,<wbr/>333,<wbr/>219,<wbr/>328,<wbr/>746,<wbr/>468,<wbr/>751,<wbr/>048,<wbr/>295,<wbr/>766,<wbr/>233,<wbr/>367,<wbr/>259,<wbr/>458,<wbr/>382,<wbr/>317,<wbr/>867,<wbr/>840,<wbr/>592,<wbr/>639,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000.

And here's the kind of question that led to my current cosy combinatorial corner: given a sorted list of all possible arrangements, what is the (_deep breath_) 2,<wbr/>844,<wbr/>113,<wbr/>050,<wbr/>416,<wbr/>543,<wbr/>052,<wbr/>622,<wbr/>601,<wbr/>377,<wbr/>724,<wbr/>177,<wbr/>730,<wbr/>794,<wbr/>635,<wbr/>580,<wbr/>558,<wbr/>165,<wbr/>101,<wbr/>289,<wbr/>087,<wbr/>907,<wbr/>211,<wbr/>046,<wbr/>617,<wbr/>413,<wbr/>162,<wbr/>202,<wbr/>727,<wbr/>992,<wbr/>682,<wbr/>826,<wbr/>505,<wbr/>610,<wbr/>602,<wbr/>434,<wbr/>122,<wbr/>752,<wbr/>894,<wbr/>055,<wbr/>956,<wbr/>478,<wbr/>001,<wbr/>246,<wbr/>987,<wbr/>975,<wbr/>654,<wbr/>867,<wbr/>880,<wbr/>927,<wbr/>440,<wbr/>389,<wbr/>088,<wbr/>878,<wbr/>653,<wbr/>260,<wbr/>211,<wbr/>539,<wbr/>374,<wbr/>415,<wbr/>866,<wbr/>208,<wbr/>792,<wbr/>187,<wbr/>943,<wbr/>989,<wbr/>161,<wbr/>515,<wbr/>680,<wbr/>780,<wbr/>226,<wbr/>637,<wbr/>325,<wbr/>041,<wbr/>170,<wbr/>571,<wbr/>521,<wbr/>944,<wbr/>136,<wbr/>857,<wbr/>756,<wbr/>357,<wbr/>610,<wbr/>408,<wbr/>690,<wbr/>480,<wbr/>987,<wbr/>642,<wbr/>359,<wbr/>289,<wbr/>070,<wbr/>142,<wbr/>617,<wbr/>861,<wbr/>100,<wbr/>311,<wbr/>817,<wbr/>255,<wbr/>304,<wbr/>659,<wbr/>710,<wbr/>866,<wbr/>175,<wbr/>534,<wbr/>883,<wbr/>866,<wbr/>241,<wbr/>789,<wbr/>715,<wbr/>155,<wbr/>834,<wbr/>223,<wbr/>460,<wbr/>969,<wbr/>127,<wbr/>343,<wbr/>122,<wbr/>506,<wbr/>553,<wbr/>271,<wbr/>818,<wbr/>293,<wbr/>567,<wbr/>727,<wbr/>306,<wbr/>529,<wbr/>166,<wbr/>052,<wbr/>023,<wbr/>268,<wbr/>045,<wbr/>186,<wbr/>141,<wbr/>937,<wbr/>347,<wbr/>041,<wbr/>818,<wbr/>966,<wbr/>122,<wbr/>038,<wbr/>640,<wbr/>793,<wbr/>796,<wbr/>799,<wbr/>685,<wbr/>516,<wbr/>455,<wbr/>929,<wbr/>549,<wbr/>507,<wbr/>013,<wbr/>186,<wbr/>833,<wbr/>430,<wbr/>265,<wbr/>450,<wbr/>466,<wbr/>044,<wbr/>505,<wbr/>744,<wbr/>178,<wbr/>793,<wbr/>393,<wbr/>254,<wbr/>499,<wbr/>311,<wbr/>497,<wbr/>130,<wbr/>361,<wbr/>821,<wbr/>989,<wbr/>719,<wbr/>697,<wbr/>224,<wbr/>981,<wbr/>301,<wbr/>053,<wbr/>842,<wbr/>088,<wbr/>290,<wbr/>183,<wbr/>796,<wbr/>995,<wbr/>774,<wbr/>643,<wbr/>108,<wbr/>623,<wbr/>128,<wbr/>572,<wbr/>424,<wbr/>596,<wbr/>295,<wbr/>207,<wbr/>864,<wbr/>719,<wbr/>379,<wbr/>479,<wbr/>419,<wbr/>093,<wbr/>894,<wbr/>525,<wbr/>035,<wbr/>709,<wbr/>405,<wbr/>252,<wbr/>305,<wbr/>572,<wbr/>483,<wbr/>849,<wbr/>439,<wbr/>595,<wbr/>150,<wbr/>582,<wbr/>362,<wbr/>925,<wbr/>993,<wbr/>365,<wbr/>553,<wbr/>189,<wbr/>173,<wbr/>069,<wbr/>073,<wbr/>069,<wbr/>239,<wbr/>396,<wbr/>792,<wbr/>666,<wbr/>051,<wbr/>139,<wbr/>882,<wbr/>098,<wbr/>736,<wbr/>318,<wbr/>227,<wbr/>488,<wbr/>496,<wbr/>884,<wbr/>365,<wbr/>414,<wbr/>402,<wbr/>236,<wbr/>391,<wbr/>348,<wbr/>245,<wbr/>541,<wbr/>096,<wbr/>370,<wbr/>960,<wbr/>228,<wbr/>964,<wbr/>587,<wbr/>593,<wbr/>160,<wbr/>349,<wbr/>264,<wbr/>902,<wbr/>263,<wbr/>727,<wbr/>302,<wbr/>169,<wbr/>421,<wbr/>863,<wbr/>892,<wbr/>154,<wbr/>866,<wbr/>370,<wbr/>306,<wbr/>041,<wbr/>614,<wbr/>127,<wbr/>727,<wbr/>830,<wbr/>967,<wbr/>236,<wbr/>995,<wbr/>581,<wbr/>667,<wbr/>801,<wbr/>392,<wbr/>480,<wbr/>447,<wbr/>679,<wbr/>422,<wbr/>359,<wbr/>386,<wbr/>205,<wbr/>525,<wbr/>674,<wbr/>979,<wbr/>615,<wbr/>059,<wbr/>536,<wbr/>733,<wbr/>032,<wbr/>646,<wbr/>807,<wbr/>296,<wbr/>164,<wbr/>119,<wbr/>104,<wbr/>440,<wbr/>573,<wbr/>184,<wbr/>560,<wbr/>777,<wbr/>118,<wbr/>243rd one? You know &hellip; for example.

## Lexicographic order

How might arrangements be sorted? To answer that I'd need to define when one arrangement should come before another. Here's a scheme.

* Step through pairs of elements from both arrangements. I.E. First look at the first element of arrangement $x$ and the first element from arrangement $y$, then the second element from arrangement $x$ and the second element from arrangement $y$, and so on.
* For the first pair with mismatching elements, if the element from $x$ should come before the element from $y$ then the whole arrangement $x$ should come before the arrangement <nobr>$y$.</nobr>

For example, here's two arrangements of the first three letters of the alphabet.

<figure class="wide">

$$
\begin{align*}
  x &= (\text{b}, \text{c}, \text{a}) \\
  y &= (\text{b}, \text{a}, \text{c})
\end{align*}
$$

</figure>

1. Compare the first element of each ordering: $x_1 = \text{b}$ and $y_1 = \text{b}$. They match, so move onto the next pair of elements.
2. Compare the second element of each ordering: $x_2 = \text{c}$ and $y_2 = \text{a}$. They don't match and $\text{a}$ comes before $\text{b}$, therefore $y$ comes before $x$.

Sorting elements this way will result in them being in [_Lexicographic order_](https://en.wikipedia.org/wiki/Lexicographic_order), and it might seem like the way mostly everything is sorted in day to day life &hellip; because it is! This is how we sort words in the dictionary, hence the name.

## Indexing

The example above sorts arrangements with reference to a pre-existing ordering of their elements: the alphabet. But it didn't _have_ to. We could've used some other ordering, and ended up with a different result. If we lived in an alternate timeline where the Latin alphabet began $(\text{c}, \text{a}, \text{b})$ rather than $(\text{a}, \text{b}, \text{c})$ then the order of our example arrangements would be reversed and $x$ would come before <nobr>$y$.</nobr>

The alphabet we have is one possible arrangement. One of 403,<wbr/>291,<wbr/>461,<wbr/>126,<wbr/>605,<wbr/>635,<wbr/>584,<wbr/>000,<wbr/>000 options in fact. To reduce confusion when talking about any alternative arrangements we can choose to ignore the specific elements of whatever collection we're arranging and instead refer only to their position in some select _conventional_ ordering. The alphabet is the convention for letters, where 'a' is first, 'b' is second, 'c' is third, and so on. We can then discuss other arrangements by saying only where elements 1, 2, and 3 are. For example, the arrangement $(\text{b}, \text{a}, \text{c})$ becomes $(2, 1, 3)$.

When we can use a sequence of numbers to refer to all the elements of a collection we're able to say that the collection is _indexed_ by those numbers. The numbers _point to_ elements in the collection.[^2]

For our books I'll use _Alphabetical by title_ as the conventional ordering. This then defines the Lexicographic order for the list of possible arrangements.

## Lehmer codes

Right, here we go: Lehmer codes index all possible arrangements of collections of elements, and they do so in Lexicographic order.

To see how we can revisit my earlier example of laying out the first three letters of the alphabet.

| Step | Options | Selection | Result |
|:----:|-------------------|--------|--------|
| <span class="founders-grotesk">1</span> | ![The letters 'a', 'b' and 'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/abc.webp){ loading=lazy width=210 height=126 } | ![The letter 'b'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/b.webp){ loading=lazy width=210 height=126 } | ![The letter 'b'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/b.webp){ loading=lazy width=210 height=126 } |
| <span class="founders-grotesk">2</span> | ![The letters 'a' and 'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ac.webp){ loading=lazy width=210 height=126 } | ![The letter 'a'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/a.webp){ loading=lazy width=210 height=126 } | ![The letter 'b'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ba.webp){ loading=lazy width=210 height=126 } |
| <span class="founders-grotesk">3</span> | ![The letter  'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/c.webp){ loading=lazy width=210 height=126 } | ![The letter 'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/c.webp){ loading=lazy width=210 height=126 } | ![The letter 'b'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bac.webp){ loading=lazy width=210 height=126 } |

Lehmer codes end up recording the steps in this procedure as a sequence of choices of index. Like so:

1. Select the second option.
2. Select the first option from what's left.
3. Select the first option from what's left.

Now, the indexes in Lehmer codes all begin at 0, rather than 1, so for the arrangement: $(\text{b}, \text{a}, \text{c})$, we have this sequence of choices: <nobr>$(1, 0, 0)$.</nobr>

Here are _all_ the choices someone could make when laying out the first three letters of the alphabet.

<table class="table-style-none table-fixed founders-grotesk">
  <thead>
    <tr class="b">
      <th>Step 1</th>
      <th>Step 2</th>
      <th>Step 3</th>
    </tr>
  </thead>
  <tbody class="f6">
    <tr>
      <td rowspan="2" class="bt b--moon-gray pr2">
        <p>0 from (a, b, c)</p>
        <img width="210" height="126" loading="lazy" alt="The letter 'a'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/a.webp" />
      </td>
      <td class="bt b--moon-gray pr2">
        <p>0 from (b, c)</p>
        <img width="210" height="126" loading="lazy" alt="The letters 'a' and 'b'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ab.webp" />
      </td>
      <td class="bt b--moon-gray">
        <p>0 from (c)</p>
        <img width="210" height="126" loading="lazy" alt="The letters 'a', 'b' and 'c'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/abc.webp" />
      </td>
    </tr>
    <tr>
      <td class="pr2 pb2">
        <p>1 from (b, c)</p>
        <img width="210" height="126" loading="lazy" alt="The letters 'a' and 'c'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ac.webp" />
      </td>
      <td class="pb2">
        <p>0 from (b)</p>
        <img width="210" height="126" loading="lazy" alt="The letters 'a', 'c' and 'b'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/acb.webp" />
      </td>
    </tr>
    <tr>
      <td rowspan="2" class="bt b--moon-gray pr2">
        <p>1 from (a, b, c)</p>
        <img width="210" height="126" loading="lazy" alt="The letter 'b'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/b.webp" />
      </td>
      <td class="bt b--moon-gray pr2">
        <p>0 from (a, c)</p>
        <img width="210" height="126" loading="lazy" alt="The letters 'b' and 'a'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ba.webp" />
      </td>
      <td class="bt b--moon-gray">
        <p>0 from (c)</p>
        <img width="210" height="126" loading="lazy" alt="The letters 'b', 'a', and 'c'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bac.webp" />
      </td>
    </tr>
    <tr>
      <td class="pr2 pb2">
        <p>1 from (a, c)</p>
        <img width="210" height="126" loading="lazy" alt="The letters 'b' and 'c'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bc.webp" />
      </td>
      <td class="pb2">
        <p>0 from (a)</p>
        <img width="210" height="126" loading="lazy" alt="The letters 'b', 'c' and 'a'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bca.webp" />
      </td>
    </tr>
    <tr>
      <td rowspan="2" class="bt b--moon-gray pr2">
        <p>2 from (a, b, c)</p>
        <img width="210" height="126" loading="lazy" alt="The letter 'c'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/c.webp" />
      </td>
      <td class="bt b--moon-gray pr2">
        <p>0 from (a, b)</p>
        <img width="210" height="126" loading="lazy" alt="The letters 'c' and 'a'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ca.webp" />
      </td>
      <td class="bt b--moon-gray">
        <p>0 from (b)</p>
        <img width="210" height="126" loading="lazy" alt="The letters 'c', 'a' and 'b'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/cab.webp" />
      </td>
    </tr>
    <tr>
      <td class="pr2 pb2">
        <p>1 from (a, b)</p>
        <img width="210" height="126" loading="lazy" alt="The letters 'c' and 'b'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/cb.webp" />
      </td>
      <td class="pb2">
        <p>0 from (a)</p>
        <img width="210" height="126" loading="lazy" alt="The letters 'c', 'b' and 'a'" src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/cba.webp" />
      </td>
    </tr>
  </tbody>
</table>

Which can be condensed into this list of sequences.

* $(0, 0, 0)$
* $(0, 1, 0)$
* $(1, 0, 0)$
* $(1, 1, 0)$
* $(2, 0, 0)$
* $(2, 1, 0)$

And here's the nifty trick: this is counting, but it's counting in a number system quite unlike the one we use day to day.

At the first step there are $3!$ (a.k.a $3 \times 2 \times 1$) possible outcomes, then at the second step there are $2!$. I could also say that at the second step there are $(3 \times 2 \times 1) / 3$ choices. It's the same thing, but by saying it that way I'm trying to emphasise that the first choice puts the rest of the sequence into one of three branches.

I could also try to emphasize that this way: when you choose 'b' you _skip over_ the $2!$ possibilities you might've reached if you'd chosen 'a'. When you choose 'c' you _skip over_ the $2!$ possibilities from choosing 'a', and also the $2!$ from 'b'. So when choosing 'c' you skip over $2 \times 2!$ possibilities.

When viewed this way it's is not _completely_ unlike ordinary counting. If asked to find the 243rd _something_ you could _skip over_ the first 200 things, then you could _skip over_ the next 40 things, finally arriving at the 3rd thing after that. By choosing 2 in the hundreds place this number goes down the branch where the range of possibilities is within $200 \leq n \lt 300$.

What's different in the case of these sequences of index choices is that instead of hundreds, tens and ones we have <nobr>$2!$s</nobr>, <nobr>$1!$s</nobr> and <nobr>$0!$s</nobr>.

What happens then if we multiply each index in the above list of sequences by its factorial place value? (Just as we do with ordinary numbers.)

* $(0 \times 2!, 0 \times 1!, 0 \times 0!) = (0, 0, 0)$
* $(0 \times 2!, 1 \times 1!, 0 \times 0!) = (0, 1, 0)$
* $(1 \times 2!, 0 \times 1!, 0 \times 0!) = (2, 0, 0)$
* $(1 \times 2!, 1 \times 1!, 0 \times 0!) = (2, 1, 0)$
* $(2 \times 2!, 0 \times 1!, 0 \times 0!) = (4, 0, 0)$
* $(2 \times 2!, 1 \times 1!, 0 \times 0!) = (4, 1, 0)$

Interesting, and now if we add the places together? (Again, just as we do with ordinary numbers.)

* $0 + 0 + 0 = 0$
* $0 + 1 + 0 = 1$
* $2 + 0 + 0 = 2$
* $2 + 1 + 0 = 3$
* $4 + 0 + 0 = 4$
* $4 + 1 + 0 = 5$

What we're looking at here is the [_Factorial number system_](https://en.wikipedia.org/wiki/Factorial_number_system), and how factorial numbers can be converted into ordinary, decimal numbers.

Now, factorial numbers are not just factorial numbers, they are also Lehmer codes. The number 2 is the decimal representation of the Lehmer code $(1, 0, 0)$. Which is a sequence of choices made when laying out an arrangement of elements. It's possible to transform that sequence of choices into indexes into the original collection. If we do so with the sequence $(1, 0, 0)$ we get these indexes: $(1, 0, 2)$. Following those indexes back to the letters they point to we get <nobr>$(\text{b}, \text{a}, \text{c})$.</nobr>

This gives me everything I need to answer my silly question: What is the 2,<wbr/>844, &hellip; 243rd possible arrangement of our 5-ish badgers worth of books? I only have to take that number, subtract one, convert that from decimal to factorial, and then use those digits as _instructions_ for laying out an arrangement. This all happens very quickly. It takes about two hundredths of a second to go from an enormous number to a list of books.

```
> BS.writeFile "sorted_books.json" (Aeson.encode (decodeLehmer books (toFactorial n)))
(0.02 secs, 17,994,448 bytes)
```

```javascript
[
  {
    "author": "Tufte, Edward R.",
    "code": "001.4226",
    "title": "The Visual Display of Quantitative Information"
  },
  {
    "author": "Weizenbaum, Joseph",
    "code": "001.64",
    "title": "Computer Power and Human Reason"
  },
  {
    "author": "Gleick, James",
    "code": "003.857",
    "title": "Chaos: Making a New Science"
  },
  {
    "author": "Galloway, Alexander",
    "code": "004.09",
    "title": "Uncomputable: Play and Politics In the Long Digital Age"
    // ...
```

I think that's really something.

***

Having written this I can think of a couple of reasons I've found Lehmer codes so interesting.

Firstly, there's something indescribably cool about how they work. Embedded in the numbers of this very strange number system is the procedure for encoding and decoding them as arrangments of elements. The strange number system is perfectly suited to encoding and decoding arrangements, it's factorial! This, to me, feels simultaneously obvious and obscure and I love it.

Secondly, when I first came across Lehmer codes I was unaware of so many of the foundations upon which they're built, and it left me a bit awe struck. I was overwhelmed thinking about all the _epistemic iterations_[^3] it would take to arrive at something like this. All the large and small thoughts, and all the people who thought them.

At the end of the day I have no idea if Lehmer codes will ever prove to be practically useful to me. Regardless, I do know that I find them wonderful.

[^1]: We were very sad to see Logical Unsanity close. The ABC did a nice feature at the time: [_Quirky tin shed bookshop 'born out of laziness' offers booklovers' sanctuary_](https://www.abc.net.au/news/2018-04-08/quirky-tin-shed-bookshop-offers-booklovers-sanctuary-brisbane/9618992)
[^2]:
    > &hellip; from _indicare_ "to point out," &hellip;

    [Index - Etymology, Origin & Meaning](https://www.etymonline.com/word/index)
[^3]: This is a term I've just come across from reading [_Beyond Measure_ by James Vincent](https://jamesvincent.info/beyond-measure) (2022). It was coined by Hasok Chang in his book _Inventing Temperature_ (2004).
