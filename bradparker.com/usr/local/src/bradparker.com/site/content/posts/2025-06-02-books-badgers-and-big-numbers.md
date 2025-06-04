---
title: Books, Badgers and Big Numbers
tags:
  - mathematics
  - books
description: |
  TBD
---

Up until April 2023 we were lucky enough to have a pretty special second hand book shop up the road, next to the local café[^1]. While it was there my mid morning coffee runs usually involved having a browse and I often picked something. So, yes, during this time our book collection grew a bit.

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

A lot of what I was buying was non-fiction, and because of this I started to probably over think the organisation of all these books. I wanted everything organised by subject, rather than author. So in the end I signed up for a [LibraryThing](https://www.librarything.com) account and used their codes to sort everything. I'm on the lookout for an alternative as I'd prefer something more standard.

Some time last year LibraryThing let me know they'd made me a Year In Review. You know, like Spotify does. It's not really my jam, but I clicked through anyway and ended up reading this sentence:

> And make sure your floors will support the added weight of 4.90 adult badgers!

I'd scanned in all our books at once, so this doesn't represent how many books we added to our collection in 2024, but rather the whole lot. At any rate, now whenever I look at our bookshelves I find myself trying to imagine five adult badgers somehow sitting in them. Which is hard, given that I have no idea how big an adult badger is.

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

What if we imagine that orderings are the same as combined sets of possibilities? We might say that the total number of orderings for some collection of things, $A$, is $|A^{|A|}|$.

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

What we need is a counting method that accounts for elements being used only once. Because that's how we might lay out an ordering of some collection. We'd choose and item to start with, then choose the next item from what's left, then the next from what's left, and so on until we run out of items.

For example, given the following collection of items.

![The letters, 'a', 'b' and 'c'](/content/posts/books-badgers-and-big-numbers/assets/images/permutations/abc-large.webp)

We could come up with an ordering like this.

<ol class="list-none flex flex-row gap-3 founders-grotesk f6">
  <li>
    <p>Start with 'b'</p>
    <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/b.webp" />
  </li>
  <li>
    <p>Then choose 'a'</p>
    <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ba.webp" />
  </li>
  <li>
    <p>Finally, choose 'c'<p>
    <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bac.webp" />
  </li>
</ol>

At each step the set of available choices reduces by one. Once 'b' is put down, only 'a' and 'c' are left. On the next step once 'a' is put down only 'c' is left.

<ul class="list-none flex flex-col gap-3 founders-grotesk f6">
  <li class="grid grid-cols-3 gap-3 bt b--moon-gray pt1">
    <figure class="col-span-1 col-start-1">
      <p>Start with 'a'</p>
      <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/a.webp" />
    </figure>
    <ul class="col-span-2 col-start-2 list-none flex flex-col gap-3">
      <li class="flex flex-row gap-3">
        <figure>
          <p>Then choose 'b'</p>
          <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ab.webp" />
        </figure>
        <figure>
          <p>Finally, choose 'c'</p>
          <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/abc.webp" />
        </figure>
      </li>
      <li class="flex flex-row gap-3 bt b--moon-gray pt1">
        <figure>
          <p>Then choose 'c'</p>
          <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ac.webp" />
        </figure>
        <figure>
          <p>Finally, choose 'b'</p>
          <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/acb.webp" />
        </figure>
      </li>
    </ul>
  </li>
  <li class="grid grid-cols-3 gap-3 bt b--moon-gray pt1">
    <figure class="col-span-1 col-start-1">
      <p>Start with 'b'</p>
      <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/b.webp" />
    </figure>
    <ul class="col-span-2 col-start-2 list-none flex flex-col gap-3">
      <li class="flex flex-row gap-3">
        <figure>
          <p>Then choose 'a'</p>
          <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ba.webp" />
        </figure>
        <figure>
          <p>Finally, choose 'c'</p>
          <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bac.webp" />
        </figure>
      </li>
      <li class="flex flex-row gap-3 bt b--moon-gray pt3">
        <figure>
          <p>Then choose 'c'</p>
          <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bc.webp" />
        </figure>
        <figure>
          <p>Finally, choose 'a'</p>
          <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/bca.webp" />
        </figure>
      </li>
    </ul>
  </li>
  <li class="grid grid-cols-3 gap-3 bt b--moon-gray pt3">
    <figure class="col-span-1 col-start-1">
      <p>Start with 'c'</p>
      <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/c.webp" />
    </figure>
    <ul class="col-span-2 col-start-2 list-none flex flex-col gap-3">
      <li class="flex flex-row gap-3">
        <figure>
          <p>Then choose 'a'</p>
          <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/ca.webp" />
        </figure>
        <figure>
          <p>Finally, choose 'b'</p>
          <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/cab.webp" />
        </figure>
      </li>
      <li class="flex flex-row gap-3 bt b--moon-gray pt3">
        <figure>
          <p>Then choose 'b'</p>
          <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/cb.webp" />
        </figure>
        <figure>
          <p>Finally, choose 'c'</p>
          <img src="/content/posts/books-badgers-and-big-numbers/assets/images/permutations/cba.webp" />
        </figure>
      </li>
    </ul>
  </li>
</ul>

<figure class="wide">

$$
434 \times 433 \times 432 \times ... \times 3 \times 2 \times 1
$$

</figure>

434! = 8,<wbr/>028,<wbr/>892,<wbr/>649,<wbr/>624,<wbr/>507,<wbr/>601,<wbr/>936,<wbr/>521,<wbr/>509,<wbr/>067,<wbr/>824,<wbr/>511,<wbr/>686,<wbr/>968,<wbr/>053,<wbr/>222,<wbr/>904,<wbr/>069,<wbr/>946,<wbr/>378,<wbr/>440,<wbr/>541,<wbr/>748,<wbr/>977,<wbr/>700,<wbr/>138,<wbr/>353,<wbr/>696,<wbr/>203,<wbr/>385,<wbr/>301,<wbr/>691,<wbr/>285,<wbr/>462,<wbr/>815,<wbr/>926,<wbr/>748,<wbr/>932,<wbr/>336,<wbr/>257,<wbr/>712,<wbr/>955,<wbr/>947,<wbr/>969,<wbr/>182,<wbr/>091,<wbr/>571,<wbr/>461,<wbr/>054,<wbr/>745,<wbr/>662,<wbr/>230,<wbr/>708,<wbr/>466,<wbr/>615,<wbr/>201,<wbr/>971,<wbr/>163,<wbr/>027,<wbr/>947,<wbr/>784,<wbr/>592,<wbr/>188,<wbr/>345,<wbr/>929,<wbr/>626,<wbr/>403,<wbr/>698,<wbr/>853,<wbr/>968,<wbr/>993,<wbr/>964,<wbr/>025,<wbr/>478,<wbr/>364,<wbr/>058,<wbr/>339,<wbr/>733,<wbr/>723,<wbr/>943,<wbr/>340,<wbr/>173,<wbr/>184,<wbr/>101,<wbr/>884,<wbr/>219,<wbr/>243,<wbr/>457,<wbr/>415,<wbr/>247,<wbr/>678,<wbr/>123,<wbr/>543,<wbr/>086,<wbr/>550,<wbr/>521,<wbr/>480,<wbr/>123,<wbr/>836,<wbr/>096,<wbr/>462,<wbr/>669,<wbr/>547,<wbr/>394,<wbr/>885,<wbr/>664,<wbr/>998,<wbr/>503,<wbr/>406,<wbr/>868,<wbr/>866,<wbr/>940,<wbr/>320,<wbr/>706,<wbr/>575,<wbr/>402,<wbr/>823,<wbr/>809,<wbr/>278,<wbr/>057,<wbr/>871,<wbr/>002,<wbr/>290,<wbr/>016,<wbr/>357,<wbr/>531,<wbr/>261,<wbr/>447,<wbr/>881,<wbr/>390,<wbr/>164,<wbr/>282,<wbr/>724,<wbr/>841,<wbr/>495,<wbr/>176,<wbr/>551,<wbr/>408,<wbr/>215,<wbr/>269,<wbr/>014,<wbr/>819,<wbr/>979,<wbr/>858,<wbr/>680,<wbr/>458,<wbr/>472,<wbr/>322,<wbr/>643,<wbr/>774,<wbr/>958,<wbr/>048,<wbr/>829,<wbr/>672,<wbr/>032,<wbr/>578,<wbr/>860,<wbr/>318,<wbr/>932,<wbr/>103,<wbr/>013,<wbr/>588,<wbr/>330,<wbr/>599,<wbr/>802,<wbr/>172,<wbr/>529,<wbr/>112,<wbr/>189,<wbr/>982,<wbr/>454,<wbr/>301,<wbr/>455,<wbr/>955,<wbr/>613,<wbr/>789,<wbr/>120,<wbr/>637,<wbr/>007,<wbr/>898,<wbr/>176,<wbr/>770,<wbr/>864,<wbr/>310,<wbr/>094,<wbr/>708,<wbr/>897,<wbr/>240,<wbr/>717,<wbr/>841,<wbr/>828,<wbr/>852,<wbr/>901,<wbr/>221,<wbr/>780,<wbr/>301,<wbr/>710,<wbr/>846,<wbr/>809,<wbr/>590,<wbr/>971,<wbr/>467,<wbr/>157,<wbr/>175,<wbr/>099,<wbr/>665,<wbr/>642,<wbr/>185,<wbr/>789,<wbr/>889,<wbr/>404,<wbr/>747,<wbr/>813,<wbr/>147,<wbr/>120,<wbr/>457,<wbr/>368,<wbr/>192,<wbr/>691,<wbr/>464,<wbr/>675,<wbr/>844,<wbr/>003,<wbr/>872,<wbr/>800,<wbr/>253,<wbr/>947,<wbr/>050,<wbr/>152,<wbr/>187,<wbr/>714,<wbr/>796,<wbr/>056,<wbr/>813,<wbr/>051,<wbr/>446,<wbr/>306,<wbr/>795,<wbr/>828,<wbr/>848,<wbr/>716,<wbr/>540,<wbr/>343,<wbr/>208,<wbr/>022,<wbr/>526,<wbr/>738,<wbr/>489,<wbr/>519,<wbr/>205,<wbr/>964,<wbr/>750,<wbr/>583,<wbr/>046,<wbr/>305,<wbr/>921,<wbr/>291,<wbr/>594,<wbr/>659,<wbr/>067,<wbr/>305,<wbr/>557,<wbr/>334,<wbr/>115,<wbr/>081,<wbr/>428,<wbr/>648,<wbr/>206,<wbr/>770,<wbr/>503,<wbr/>166,<wbr/>770,<wbr/>246,<wbr/>730,<wbr/>393,<wbr/>378,<wbr/>224,<wbr/>194,<wbr/>474,<wbr/>390,<wbr/>485,<wbr/>016,<wbr/>248,<wbr/>320,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000,<wbr/>000

[^1]: We were very sad to see Logical Unsanity close. The ABC did a nice feature at the time: [_Quirky tin shed bookshop 'born out of laziness' offers booklovers' sanctuary_](https://www.abc.net.au/news/2018-04-08/quirky-tin-shed-bookshop-offers-booklovers-sanctuary-brisbane/9618992)
