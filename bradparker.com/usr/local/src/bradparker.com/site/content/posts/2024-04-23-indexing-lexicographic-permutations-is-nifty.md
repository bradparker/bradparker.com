---
title: Indexing lexicographic permutations is nifty
tags:
  - development
description: |
  TBD
---

Lehmer codes encode permutations of ordered sets.

## Two-up

Two-up is a game played in Australia. Someone tosses two coins in the air at the same time and people place bets on the outcome. It is forbidden by law but for one day of the year, ANZAC day. If you want to get a feeling for how betting on a coin toss could be outlawed, and otherwise just enjoy a great if but harrowing novel, I reckon [Wake in Fright](https://en.wikipedia.org/wiki/Wake_in_Fright) is worth your time.

How many possible states can two coins be in? The first coin can be heads or tails, and the second coin can be heads or tails.

<div class="grid gap-3 grid-rows-2 grid-cols-2">
  <figure>
    <img src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/heads-heads.jpeg.webp" />
    <figcaption>
      First heads, second heads
    </figcaption>
  </figure>
  <figure>
    <img src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/heads-tails.jpeg.webp" />
    <figcaption>
      First heads, second tails
    </figcaption>
  </figure>
  <figure>
    <img src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/tails-heads.jpeg.webp" />
    <figcaption>
      First tails, second heads
    </figcaption>
  </figure>
  <figure>
    <img src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/tails-tails.jpeg.webp" />
    <figcaption>
      First tails, second tails
    </figcaption>
  </figure>
</div>

There are two coins, each with two sides they could land on.

<figure class="figure">
  <em>No. of sides coin 1</em> &times; <em>No. of sides coin 2</em> = <em>No. of possibilities</em>
</figure>

So.

<figure class="figure">
  <p>2 &times; 2 = 4</p>
</figure>

In Two-up two of these possibilities are counted as the same outcome. The order of the coins doesn't matter, which is handy, keeping track of them flying through the air, a few pints deep, would surely be diabolical. So one outcome ends up being twice as likely as the other two. Sounds simple, ends up pretty complicated for [John Grant](https://en.wikipedia.org/wiki/Wake_in_Fright).

## Hazard

Hazard is a game that was mostly played a long time in England. Someone rolls a pair of dice and places bets on the outcome. The rules for assessing the success or failure of an outcome are fairly complicated. It's referenced in Chaucer's _Canterbury Tales_ in a passage that may have given rise to the idiom "at sixes and sevens", which refers to being in a state of confusion or disarray. I've never read _Canterbury tales_, I suspect if I tried I'd end up at sixes and sevens.

How many possible states can two dice be in? The first dice can have any one of the numbers one to six face up, as can the second.

<div class="founders-grotesk">
  <table class="table-fixed min-w-full border-separate border-spacing-3">
    <tbody>
      <tr>
        <th class="text-center"></th>
        <th class="text-center">1</th>
        <th class="text-center">2</th>
        <th class="text-center">3</th>
        <th class="text-center">4</th>
        <th class="text-center">5</th>
        <th class="text-center">6</th>
      </tr>
      <tr>
        <th class="align-middle">1</th>
        <td>
          <img alt="Two dice. The first with the 1 side up, the second with the 1 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/1-1.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 1 side up, the second with the 2 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/1-2.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 1 side up, the second with the 3 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/1-3.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 1 side up, the second with the 4 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/1-4.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 1 side up, the second with the 5 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/1-5.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 1 side up, the second with the 6 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/1-6.JPEG.190.webp" />
        </td>
      </tr>
      <tr>
        <th class="align-middle">2</th>
        <td>
          <img alt="Two dice. The first with the 2 side up, the second with the 1 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/2-1.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 2 side up, the second with the 2 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/2-2.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 2 side up, the second with the 3 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/2-3.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 2 side up, the second with the 4 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/2-4.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 2 side up, the second with the 5 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/2-5.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 2 side up, the second with the 6 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/2-6.JPEG.190.webp" />
        </td>
      </tr>
      <tr>
        <th class="align-middle">3</th>
        <td>
          <img alt="Two dice. The first with the 3 side up, the second with the 1 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/3-1.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 3 side up, the second with the 2 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/3-2.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 3 side up, the second with the 3 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/3-3.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 3 side up, the second with the 4 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/3-4.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 3 side up, the second with the 5 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/3-5.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 3 side up, the second with the 6 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/3-6.JPEG.190.webp" />
        </td>
      </tr>
      <tr>
        <th class="align-middle">4</th>
        <td>
          <img alt="Two dice. The first with the 4 side up, the second with the 1 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/4-1.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 4 side up, the second with the 2 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/4-2.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 4 side up, the second with the 3 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/4-3.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 4 side up, the second with the 4 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/4-4.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 4 side up, the second with the 5 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/4-5.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 4 side up, the second with the 6 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/4-6.JPEG.190.webp" />
        </td>
      </tr>
      <tr>
        <th class="align-middle">5</th>
        <td>
          <img alt="Two dice. The first with the 5 side up, the second with the 1 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/5-1.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 5 side up, the second with the 2 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/5-2.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 5 side up, the second with the 3 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/5-3.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 5 side up, the second with the 4 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/5-4.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 5 side up, the second with the 5 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/5-5.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 5 side up, the second with the 6 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/5-6.JPEG.190.webp" />
        </td>
      </tr>
      <tr>
        <th class="align-middle">6</th>
        <td>
          <img alt="Two dice. The first with the 6 side up, the second with the 1 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/6-1.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 6 side up, the second with the 2 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/6-2.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 6 side up, the second with the 3 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/6-3.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 6 side up, the second with the 4 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/6-4.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 6 side up, the second with the 5 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/6-5.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 6 side up, the second with the 6 side up" src="/content/posts/indexing-lexicographic-permutations-is-nifty/assets/images/6-6.JPEG.190.webp" />
        </td>
      </tr>
    </tbody>
  </table>
</div>

There are two dice, each with six sides they could land on.

<figure class="figure">
  <em>No. of sides dice 1</em> &times; <em>No. of sides dice 2</em> = <em>No. of possibilities</em>
</figure>

So.

<figure class="figure">
  <p>6 &times; 6 = 36</p>
</figure>

As with Two-up, many of these possibilities are counted as the same outcome. All that matters is the sum of the numbers on each dice. So not only is 6 and 4 equal to 4 and 6, but also 3 and 7. But we needn't worry about all that.

## DNA

One more game of chance.

<div class="overflow-x-auto overflow-y-hidden">
  <ul class="
    fira-mono
    list-none
    p-t-12
    transform-style-preserve3d
    transform-origin-0
    transform-rotate-3d-iso
    grid
    grid-rows-1
    grid-cols-1
    w-4/5
    layers
  ">
    <li class="row-span-full col-span-full backface-visibility-hidden layer-first">
      <span class="sr-only">T</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg">
        <li>
          <span class="sr-only">TT</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TTT</li>
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TTC</li>
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TTA</li>
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TTG</li>
          </ul>
        </li>
        <li>
          <span class="sr-only">TC</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TCT</li>
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TCC</li>
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TCA</li>
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TCG</li>
          </ul>
        </li>
        <li>
          <span class="sr-only">TA</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TAT</li>
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TAC</li>
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TAA</li>
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TAG</li>
          </ul>
        </li>
        <li>
          <span class="sr-only">TG</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TGT</li>
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TGC</li>
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TGA</li>
            <li class="w-1/4 rounded-lg p-2 text-green bg-green/25">TGG</li>
          </ul>
        </li>
      </ul>
    </li>
    <li class="row-span-full col-span-full backface-visibility-hidden layer-second">
      <span class="sr-only">C</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg">
        <li>
          <span class="sr-only">CT</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CTT</li>
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CTC</li>
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CTA</li>
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CTG</li>
          </ul>
        </li>
        <li>
          <span class="sr-only">CC</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CCT</li>
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CCC</li>
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CCA</li>
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CCG</li>
          </ul>
        </li>
        <li>
          <span class="sr-only">CA</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CAT</li>
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CAC</li>
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CAA</li>
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CAG</li>
          </ul>
        </li>
        <li>
          <span class="sr-only">CG</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CGT</li>
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CGC</li>
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CGA</li>
            <li class="w-1/4 rounded-lg p-2 text-orange bg-orange/25">CGG</li>
          </ul>
        </li>
      </ul>
    </li>
    <li class="row-span-full col-span-full backface-visibility-hidden layer-third">
      <span class="sr-only">A</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg">
        <li>
          <span class="sr-only">AT</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">ATT</li>
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">ATC</li>
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">ATA</li>
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">ATG</li>
          </ul>
        </li>
        <li>
          <span class="sr-only">AC</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">ACT</li>
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">ACC</li>
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">ACA</li>
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">ACG</li>
          </ul>
        </li>
        <li>
          <span class="sr-only">AA</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">AAT</li>
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">AAC</li>
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">AAA</li>
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">AAG</li>
          </ul>
        </li>
        <li>
          <span class="sr-only">AG</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">AGT</li>
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">AGC</li>
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">AGA</li>
            <li class="w-1/4 rounded-lg p-2 text-blue bg-blue/25">AGG</li>
          </ul>
        </li>
      </ul>
    </li>
    <li class="row-span-full col-span-full backface-visibility-hidden layer-fourth">
      <span class="sr-only">G</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg">
        <li>
          <span class="sr-only">GT</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GTT</li>
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GTC</li>
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GTA</li>
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GTG</li>
          </ul>
        </li>
        <li>
          <span class="sr-only">GC</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GCT</li>
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GCC</li>
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GCA</li>
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GCG</li>
          </ul>
        </li>
        <li>
          <span class="sr-only">GA</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GAT</li>
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GAC</li>
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GAA</li>
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GAG</li>
          </ul>
        </li>
        <li>
          <span class="sr-only">GG</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GGT</li>
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GGC</li>
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GGA</li>
            <li class="w-1/4 rounded-lg p-2 text-purple bg-purple/25">GGG</li>
          </ul>
        </li>
      </ul>
    </li>
  </ul>
</div>
