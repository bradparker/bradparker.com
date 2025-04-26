---
title: Cracking open the window to the cosmos of combinatorics
tags:
  - development
description: |
  Lehmer codes encode permutations of sets. They let you efficiently index into the lexicographically ordered sequence of all possible arrangements of a collection of sortable elements. If little to none of that makes sense to you, I hope you'll read on, this is specifically for you.
---

Lehmer codes encode permutations of sets. They let you efficiently index into the lexicographically ordered sequence of all possible arrangements of a collection of sortable elements. If little to none of that makes sense to you, I hope you'll read on, this is specifically for you.

## Two-up

Two-up is a game played in Australia. Someone tosses two coins in the air at the same time and people place bets on the outcome. It is forbidden by law except for one day of the year, ANZAC day. If you want to understand why betting on a coin toss might be outlawed (and otherwise just enjoy a great if but harrowing novel) I reckon [Wake in Fright](https://en.wikipedia.org/wiki/Wake_in_Fright) is worth your time.

How many possible states can two coins be in? The first coin can be heads or tails, and the second coin can be heads or tails.

<div class="grid gap-3 grid-rows-2 grid-cols-2">
  <figure>
    <img src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/two-up/heads-heads.jpeg.webp" />
    <figcaption>
      First heads, second heads
    </figcaption>
  </figure>
  <figure>
    <img src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/two-up/heads-tails.jpeg.webp" />
    <figcaption>
      First heads, second tails
    </figcaption>
  </figure>
  <figure>
    <img src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/two-up/tails-heads.jpeg.webp" />
    <figcaption>
      First tails, second heads
    </figcaption>
  </figure>
  <figure>
    <img src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/two-up/tails-tails.jpeg.webp" />
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

Hazard is a game that was mostly played in 17th and 18th century England. Someone rolls a pair of dice and places bets on the outcome. The rules for assessing the success or failure of an outcome are fairly complicated. It's referenced in Chaucer's _Canterbury Tales_ in a passage that may have given rise to the idiom "at sixes and sevens", which refers to being in a state of confusion or disarray. I've never read _Canterbury tales_, I suspect if I tried I'd end up at sixes and sevens.

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
          <img alt="Two dice. The first with the 1 side up, the second with the 1 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/1-1.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 1 side up, the second with the 2 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/1-2.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 1 side up, the second with the 3 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/1-3.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 1 side up, the second with the 4 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/1-4.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 1 side up, the second with the 5 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/1-5.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 1 side up, the second with the 6 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/1-6.JPEG.190.webp" />
        </td>
      </tr>
      <tr>
        <th class="align-middle">2</th>
        <td>
          <img alt="Two dice. The first with the 2 side up, the second with the 1 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/2-1.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 2 side up, the second with the 2 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/2-2.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 2 side up, the second with the 3 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/2-3.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 2 side up, the second with the 4 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/2-4.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 2 side up, the second with the 5 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/2-5.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 2 side up, the second with the 6 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/2-6.JPEG.190.webp" />
        </td>
      </tr>
      <tr>
        <th class="align-middle">3</th>
        <td>
          <img alt="Two dice. The first with the 3 side up, the second with the 1 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/3-1.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 3 side up, the second with the 2 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/3-2.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 3 side up, the second with the 3 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/3-3.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 3 side up, the second with the 4 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/3-4.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 3 side up, the second with the 5 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/3-5.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 3 side up, the second with the 6 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/3-6.JPEG.190.webp" />
        </td>
      </tr>
      <tr>
        <th class="align-middle">4</th>
        <td>
          <img alt="Two dice. The first with the 4 side up, the second with the 1 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/4-1.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 4 side up, the second with the 2 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/4-2.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 4 side up, the second with the 3 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/4-3.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 4 side up, the second with the 4 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/4-4.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 4 side up, the second with the 5 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/4-5.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 4 side up, the second with the 6 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/4-6.JPEG.190.webp" />
        </td>
      </tr>
      <tr>
        <th class="align-middle">5</th>
        <td>
          <img alt="Two dice. The first with the 5 side up, the second with the 1 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/5-1.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 5 side up, the second with the 2 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/5-2.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 5 side up, the second with the 3 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/5-3.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 5 side up, the second with the 4 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/5-4.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 5 side up, the second with the 5 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/5-5.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 5 side up, the second with the 6 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/5-6.JPEG.190.webp" />
        </td>
      </tr>
      <tr>
        <th class="align-middle">6</th>
        <td>
          <img alt="Two dice. The first with the 6 side up, the second with the 1 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/6-1.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 6 side up, the second with the 2 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/6-2.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 6 side up, the second with the 3 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/6-3.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 6 side up, the second with the 4 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/6-4.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 6 side up, the second with the 5 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/6-5.JPEG.190.webp" />
        </td>
        <td>
          <img alt="Two dice. The first with the 6 side up, the second with the 6 side up" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/hazard/6-6.JPEG.190.webp" />
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

Deoxyribonucleic acid is a pretty special molecule. Two chains of smaller molecular units (called nucleotides) each zipped together by the propensity for electron density to be greater nearer to nitrogen or oxygen atoms rather than hydrogen atoms.

DNA nucleotides all have a deoxyribose component attached to one of four different sub-units (nitrogenous bases): adenine, cytosine, guanine and thymine. It's from these we get the letters we use to denote the genetic code: A, C, G and T.

Cells use DNA for multiple purposes, one of which is as a sort of template that informs protein construction. Proteins are also pretty special molecules, built out of one or more chains (polypeptides) of other molecular units (amino acids).

In 1961 Francis Crick, Sydney Brenner, Leslie Barnett and R. J. Watts-Tobin figured out that this process of protein construction requires that the nucleotides that make up DNA come in groups of three. They called these groups "codons".

How many different codons could there be?

<div class="overflow-x-auto overflow-y-hidden">
  <ul class="list-none grid">
    <li class="row-span-full col-span-full" style="transform: translate(-12.5%, 12.5%) scale(0.75);">
      <span class="sr-only">T</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg">
        <li>
          <span class="sr-only">TT</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TTT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TTT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TTC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TTC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TTA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TTA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TTG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TTG"
              />
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">TC</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TCT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TCT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TCC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TCC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TCA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TCA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TCG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TCG"
              />
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">TA</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TAT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TAT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TAC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TAC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TAA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TAA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TAG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TAG"
              />
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">TG</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TGT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TGT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TGC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TGC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TGA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TGA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/TGG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: TGG"
              />
            </li>
          </ul>
        </li>
      </ul>
    </li>
    <li class="row-span-full col-span-full" style="transform: translate(-4.1666%, 4.1666%) scale(0.75);">
      <span class="sr-only">C</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg">
        <li>
          <span class="sr-only">CT</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CTT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CTT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CTC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CTC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CTA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CTA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CTG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CTG"
              />
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">CC</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CCT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CCT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CCC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CCC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CCA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CCA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CCG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CCG"
              />
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">CA</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CAT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CAT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CAC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CAC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CAA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CAA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CAG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CAG"
              />
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">CG</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CGT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CGT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CGC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CGC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CGA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CGA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/CGG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: CGG"
              />
            </li>
          </ul>
        </li>
      </ul>
    </li>
    <li class="row-span-full col-span-full" style="transform: translate(4.1666%, -4.1666%) scale(0.75);">
      <span class="sr-only">A</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg">
        <li>
          <span class="sr-only">AT</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/ATT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: ATT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/ATC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: ATC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/ATA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: ATA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/ATG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: ATG"
              />
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">AC</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/ACT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: ACT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/ACC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: ACC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/ACA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: ACA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/ACG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: ACG"
              />
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">AA</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/AAT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: AAT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/AAC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: AAC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/AAA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: AAA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/AAG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: AAG"
              />
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">AG</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/AGT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: AGT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/AGC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: AGC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/AGA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: AGA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/AGG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: AGG"
              />
            </li>
          </ul>
        </li>
      </ul>
    </li>
    <li class="row-span-full col-span-full" style="transform: translate(12.5%, -12.5%) scale(0.75);">
      <span class="sr-only">G</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg">
        <li>
          <span class="sr-only">GT</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GTT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GTT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GTC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GTC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GTA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GTA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GTG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GTG"
              />
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">GC</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GCT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GCT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GCC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GCC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GCA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GCA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GCG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GCG"
              />
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">GA</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GAT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GAT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GAC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GAC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GAA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GAA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GAG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GAG"
              />
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">GG</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GGT.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GGT"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GGC.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GGC"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GGA.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GGA"
              />
            </li>
            <li class="w-1/4 rounded-lg">
              <img
                src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/codons/GGG.JPEG.204.webp"
                alt="Cookies in the shape of letters, in this case: GGG"
              />
            </li>
          </ul>
        </li>
      </ul>
    </li>
  </ul>
</div>

There are three 'slots' in each codon, which could each be filled by one of four nucleotide types.

<figure class="figure">
  <em>No. of nucleotides</em> &times; <em>No. of nucleotides</em> &times; <em>No. of nucleotides</em> = <em>No. of possible codons</em>
</figure>

So.

<figure class="figure">
  <p>4 &times; 4 &times; 4 = 64</p>
</figure>

In 1954 Watson and Crick intuited a list of the 20 amino acids believed to be constructed according to DNA's template. They were inspired to do so in response to a tidy, but ultimately incorrect, geometric explanation of the process put forward by a theoretical physicist, George Gamow. Later, in 1956, they came up with their own tidy, but ultimately incorrect, combinatorial explanation.[^1]

This seems like fiendishly complex stuff, there's sure to be many red herrings on the path to the right explanation. I guess that's just how the cookie crumbles.

## Quartets

The string quartet has been around as a musical genre and an ensemble instrumentation since the 1750s. In 1825 Beethoven wrote some particularly spicy quartets, numbers 12 to 16, the last major compositions he completed. I love them, though not everyone does. A contemporary of Beethoven, and composer himself, Louis Spohr, described them as "indecipherable, uncorrected horrors."

Three of the quartets, 15, 13, and 14, centre thematically around particular groupings of four notes, groupings united by how far away their constituent notes are from each other. Four note groupings that share a shape like this are called tetrachords. This has me wondering what horrors might lurk in the collection of all possible voicings a string quartet could play?

First, how many are there? I'm honestly not sure, the ranges of these instruments are a bit flexible, it can depend on the player, whether harmonics are included, the construction of a particular instance of an instrument. If I might be permitted to use these as the ranges for the instruments in question we can at least get a ballpark figure.

Violin
:   G3 - A7, 51 notes

Viola
:   C3 - E6, 41 notes

Cello
:   C2 - A5, 46 notes

The fist and second violins can each play any one of 51 notes, the viola one of 41, and the cello one of 46. So.

<figure class="figure">
  <p>51 &times; 51 &times; 41 &times; 46 = 4,905,486</p>
</figure>

That is a lot.

Here they all are, generated pseudo randomly, without any repeats, at a rate of ten per second. It would take you over five days and sixteen hours of uninterrupted listening to hear them all.

<button id="string-quartet-control">
  Play
</button>

**Note** if reading on a mobile device you may need to disable silent mode to hear this.

<script type="module" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/javascript/string-quartet-voicings.js"></script>

## Space, and getting lost in it

Beethoven's four note groupings are in there somewhere, but where? Where, for example, is E4, F4, G#4, A4?

<svg viewBox="0 40 115 66" height="4rem"><g><path stroke-width="1" stroke-dasharray="none" fill="none" stroke="#999999" d="M10 50L104.96752000000001 50"></path><path stroke-width="1" stroke-dasharray="none" fill="none" stroke="#999999" d="M10 60L104.96752000000001 60"></path><path stroke-width="1" stroke-dasharray="none" fill="none" stroke="#999999" d="M10 70L104.96752000000001 70"></path><path stroke-width="1" stroke-dasharray="none" fill="none" stroke="#999999" d="M10 80L104.96752000000001 80"></path><path stroke-width="1" stroke-dasharray="none" fill="none" stroke="#999999" d="M10 90L104.96752000000001 90"></path><rect x="104.96752000000001" y="49.5" width="1" height="41" fill="black"></rect><g><path stroke-width="0.3" stroke-dasharray="none" fill="black" stroke="none" d="M28.711104 65.844288C28.660416 65.41344,28.711104 65.388096,28.9392 65.16C32.892864 61.485119999999995,35.883456 56.847167999999996,35.883456 51.246144C35.883456 48.078143999999995,34.996415999999996 44.935488,33.50112 42.755903999999994C32.943552 41.944896,32.005824000000004 40.931135999999995,31.60032 40.931135999999995C31.09344 40.931135999999995,29.95296 41.868863999999995,29.243328 42.679871999999996C26.53152 45.670463999999996,25.64448 50.232383999999996,25.64448 54.033984C25.64448 56.137536,25.923264000000003 58.519872,26.176704 60.015168C26.252736 60.446016,26.278080000000003 60.522048,25.847231999999998 60.902208C20.57568 65.236032,15 70.456896,15 77.832C15 84.168,19.333824 90.199872,28.280256 90.199872C29.116608 90.199872,30.079680000000003 90.12384,30.814656 89.971776C31.194816000000003 89.89574400000001,31.270848 89.8704,31.346880000000002 90.301248C31.777728 92.759616,32.335296 95.927616,32.335296 97.651008C32.335296 103.04928000000001,28.685760000000002 103.708224,26.53152 103.708224C24.554688 103.708224,23.61696 103.12531200000001,23.61696 102.643776C23.61696 102.390336,23.946432 102.28896,24.782784 102.010176C25.923264000000003 101.680704,27.215808000000003 100.71763200000001,27.215808000000003 98.588736C27.215808000000003 96.58656,25.948608 94.863168,23.718336 94.863168C21.285312 94.863168,19.81536 96.814656,19.81536 99.070272C19.81536 101.42726400000001,21.234624 105.026112,26.759616 105.026112C29.19264 105.026112,33.931968 103.910976,33.931968 97.72704C33.931968 95.62348800000001,33.27302400000001 92.176704,32.892864 89.89574400000001C32.816832000000005 89.464896,32.842176 89.515584,33.349056000000004 89.287488C37.04928 87.817536,39.482304 84.725568,39.482304 80.594496C39.482304 75.9312,36.060864 71.800128,30.687936 71.800128C29.750208 71.800128,29.750208 71.800128,29.623488000000002 71.141184M32.157888 46.582848C33.349056000000004 46.582848,34.337472000000005 47.571264,34.337472000000005 49.57344C34.337472000000005 53.628479999999996,30.865344 56.923199999999994,28.001472 59.432255999999995C27.748032000000002 59.660352,27.595968 59.609663999999995,27.519936 59.128128C27.367872 58.1904,27.29184 56.948544,27.29184 55.78272C27.29184 50.08032,29.927616 46.582848,32.157888 46.582848M28.17888 71.445312C28.280256 72.1296,28.280256 72.10425599999999,27.621312000000003 72.307008C24.427968 73.3968,22.324416 76.286016,22.324416 79.403328C22.324416 82.672704,24.047808 85.004352,26.53152 85.866048C26.835648 85.967424,27.266496 86.0688,27.519936 86.0688C27.798720000000003 86.0688,27.950784 85.891392,27.950784 85.663296C27.950784 85.409856,27.672 85.30848,27.41856 85.207104C25.872576000000002 84.54816,24.782784 82.976832,24.782784 81.304128C24.782784 79.200576,26.202048 77.654592,28.43232 77.020992C29.015232 76.868928,29.091264000000002 76.919616,29.167296 77.32512L30.992064 88.19769600000001C31.068096 88.6032,31.017408 88.6032,30.485184000000004 88.704576C29.902272000000004 88.805952,29.167296 88.881984,28.43232 88.881984C22.045632 88.881984,17.91456 85.33382399999999,17.91456 80.265024C17.91456 78.110784,18.29472 75.221568,21.310656 71.800128C23.515584 69.367104,25.188288 67.998528,26.886336 66.629952C27.266496 66.325824,27.342528 66.37651199999999,27.41856 66.756672M30.687936 77.249088C30.611904000000003 76.792896,30.662592000000004 76.69152,31.09344 76.742208C34.058688000000004 76.995648,36.49171200000001 79.47936,36.49171200000001 82.672704C36.49171200000001 84.979008,35.097792 86.82912,33.070272 87.868224C32.639424000000005 88.070976,32.563392 88.070976,32.48736 87.640128"></path></g></g><g><g pointer-events="bounding-box"><g pointer-events="bounding-box"><path stroke-width="0.3" stroke-dasharray="none" fill="black" stroke="none" d="M77.55511999999999 84.9456C72.19184 84.9456,68.82224 87.16392,68.82224 89.91576C68.82224 92.63952,71.1248 95.0544,77.16199999999999 95.0544C83.78887999999999 95.0544,85.89488 92.75184,85.89488 89.91576C85.89488 87.0516,81.31783999999999 84.9456,77.55511999999999 84.9456M73.31504 87.44472C73.76432 86.04072,75.25255999999999 85.84416,76.51616 85.84416C79.29607999999999 85.84416,81.5144 88.82064,81.5144 91.2636C81.5144 92.49912,80.98088 93.6504,79.66112 93.95928C79.268 94.0716,78.81872 94.12776,78.39752 94.12776C76.93736 94.12776,75.44912 93.14496,74.60672 92.02176C73.79239999999999 91.09512,73.20272 89.7192,73.20272 88.42752C73.20272 88.09056,73.23079999999999 87.78168,73.31504 87.44472"></path></g></g><g></g></g><g><g pointer-events="bounding-box"><g pointer-events="bounding-box"><path stroke-width="0.3" stroke-dasharray="none" fill="black" stroke="none" d="M96.62776 79.9456C91.26448 79.9456,87.89488 82.16392,87.89488 84.91576C87.89488 87.63952,90.19744 90.0544,96.23464 90.0544C102.86152 90.0544,104.96752000000001 87.75184,104.96752000000001 84.91576C104.96752000000001 82.0516,100.39048 79.9456,96.62776 79.9456M92.38768 82.44472C92.83696 81.04072,94.3252 80.84416,95.5888 80.84416C98.36872 80.84416,100.58704 83.82064,100.58704 86.2636C100.58704 87.49912,100.05352 88.6504,98.73376 88.95928C98.34064000000001 89.0716,97.89136 89.12776,97.47016 89.12776C96.01 89.12776,94.52176 88.14496,93.67936 87.02176C92.86504 86.09512,92.27536 84.7192,92.27536 83.42752C92.27536 83.09056,92.30344 82.78168,92.38768 82.44472"></path></g></g><g></g></g><g ><g pointer-events="bounding-box"><g pointer-events="bounding-box"><path stroke-width="0.3" stroke-dasharray="none" fill="black" stroke="none" d="M77.55511999999999 74.9456C72.19184 74.9456,68.82224 77.16392,68.82224 79.91576C68.82224 82.63952,71.1248 85.0544,77.16199999999999 85.0544C83.78887999999999 85.0544,85.89488 82.75184,85.89488 79.91576C85.89488 77.0516,81.31783999999999 74.9456,77.55511999999999 74.9456M73.31504 77.44472C73.76432 76.04072,75.25255999999999 75.84416,76.51616 75.84416C79.29607999999999 75.84416,81.5144 78.82064,81.5144 81.2636C81.5144 82.49912,80.98088 83.6504,79.66112 83.95928C79.268 84.0716,78.81872 84.12776,78.39752 84.12776C76.93736 84.12776,75.44912 83.14496,74.60672 82.02176C73.79239999999999 81.09512,73.20272 79.7192,73.20272 78.42752C73.20272 78.09056,73.23079999999999 77.78168,73.31504 77.44472"></path></g></g><g><path stroke-width="0.3" stroke-dasharray="none" fill="black" stroke="none" d="M65.32976 75.3488C65.60336 75.23936,65.82224 74.91104,65.82224 74.69216L65.82224 71.87408C65.82224 71.68256,65.68544 71.57312,65.52127999999999 71.57312C65.46655999999999 71.57312,65.41184 71.57312,65.32976 71.60048C65.32976 71.60048,64.53631999999999 71.9288,64.34479999999999 71.95616C64.07119999999999 71.95616,63.797599999999996 71.76464,63.797599999999996 71.46368L63.797599999999996 66.64832C63.797599999999996 66.40208,63.551359999999995 66.21056,63.25039999999999 66.21056C62.86735999999999 66.21056,62.62111999999999 66.40208,62.62111999999999 66.64832L62.62111999999999 71.76464C62.566399999999994 72.14768,62.456959999999995 72.66752,62.101279999999996 72.91376C61.63615999999999 73.18736,60.295519999999996 73.73456,59.61151999999999 73.89872C59.283199999999994 73.89872,59.14639999999999 73.4336,59.14639999999999 73.10528L59.14639999999999 68.372C59.14639999999999 68.15312,58.87279999999999 67.93424,58.599199999999996 67.93424C58.216159999999995 67.93424,57.969919999999995 68.15312,57.969919999999995 68.372L57.969919999999995 73.7072C57.969919999999995 74.2544,57.723679999999995 74.63744,57.504799999999996 74.74688C57.258559999999996 74.88368,56.46511999999999 75.18464,56.46511999999999 75.18464C56.19151999999999 75.26672,55.99999999999999 75.59504,55.99999999999999 75.81392L55.99999999999999 78.632C55.99999999999999 78.85088,56.10943999999999 78.98768,56.35567999999999 78.98768L56.43775999999999 78.93296C56.46511999999999 78.93296,57.06703999999999 78.68672,57.367999999999995 78.54992L57.42271999999999 78.4952C57.723679999999995 78.4952,57.969919999999995 78.9056,57.969919999999995 79.20656L57.969919999999995 83.11904C57.969919999999995 83.5568,57.77839999999999 83.91248,57.53215999999999 84.02192C57.31327999999999 84.104,56.46511999999999 84.45968,56.46511999999999 84.45968C56.19151999999999 84.54176,55.99999999999999 84.84272,55.99999999999999 85.08896L55.99999999999999 87.87968C55.99999999999999 88.12592,56.10943999999999 88.23536,56.35567999999999 88.23536L56.43775999999999 88.208C56.46511999999999 88.208,57.012319999999995 87.96176,57.367999999999995 87.85232C57.42271999999999 87.7976,57.45007999999999 87.7976,57.504799999999996 87.7976C57.77839999999999 87.7976,57.969919999999995 88.23536,57.969919999999995 88.42688L57.969919999999995 93.2696C57.969919999999995 93.51584,58.216159999999995 93.70736,58.48975999999999 93.70736C58.87279999999999 93.70736,59.14639999999999 93.51584,59.14639999999999 93.2696L59.14639999999999 87.7976C59.14639999999999 87.27776,59.33791999999999 87.00416,59.556799999999996 86.92208L61.93711999999999 85.93712C61.93711999999999 85.93712,61.991839999999996 85.93712,61.991839999999996 85.93712L62.073919999999994 85.90976C62.42959999999999 85.90976,62.62111999999999 86.37488,62.62111999999999 86.62112L62.62111999999999 91.54592C62.62111999999999 91.79216,62.86735999999999 91.98367999999999,63.14095999999999 91.98367999999999C63.551359999999995 91.98367999999999,63.797599999999996 91.79216,63.797599999999996 91.54592L63.797599999999996 85.93712C63.797599999999996 85.63616,63.96175999999999 85.17104,64.23535999999999 85.03424C64.50895999999999 84.9248,65.32976 84.59648,65.32976 84.59648C65.60336 84.48704,65.82224 84.18608,65.82224 83.93984L65.82224 81.14912C65.82224 80.9576,65.68544 80.8208,65.52127999999999 80.8208C65.46655999999999 80.8208,65.41184 80.8208,65.32976 80.87552L64.31743999999999 81.25856C64.07119999999999 81.25856,63.797599999999996 81.01232,63.797599999999996 80.5472L63.797599999999996 76.88096C63.797599999999996 76.60736,63.98911999999999 75.86864,64.31743999999999 75.73184M62.62111999999999 81.7784C62.37487999999999 82.57184,60.54176 83.33792,59.61151999999999 83.33792C59.39263999999999 83.33792,59.201119999999996 83.2832,59.14639999999999 83.1464C59.064319999999995 82.98224,59.03695999999999 82.13408,59.03695999999999 81.17648C59.03695999999999 79.97264,59.064319999999995 78.57728,59.14639999999999 78.27632C59.22847999999999 77.59232,61.03423999999999 76.77152,62.01919999999999 76.77152C62.29279999999999 76.77152,62.53903999999999 76.8536,62.62111999999999 77.01776C62.703199999999995 77.20928,62.78527999999999 78.19424,62.78527999999999 79.26128C62.78527999999999 80.32832,62.703199999999995 81.42272,62.62111999999999 81.7784"></path></g></g><g><g pointer-events="bounding-box"><g pointer-events="bounding-box"><path stroke-width="0.3" stroke-dasharray="none" fill="black" stroke="none" d="M77.55511999999999 69.9456C72.19184 69.9456,68.82224 72.16392,68.82224 74.91576C68.82224 77.63952,71.1248 80.0544,77.16199999999999 80.0544C83.78887999999999 80.0544,85.89488 77.75184,85.89488 74.91576C85.89488 72.0516,81.31783999999999 69.9456,77.55511999999999 69.9456M73.31504 72.44472C73.76432 71.04072,75.25255999999999 70.84416,76.51616 70.84416C79.29607999999999 70.84416,81.5144 73.82064,81.5144 76.2636C81.5144 77.49912,80.98088 78.6504,79.66112 78.95928C79.268 79.0716,78.81872 79.12776,78.39752 79.12776C76.93736 79.12776,75.44912 78.14496,74.60672 77.02176C73.79239999999999 76.09512,73.20272 74.7192,73.20272 73.42752C73.20272 73.09056,73.23079999999999 72.78168,73.31504 72.44472"></path></g></g></g></svg>

It might seem like a nonesense question, but it has a few answers. One answer is this, we can think of each instrument as a dimension in space. Instead of the familiar three dimensions we think of moving about in day to day life, left-right, up-down and forward-backward, we have four dimensions: the cello, the viola, the second violin and the first violin. Because each instrument is constrained in the range of notes they can play this four dimensional space doesn't go on forever like the three dimensional one we live in. Each dimension starts at 0, being the lowest note that instrument can play, and then goes on to count each note finishing at the highest. So, where is E4, F4, G#4, A4? It's at position: (28, 17, 13, 14).

And where is that exactly? Well, I can't show you. Unlike the examples of coin tosses, dice rolls or codons, I can't create a visualisation of this space of posibilities. I can't see four dimensional things, and I lack the imgination to even try to picture them. What I can do, though, is squish the four dimensional space of possibilities into a different shape and show you that instead.

To demonstrate, here's a two dimensional space of possibilities. Perhaps imagine it's a duet of tiny, five tone, glokenspiels.

![Colourful marbles arranged on a five by five grid. The columns and rows are numbered, zero to four. The first row is red, the second row is orange, the third row is yellow, the fourth row is green and the fifth row is blue.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5-space-1-small.webp)

![Colourful marbles, arranged on a mostly five by five grid. The first row is ten marbles wide, the second row is missing, it was moved from to the end of the first row. The columns are numbered, zero to nine. The rows are numbered zero to four. The first row is red and orange, the third row is yellow, the fourth row is green and the fifth row is blue.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5-space-2-small.webp)

![Colourful marbles, arranged on a mostly five by five grid. The first row is fifteen marbles wide, the second and third rows are missing, they have been moved to the first row. The columns are numbered, zero to fourteen. The rows are numbered zero to four. The first row is red, yellow and orange, the fourth row is green and the fifth row is blue.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5-space-3-small.webp)

![Colourful marbles, arranged on a mostly five by five grid. The first row is twenty marbles wide, the second, third and fourth rows are missing, they have been moved to the first row. The columns are numbered, zero to nineteen. The rows are numbered zero to four. The first row is red, yellow, orange and green, the fifth row is blue.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5-space-4-small.webp)

![Colourful marbles, arranged in a row of twenty five marbles.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5-space-5-small.webp)

[^1]: [Deciphering the Genetic Code: The Most Beautiful False Theory in Biochemistry – Part 1](https://www.chemistryviews.org/details/ezine/11312121/Deciphering_the_Genetic_Code_The_Most_Beautiful_False_Theory_in_Biochemistry__Pa/)
