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

The string quartet has been around as musical genre and ensemble instrumentation since the 1750s. In 1825 Beethoven wrote some particularly spicy quartets, numbers 12, 13, 14, 15 and 16, the last major compositions he completed. I love them, though not everyone does. A contemporary of Beethoven, and composer himself, Louis Spohr, described them as "indecipherable, uncorrected horrors."

Three of the quartets, 15, 13, and 14, centre thematically around a particular group of four notes, a tetrachord. I wonder what other horrors lurk in the collection of all possible groups of four notes a string quartet can play?

First, how many are there? I'm honestly not sure, the ranges of these instruments are a bit flexible, it can depend on the player, whether harmonics are included, the construction of a particular instance of an instrument. If I might be permitted to use these as the ranges for the instruments in question we can at least get a ballpark figure.

* **Violin** G3 - A7, 51 notes
* **Viola** C3 - E6, 41 notes
* **Cello** C2 - A5, 46 notes

<figure class="figure">
  <em>No. of notes the first violin can play</em><br />
  &times; <em>No. of notes the second violin can play</em><br />
  &times; <em>No. of notes the viola can play</em><br />
  &times; <em>No. of notes the cello can</em><br />
  = <em>No. of possible voicings a string quartet can play</em>
</figure>

So.

<figure class="figure">
  <p>51 &times; 51 &times; 41 &times; 46 = 4,905,486</p>
</figure>

That's a lot.

Here they all are, generated pseudo randomly, without any repeats, twenty per second. It would take you nearly three days of uninterrupted listening to hear them all.

**Note** if reading on a mobile device you may need to disable silent mode to hear this.

<button id="string-quartet-control">
  Play
</button>

<script type="module" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/javascript/string-quartet-voicings.js"></script>

[^1]: [Deciphering the Genetic Code: The Most Beautiful False Theory in Biochemistry – Part 1](https://www.chemistryviews.org/details/ezine/11312121/Deciphering_the_Genetic_Code_The_Most_Beautiful_False_Theory_in_Biochemistry__Pa/)
