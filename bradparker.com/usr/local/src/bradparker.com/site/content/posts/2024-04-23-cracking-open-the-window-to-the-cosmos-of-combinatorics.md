---
title: Cracking open the window to the cosmos of combinatorics
tags:
  - mathematics
description: |
  From games of chance like two-up and hazard to listening to finite and discrete N-dimensional spaces of possibilities. Here is a tour through my very early steps into the cosmos of combinatorics.

  ![Colourful magnetic blocks, arranged in a cube, five wide, five high and five deep. There are arrows labelling the width, height, and depth as 'x', 'y' and 'z' respectively.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5x5-space-1-small-with-axes.webp)
---

I'm a beginner, but it seems to me that at least some applications of combinatorics can make spaces of possibilities navigable. With it we can get a handle on the otherwise overwhelmingly huge. To demonstrate how this might be done, however, we shall start small.

## Two-up

Two-up is a game played in Australia. Someone tosses two coins in the air at the same time and people place bets on the outcome. It is forbidden by law except for one day of the year, ANZAC day. If you want to understand why betting on a coin toss might be outlawed (and otherwise just enjoy a great if but harrowing novel) I reckon [Wake in Fright](https://en.wikipedia.org/wiki/Wake_in_Fright_(novel)) is worth your time.

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

In Two-up two of these possibilities are counted as the same outcome. The order of the coins doesn't matter, which is handy, keeping track of them flying through the air, a few pints deep, would surely be diabolical. So one outcome ends up being twice as likely as the other two. Sounds simple, ends up pretty complicated for [John Grant](https://en.wikipedia.org/wiki/Wake_in_Fright_(novel)#Plot_summary).

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

Deoxyribonucleic acid is a pretty special molecule. Two chains of smaller molecular units (called nucleotides) each zipped together by the attractive force brought about by the tendancy of electron density near nitrogen or oxygen atoms to be greater than near hydrogen atoms.

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

Three of the quartets, 15, 13, and 14, centre thematically around particular groupings of four notes, groupings united by how far away their constituents are from each other. Which has me wondering what indecipherable, uncorrected horrors might lurk in the collection of all possible voicings a string quartet could play?

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

### In space

It's a vast space of possibilities, and Beethoven's four note groupings are in there somewhere, but where? Where, for example, is E4, F4, G#4, A4?

It might seem like a nonsense question, but at least it can be answered. One answer is this, we can think of each instrument as a dimension in space and plot E4, F4, G#4, A4 within it. Instead of the familiar three dimensions we think of moving about in day to day life, left-right, up-down and forward-backward, we have four dimensions: the **cello**, the **viola**, the **second violin** and the **first violin**. Because each instrument is constrained in the range of notes it can play we're really talking about some four dimensional shape. We can imagine plotting coordinates within this shape by measuring how far along each dimension a point extends. Starting at 0, being the lowest note that instrument can play, and counting each subsequent note, ending at the highest.

So, where is E4, F4, G#4, A4 within that shape? It's at the position: **Cello** = 28, **Viola** = 17, **Second Violin** = 13 and **First Violin** = 14. And where is that exactly? Well, I can't show you. Unlike the examples of coin tosses, dice rolls or DNA codons, I can't create a visualisation of this space of possibilities. I've never seen any four dimensional things, and I lack the imagination to even try to picture them. What I can do though is unfold the four dimensional space of possibilities into a different shape and show you that instead.

To demonstrate, here's a two dimensional space of possibilities. Perhaps imagine it's a duet of tiny, five tone, glockenspiels.

![Colourful marbles arranged on a five by five grid. The columns and rows are numbered, zero to four. The first row is red, the second row is orange, the third row is yellow, the fourth row is green and the fifth row is blue.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5-space-1-small.webp)

Because this shape doesn't go on forever (it's finite) and I'm not planning on breaking any marbles into pieces (they're discrete) two dimensions can be flattened into one by placing each row end to end. A rectangle can become a line.

![Colourful marbles, arranged on a mostly five by five grid. The first row is ten marbles wide, the second row is missing, it was moved from to the end of the first row. The columns are numbered, zero to nine. The rows are numbered zero to four. The first row is red and orange, the third row is yellow, the fourth row is green and the fifth row is blue.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5-space-2-small.webp)

![Colourful marbles, arranged on a mostly five by five grid. The first row is fifteen marbles wide, the second and third rows are missing, they have been moved to the first row. The columns are numbered, zero to fourteen. The rows are numbered zero to four. The first row is red, yellow and orange, the fourth row is green and the fifth row is blue.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5-space-3-small.webp)

![Colourful marbles, arranged on a mostly five by five grid. The first row is twenty marbles wide, the second, third and fourth rows are missing, they have been moved to the first row. The columns are numbered, zero to nineteen. The rows are numbered zero to four. The first row is red, yellow, orange and green, the fifth row is blue.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5-space-4-small.webp)

![Colourful marbles, arranged in a row of twenty five marbles.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5-space-5-small.webp)

With this scheme the second yellow marble moves from column 1 row 2 to column 11 row 0. Each marble is being counted, left to right along each row.

<div class="founders-grotesk text-center">
  <table class="table-fixed min-w-full border-separate border-spacing-3">
    <tbody>
      <tr>
        <th></th>
        <th>0</th>
        <th>1</th>
        <th>2</th>
        <th>3</th>
        <th>4</th>
      </tr>
      <tr>
        <th>0</th>
        <td><strong>0</strong></td>
        <td><strong>1</strong></td>
        <td><strong>2</strong></td>
        <td><strong>3</strong></td>
        <td><strong>4</strong></td>
      </tr>
      <tr>
        <th>1</th>
        <td><strong>5</strong></td>
        <td><strong>6</strong></td>
        <td><strong>7</strong></td>
        <td><strong>8</strong></td>
        <td><strong>9</strong></td>
      </tr>
      <tr>
        <th>2</th>
        <td><strong>10</strong></td>
        <td><div class="rounded-lg bg-yellow"><strong>11</strong></div></td>
        <td>12</td>
        <td>13</td>
        <td>14</td>
      </tr>
      <tr>
        <th>3</th>
        <td>15</td>
        <td>16</td>
        <td>17</td>
        <td>18</td>
        <td>19</td>
      </tr>
      <tr>
        <th>4</th>
        <td>20</td>
        <td>21</td>
        <td>22</td>
        <td>23</td>
        <td>24</td>
      </tr>
    </tbody>
  </table>
</div>

In general, columns are counted <em>row number</em> times (in this case 2), then <em>column number</em> is added.

<figure class="figure">

<em>No. of columns</em> &times; <em>Row number</em> + <em>Column number</em>

</figure>

With the addition of another imaginary glockenspiel the same process can be demonstrated in three dimensions, this time labeled $x$, $y$, and $z$.

![Colourful magnetic blocks, arranged in a cube, five wide, five high and five deep. There are arrows labelling the width, height, and depth as 'x', 'y' and 'z' respectively.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5x5-space-1-small-with-axes.webp)

This three dimensional shape can become a two dimensional shape by laying out each layer, end to end.

<div class="grid gap-3 grid-rows-2 grid-cols-2">

  ![Colourful magnetic blocks, arranged in a cube, five wide, five high and five deep. Each layer has a different colour of magnetic block. Top to bottom it goes: blue, green, yellow, red and black.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5x5-space-1-small.webp)

  ![Colourful magnetic blocks, arranged in a rectangular prism, five wide, five high and four deep. Each layer has a different colour of magnetic block. Top to bottom it goes: blue, green, yellow, and red. A black five by five square of blocks sits just behind.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5x5-space-2-small.webp)

  ![Colourful magnetic blocks, arranged in a rectangular prism, five wide, five high and three deep. Each layer has a different colour of magnetic block. Top to bottom it goes: blue, green and yellow. Two five by five squares of blocks sit just behind, one red and one black.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5x5-space-3-small.webp)

  ![Colourful magnetic blocks, arranged in a rectangular prism, five wide, five high and two deep. Each layer has a different colour of magnetic block. The top is blue, and the bottom is green. Three five by five squares of blocks sit just behind, one yellow, one red and one black.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5x5-space-4-small.webp)

</div>

![Five five-by-five squares of magnetic blocks laid out in a line. Each has a different colour of block: blue, green, yellow, red and black.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5x5-space-5-small.webp)

The yellow cube that begins at position $x = 1, y = 2, z = 1$ ends up at position $x = 1, y = 0, z = 11$.

![Colourful magnetic blocks, arranged in a cube, five wide, five high and five deep. The top two layers have been removed to reveal the yellow layer, an arrow is pointing to the block one in from the side and the top.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5x5-space-open-with-arrow-small.webp)

![Five five-by-five squares of magnetic blocks laid out in a line. Each has a different colour of block: blue, green, yellow, red and black. An arrow is pointing to the block one in from the side and one in from the top within the yellow square of blocks.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/5x5x5-space-5-with-arrow-small.webp)

Because the $y$ dimension will now always be 0 we can ignore it which allows us to say: position $(1, 2, 1)$ has moved to $(1, 11)$. More generally we could say: any position $(x, y, z)$ has moved to position $(x, |Z| \times y + z)$. Where $|Z|$ means: the total number of possible values of $z$. The $z$ dimension has now absorbed the $y$ dimension.

This, as with turning two dimensions into one, counts positions. However in this case it doesn't count them all, only those on the $y \times z$ plane.

<div class="founders-grotesk lh-solid overflow-x-auto overflow-y-hidden">
  <ul class="list-none grid">
    <li class="row-span-full col-span-full" style="transform: translate(10%, -10%) scale(0.75);">
      <span class="sr-only">Z = 4</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg p-3">
        <li>
          <span class="sr-only">Z = 4, Y = 0</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,0,4">
              0,4
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,0,4">
              1,4
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,0,4">
              2,4
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,0,4">
              3,4
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,0,4">
              4,4
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 4, Y = 1</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,1,4">
              0,9
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,1,4">
              1,9
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,1,4">
              2,9
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,1,4">
              3,9
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,1,4">
              4,9
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 4, Y = 2</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,2,4">
              0,14
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,2,4">
              1,14
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,2,4">
              2,14
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,2,4">
              3,14
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,2,4">
              4,14
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 4, Y = 3</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,3,4">
              0,19
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,3,4">
              1,19
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,3,4">
              2,19
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,3,4">
              3,19
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,3,4">
              4,19
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 4, Y = 4</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,4,4">
              0,24
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,4,4">
              1,24
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,4,4">
              2,24
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,4,4">
              3,24
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,4,4">
              4,24
            </li>
          </ul>
        </li>
      </ul>
    </li>
    <li class="row-span-full col-span-full" style="transform: translate(5%, -5%) scale(0.75);">
      <span class="sr-only">Z = 3</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg p-3">
        <li>
          <span class="sr-only">Z = 3, Y = 0</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,0,3">
              0,3
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,0,3">
              1,3
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,0,3">
              2,3
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,0,3">
              3,3
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,0,3">
              4,3
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 3, Y = 1</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,1,3">
              0,8
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,1,3">
              1,8
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,1,3">
              2,8
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,1,3">
              3,8
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,1,3">
              4,8
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 3, Y = 2</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,2,3">
              0,13
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,2,3">
              1,13
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,2,3">
              2,13
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,2,3">
              3,13
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,2,3">
              4,13
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 3, Y = 3</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,3,3">
              0,18
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,3,3">
              1,18
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,3,3">
              2,18
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,3,3">
              3,18
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,3,3">
              4,18
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 3, Y = 4</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,4,3">
              0,23
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,4,3">
              1,23
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,4,3">
              2,23
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,4,3">
              3,23
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,4,3">
              4,23
            </li>
          </ul>
        </li>
      </ul>
    </li>
    <li class="row-span-full col-span-full" style="transform: translate(0%, 0%) scale(0.75);">
      <span class="sr-only">Z = 2</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg p-3">
        <li>
          <span class="sr-only">Z = 2, Y = 0</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,0,2">
              0,2
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,0,2">
              1,2
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,0,2">
              2,2
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,0,2">
              3,2
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,0,2">
              4,2
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 2, Y = 1</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,1,2">
              0,7
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,1,2">
              1,7
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,1,2">
              2,7
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,1,2">
              3,7
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,1,2">
              4,7
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 2, Y = 2</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,2,2">
              0,12
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,2,2">
              1,12
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,2,2">
              2,12
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,2,2">
              3,12
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,2,2">
              4,12
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 2, Y = 3</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,3,2">
              0,17
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,3,2">
              1,17
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,3,2">
              2,17
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,3,2">
              3,17
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,3,2">
              4,17
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 2, Y = 4</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,4,2">
              0,22
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,4,2">
              1,22
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,4,2">
              2,22
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,4,2">
              3,22
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,4,2">
              4,22
            </li>
          </ul>
        </li>
      </ul>
    </li>
    <li class="row-span-full col-span-full" style="transform: translate(-5%, 5%) scale(0.75);">
      <span class="sr-only">Z = 1</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg p-3">
        <li>
          <span class="sr-only">Z = 1, Y = 0</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,0,1">
              0,1
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,0,1">
              1,1
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,0,1">
              2,1
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,0,1">
              3,1
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,0,1">
              4,1
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 1, Y = 1</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,1,1">
              0,6
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,1,1">
              1,6
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,1,1">
              2,6
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,1,1">
              3,6
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,1,1">
              4,6
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 1, Y = 2</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,2,1">
              0,11
            </li>
            <li class="w-1/5 rounded-lg p-3 bg-yellow" title="1,2,1">
              <strong>1,11</strong>
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,2,1">
              2,11
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,2,1">
              3,11
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,2,1">
              4,11
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 1, Y = 3</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,3,1">
              0,16
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,3,1">
              1,16
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,3,1">
              2,16
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,3,1">
              3,16
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,3,1">
              4,16
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 1, Y = 4</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,4,1">
              0,21
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,4,1">
              1,21
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,4,1">
              2,21
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,4,1">
              3,21
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,4,1">
              4,21
            </li>
          </ul>
        </li>
      </ul>
    </li>
    <li class="row-span-full col-span-full" style="transform: translate(-10%, 10%) scale(0.75);">
      <span class="sr-only">Z = 0</span>
      <ul class="list-none space-y-3 p-3 bg-white/60 rounded-lg p-3">
        <li>
          <span class="sr-only">Z = 0, Y = 0</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,0,0">
              0,0
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,0,0">
              1,0
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,0,0">
              2,0
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,0,0">
              3,0
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,0,0">
              4,0
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 0, Y = 1</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,1,0">
              0,5
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,1,0">
              1,5
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,1,0">
              2,5
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,1,0">
              3,5
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,1,0">
              4,5
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 0, Y = 2</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,2,0">
              0,10
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,2,0">
              1,10
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,2,0">
              2,10
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,2,0">
              3,10
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,2,0">
              4,10
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 0, Y = 3</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,3,0">
              0,15
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,3,0">
              1,15
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,3,0">
              2,15
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,3,0">
              3,15
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,3,0">
              4,15
            </li>
          </ul>
        </li>
        <li>
          <span class="sr-only">Z = 0, Y = 4</span>
          <ul class="list-none flex gap-3">
            <li class="w-1/5 rounded-lg p-3" title="0,4,0">
              0,20
            </li>
            <li class="w-1/5 rounded-lg p-3" title="1,4,0">
              1,20
            </li>
            <li class="w-1/5 rounded-lg p-3" title="2,4,0">
              2,20
            </li>
            <li class="w-1/5 rounded-lg p-3" title="3,4,0">
              3,20
            </li>
            <li class="w-1/5 rounded-lg p-3" title="4,4,0">
              4,20
            </li>
          </ul>
        </li>
      </ul>
    </li>
  </ul>
</div>

Unfolding the four dimensional shape which contains all the voicings a string quartet can play by combining the two violins into one dimension yields a three dimensional rectangular prism of these measurements: **Cello** = 46, **Viola** = 41, **Violins** = 2,601. Within which Beethoven's four notes (E4, F4, G#4 and A4) can be found at **Cello** = 28, **Viola** = 17, **Violins** = 677.

![A skinny and short but deep rectangular prism drawn in a grid lined notebook with a black dot drawn roughly a quarter along the depth, roughly halfway along the width, and roughly halfway up the height. The section with the dot also has a zoomed in copy drawn above, to better show where it is in terms of the height and width.](/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/images/spaces/prism-hand-drawn.webp)

Which doesn't feel very enlightening to look at, at least for me. It could be nonsense. Getting to this point, however, has started to lead somewhere sensible.

### In time

If a two dimensional set of possibilities can be unfolded into a one dimensional set of possibilities, and a three dimensional set can be unfolded into a two dimensional set, and a four dimensional set can be unfolded into a three dimensional set, then a four dimensional set of possibilities can be unfolded into a single dimension of possibilities. We can lay all the possibilities out in a line. We can arrange them in some sort of order.

This enables turning any four note voicing into a single, unique, number between 0 and 4,905,485. For example Beethoven's group of four notes (E4, F4, G#4 and A4), which, in the space of string quartet voicings, has the coordinates **Cello** = 28, **Viola** = 17, **Second Violin** = 13, **First Violin** = 14, becomes 1,371,932.

By referring to the scheme for unfolding dimensions explored above its possible to describe a method for finding the number ($i$) of any voicing made up of:
* A note played by the first violin ($w_1$), from within the set of notes it can play ($W$)
* A note played by the second violin ($w_2$), from within the set of notes it can play ($W$)[^2]
* A note played by the viola ($v$), from within the set of notes it can play ($V$)
* A note played by the cello ($c$), from within the set of notes it can play ($C$)

<figure class="wide">

$$
i = ((w_1 \times |W| + w_2) \times |V| + v) \times |C| + c
$$

</figure>

Where putting '$|$' either side of the name for a set of things means "the total number of things in the set." For example $|C|$ means: the total number of notes a cello can play.

This allows for counting through all the voicings and it bears a resemblance to how we usually count things. A number such as 1,825 is usually thought of as "5 ones, plus 2 tens, plus 8 hundreds, plus 1 thousand." However it could also be stated as follows.

<figure class="wide">

$$
\begin{align*}
1,825
  &= ((1 \times 10 + 8) \times 10 + 2) \times 10 + 5 \\
  &= ((10 + 8) \times 10 + 2) \times 10 + 5 \\
  &= (18 \times 10 + 2) \times 10 + 5 \\
  &= (180 + 2) \times 10 + 5 \\
  &= 182 \times 10 + 5 \\
  &= 1,820 + 5 \\
  &= 1,825
\end{align*}
$$

</figure>

Which to me looks like successively pushing the ones place to the left, by multiplying by ten, to make room for the next ones value.

This suggests that there should be a method by which the number for Beethoven's notes can be found which more closely matches how we talk about everyday numbers. And indeed there is, though it might not look the same at first flush.

<figure class="wide">

$$
\begin{align*}
i &= w_1 \times (|W| \times |V| \times |C|) + w_2 \times (|V| \times |C|) + v \times |C| + c \\
  &= 14 \times (51 \times 41 \times 46) + 13 \times (41 \times 46) + 17 \times 46 + 28 \\
  &= 1,371,932
\end{align*}
$$

</figure>

It's possible to express the number 1,825 equivalently.

<figure class="wide">

$$
\begin{align*}
1,825
  &= 1 \times (10 \times 10 \times 10) + 8 \times (10 \times 10) + 2 \times 10 + 5 \\
  &= 1,825
\end{align*}
$$

</figure>

It's also possible to demonstrate that the two expressions are the same.

<figure class="wide">

$$
\begin{align*}
i
  &= ((w_1 \times |W| + w_2) \times |V| + v) \times |C| + c \\
  &= (w_1 \times |W| + w_2) \times (|V| \times |C|) + v \times |C| + c \\
  &= w_1 \times (|W| \times |V| \times |C|) + w_2 \times (|V| \times |C|) + v \times |C| + c
\end{align*}
$$

</figure>

What has been arrived at here is a sort of number system that counts through all the voicings a string quartet can play, just as we might count anything else. What makes this a little more interesting is that it doesn't _just_ count, it _encodes_. Unlike when counting many things in day to day life, counting these voicings using the method described allows for recovering the original four parts from the single number assigned to them when counted.

To perform this recovery, to decode what was encoded, we need to undo each multiplication and addition which converted the note positions into their corresponding number. To undo one step of multiplication and addition we can use the following.

Begin with some number $i$ which we know to be made by multiplying some number $q$ by some number $n$ and then adding some number $r$ to that.

<figure class="wide">

$$
i = r + q \times n
$$

</figure>

Which can be shuffled around to produce $q$.

<figure class="wide">

$$
q = \frac{i - r}{n}
$$

</figure>

And also $r$.

<figure class="wide">

$$
r = i - q \times n
$$

</figure>

However in our case we don't know enough to solve these equations. What's required here is a method for finding at least $q$ or $r$ independent of each other. What's needed is "floored division" (written as $\lfloor \frac{a}{b} \rfloor$).

<figure class="wide">

$$
q = \lfloor \frac{i}{n} \rfloor
$$

</figure>

Floored division is like regular division except that the result is rounded down to a whole number. Or said another way: the remainder is discarded. It can be recovered however using the reshuffling above.

<figure class="wide">

$$
r = i - q \times n
$$

</figure>

This enables decoding a number step by step, undoing multiplications by using floored division and undoing additions by finding the remainder. Beginning with a reminder of the number we're decoding.

<figure class="wide">

$$
\begin{align*}
i
  &= c + |C| \times (v + |V| \times (w_{2} + (|W| \times w_{1}))) \\
  &= c + 46 \times (v + 41 \times (w_{2} + (51 \times w_{1}))) \\
  &= 1,371,932
\end{align*}
$$

</figure>

And then onto finding the value for $c$, the note the cello plays.

<figure class="wide">

$$
\begin{align*}
q_c
  &= \lfloor \frac{i}{|C|} \rfloor \\
  &= \lfloor \frac{1,371,932}{46} \rfloor \\
  &= 29,824 \\
c &= i - |C| \times q_c \\
  &= 1,371,932 - 46 \times 29,824 \\
  &= 28
\end{align*}
$$

</figure>

A layer of addition and multiplication has been peeled off.

<figure class="wide">

$$
\begin{align*}
1,371,932
  &= c + |C| \times q_c \\
  &= 28 + 46 \times 29,824
\end{align*}
$$

</figure>

Which means another layer can be peeled off by starting from $q_c$ instead of $i$, resulting in the value for $v$, the note the viola plays.

<figure class="wide">

$$
\begin{align*}
q_v
  &= \lfloor \frac{q_c}{|V|} \rfloor \\
  &= \lfloor \frac{29,824}{41} \rfloor \\
  &= 727 \\
v &= q_c - |V| \times q_v \\
  &= 29,824 - 41 \times 727 \\
  &= 17
\end{align*}
$$

</figure>

Continuing this process recovers the remaining two notes, those played by the first and second violins.

<figure class="wide">

$$
\begin{align*}
q_{w_2}
  &= \lfloor \frac{q_v}{|W|} \rfloor \\
  &= \lfloor \frac{727}{51} \rfloor \\
  &= 14 \\
w_2 &= q_{w_2} - |W| \times q_{w_2} \\
  &= 727 - 51 \times 14 \\
  &= 13 \\
w_1 &= q_{w_2}
\end{align*}
$$

</figure>

With this ability to decode ordinary numbers into string quartet voicings it's possible to enumerate all the possible voicings just by counting from 0 to 4,905,485. Doing so sounds like this.

<figure>
  <button id="string-quartet-linear-control">
    Play
  </button>

  <figcaption>

  **Note** if reading on a mobile device you may need to disable silent mode to hear this.

  </figcaption>
</figure>

At this speed, ten notes per second, it would take you more than a day and a half of uninterrupted listening to reach Beethoven's notes.

Turning voicings into numbers has afforded us another opportunity and it's with this that I'll finish. We can use a non-repeating random number generator to listen to each and every voicing. There's no telling when you'll hit Beethoven's notes, but they're in there. To hear all the voicings, to make sure you hear Beethoven's, would take over five days and sixteen hours.

<figure>
  <button id="string-quartet-random-control">
    Play
  </button>

  <figcaption>

  **Note** if reading on a mobile device you may need to disable silent mode to hear this.

  </figcaption>
</figure>

***

Combinatorics afforded us a path via which we could navigate an initially overwhelming space of possibilities. And not only that, it has also allowed us to walk a path which visited an Australian literary classic, confusing dice games from Early modern Britain, attempts to explain the bewildering complexities of DNA, and some really banging Beethoven.

<script type="module" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/javascript/string-quartet-voicings.js"></script>

[^1]: [Deciphering the Genetic Code: The Most Beautiful False Theory in Biochemistry – Part 1](https://www.chemistryviews.org/details/ezine/11312121/Deciphering_the_Genetic_Code_The_Most_Beautiful_False_Theory_in_Biochemistry__Pa/)
[^2]: My apologies, there's already a 'v' and 'w' is just the next letter in the alphabet. If it helps pronounce it "first wiolin" in your head. Or out loud. Do whatever best appeals.
