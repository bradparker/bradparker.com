---
title: Quadratic equations
tags:
  - Mathematics
description: |
  Up and down
---

## Vertex form

$$
ax^2 + bx + c =
  a \left( x + \frac{b}{2a} \right)^2 + \frac{4ac - b^2}{4a}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  ax^2 + bx + c \\
  a \left( x^2 + \frac{bx}{a} + \frac{c}{a} \right) \\
  a \left( x^2 + \frac{bx}{a} + \left( \frac{b}{2a} \right)^2 + \frac{c}{a} - \left( \frac{b}{2a} \right)^2 \right) \\
  a \left( \left( x + \frac{b}{2a} \right)^2 + \frac{c}{a} - \left( \frac{b}{2a} \right)^2 \right) \\
  a \left( \left( x + \frac{b}{2a} \right)^2 + \frac{c}{a} - \frac{b^2}{4a^2} \right) \\
  a \left( \left( x + \frac{b}{2a} \right)^2 + \frac{4ca^2 - ab^2}{4a^3} \right) \\
  a \left( \left( x + \frac{b}{2a} \right)^2 + \frac{a \left( 4ac - b^2 \right)}{4a^3} \right) \\
  a \left( \left( x + \frac{b}{2a} \right)^2 + \frac{4ac - b^2}{4a^2} \right) \\
  a \left( x + \frac{b}{2a} \right)^2 + \frac{4ac - b^2}{4a} \\
  \end{multline}
  $$

  </div>
</details>

## Quadratic formula

$$
x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a} \iff ax^2 + bx + c = 0
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  ax^2 + bx + c = 0 \\
  a \left( x^2 + \frac{bx}{a} + \frac{c}{a} \right) = 0 \\
  x^2 + \frac{bx}{a} + \frac{c}{a} = 0 \\
  x^2 + \frac{bx}{a} + \left( \frac{b}{2a} \right)^2 + \frac{c}{a} - \left( \frac{b}{2a} \right)^2 = 0 \\
  \left( x + \frac{b}{2a} \right)^2 + \frac{c}{a} - \left( \frac{b}{2a} \right)^2 = 0 \\
  \left( x + \frac{b}{2a} \right)^2 + \frac{c}{a} - \frac{b^2}{4a^2} = 0 \\
  \left( x + \frac{b}{2a} \right)^2 + \frac{4ca^2 - ab^2}{4a^3} = 0 \\
  \left( x + \frac{b}{2a} \right)^2 + \frac{a \left( 4ac - b^2 \right)}{4a^3} = 0 \\
  \left( x + \frac{b}{2a} \right)^2 + \frac{4ac - b^2}{4a^2} = 0 \\
  \left( x + \frac{b}{2a} \right)^2 = \frac{b^2 - 4ac}{4a^2} \\
  x + \frac{b}{2a} = \pm \sqrt{\frac{b^2 - 4ac}{4a^2}} \\
  x + \frac{b}{2a} = \pm \frac{\sqrt{b^2 - 4ac}}{\sqrt{4a^2}} \\
  x + \frac{b}{2a} = \pm \frac{\sqrt{b^2 - 4ac}}{2a} \\
  x = \frac{-b}{2a} \pm \frac{\sqrt{b^2 - 4ac}}{2a} \\
  x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a} \\
  \end{multline}
  $$

  </div>
</details>

## Factored form

$$
ax^2 + bx + c = a \left(x - \frac{-b + \sqrt{b^2 - 4ac}}{2a} \right) \left(x - \frac{-b - \sqrt{b^2 - 4ac}}{2a} \right)
$$
