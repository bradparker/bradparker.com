---
title: Trigonometry — The Angle Sum Theorem
description: |
  Description...
tags:
  - Mathematics
---

## Definitions

<svg viewBox="0 0 100 50">
  <style>
    text { font: italic 4px serif; }
  </style>
  <polygon
    fill="none"
    stroke="black"
    stroke-width="0.3"
    points="
      10,40
      30,10
      90,40
    "
  />
  <text x="8" y="42" text-anchor="end">A</text>
  <text x="30" y="8">B</text>
  <text x="92" y="42">C</text>
  <line x1="30" y1="10" x2="30" y2="40" fill="none" stroke="black" stroke-width="0.3" />
  <text x="32" y="38">D</text>
</svg>

$$
\begin{align*}
\alpha  &= \angle BAD \\
a       &= \overbar{BC} \\
\beta_1  &= \angle ABD \\
b_1      &= \overbar{AD} \\
\beta_2 &= \angle DBC \\
b_2     &= \overbar{DC} \\
\gamma  &= \angle BCD \\
c       &= \overbar{AB} \\
h       &= \overbar{BD}
\end{align*}
$$

## Observations

$$
\begin{multline}
\sin \alpha = \cos \beta_1  = \frac{h}{c} \\
\sin \gamma = \cos \beta_2 = \frac{h}{a} \\
\cos \alpha = \sin \beta_1  = \frac{b_1}{c} \\
\cos \gamma = \sin \beta_2 = \frac{b_2}{a} \\
c^2 = b_1^2 + h^2 \\
a^2 = b_2^2 + h^2
\end{multline}
$$

## Hypothesis

$$
\cos \left( \alpha + \beta_1 + \beta_2 + \gamma \right) = -1
$$

## Proof

<figure class="wide">

$$
\begin{multline}
\cos \left( \alpha + \beta_1 + \beta_2 + \gamma \right) = -1 \\
\implies \\
\cos \left( \alpha + \beta_1 \right) \cos \left( \beta_2 + \gamma \right) -
  \sin \left( \alpha + \beta_1 \right) \sin \left( \beta_2 + \gamma \right)
  = -1 \\
\implies \\
\left( \cos \alpha \cos \beta_1 - \sin \alpha \sin \beta_1 \right)
  \left( \cos \beta_2 \cos \gamma - \sin \beta_2 \sin \gamma \right) -
    \left( \sin \alpha \cos \beta_1 + \cos \alpha \sin \beta_1 \right)
    \left( \sin \beta_2 \cos \gamma + \cos \beta_2 \sin \gamma \right)
  = -1 \\
\implies \\
\left( \frac{b_1}{c} \cdot \frac{h}{c} - \frac{h}{c} \cdot \frac{b_1}{c} \right)
  \left( \frac{h}{a} \cdot \frac{b_2}{a} - \frac{b_2}{a} \cdot \frac{h}{a} \right) -
    \left( \frac{h}{c} \cdot \frac{h}{c} + \frac{b_1}{c} \cdot \frac{b_1}{c} \right)
    \left( \frac{b_2}{a} \cdot \frac{b_2}{a} + \frac{h}{a} \cdot \frac{h}{a} \right)
  = -1 \\
\implies \\
0 -
  \left( \frac{h^2 + b_1^2}{c^2} \right)
  \left( \frac{b_2^2 + h^2}{a^2} \right)
  = -1 \\
\implies \\
0 -
  \left( \frac{c^2}{c^2} \right)
  \left( \frac{a^2}{a^2} \right)
  = -1 \\
\implies \\
0 - 1 \left( 1 \right) = -1 \\
\implies \\
0 - 1 = -1 \\
\QED
\end{multline}
$$

</figure>
