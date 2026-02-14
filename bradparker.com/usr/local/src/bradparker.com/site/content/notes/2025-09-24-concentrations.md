---
title: "Concentrations: algebraically"
tags:
  - Mathematics
description: |
  Algebraically
---

An equation for mixing solutions with different concentrations of solute. Written a bit like a function from input solutions to an output solution; the process of mixing.

$$
c_a v_a + c_b v_b = c v, v = v_a + v_b
$$

Which implies the following three equations.

$$
\begin{multline}
c_a v_a + c_b v_b = c (v_a + v_b) \\
c_a (v - v_b) + c_b v_b = c v \\
c_a v_a + c_b (v - v_a) = c v
\end{multline}
$$

Each of these can be used to derive expressions for roots. Useful for finding expressions for each variable.

$$
\begin{multline}
c_a v_a + c_b v_b = c (v_a + v_b) \\
\iff c_a v_a + c_b v_b - c (v_a + v_b) = 0 \\
\iff v_a (c_a - c) + v_b (c_b - c) = 0
\end{multline}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  c_a v_a + c_b v_b = c (v_a + v_b) \\
  \iff c_a v_a + c_b v_b - c (v_a + v_b) = 0 \\
  \iff c_a v_a + c_b v_b - c v_a - c v_b = 0 \\
  \iff v_a (c_a - c) + v_b (c_b - c) = 0 \\
  \end{multline}
  $$

  </div>
</details>

$$
\begin{multline}
c_a (v - v_b) + c_b v_b = c v \\
\iff c_a (v - v_b) + c_b v_b - c v = 0 \\
\iff v (c_a - c) + v_b (c_b - c_a)  = 0
\end{multline}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  c_a (v - v_b) + c_b v_b = c v \\
  \iff c_a (v - v_b) + c_b v_b - c v = 0 \\
  \iff c_a v - c_a v_b + c_b v_b - c v = 0 \\
  \iff v (c_a - c) + v_b (c_b - c_a)  = 0
  \end{multline}
  $$

  </div>
</details>

$$
\begin{multline}
c_a v_a + c_b (v - v_a) = c v \\
\iff c_a v_a + c_b (v - v_a) - c v = 0 \\
\iff v_a (c_a - c_b) + v (c_b - c) = 0
\end{multline}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  c_a v_a + c_b (v - v_a) = c v \\
  \iff c_a v_a + c_b (v - v_a) - c v = 0 \\
  \iff c_a v_a + c_b v - c_b v_a - c v = 0 \\
  \iff v_a (c_a - c_b) + v (c_b - c) = 0 \\
  \end{multline}
  $$

  </div>
</details>

From here I can enumerate all the possible equations for each variable.

## Concentrations

### In terms of _c_

$$
c = \frac{c_a v_a + c_b v_b}{v_a + v_b}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  c_a v_a + c_b v_b - c (v_a + v_b) = 0 \\
  \iff c_a v_a + c_b v_b = c (v_a + v_b) \\
  \iff \frac{c_a v_a + c_b v_b}{(v_a + v_b)} = c \\
  \end{multline}
  $$

  </div>
</details>

$$
c = \frac{c_a (v - v_b) + c_b v_b}{v}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  c_a (v - v_b) + c_b v_b - c v = 0 \\
  \iff c_a (v - v_b) + c_b v_b = c v \\
  \iff \frac{c_a (v - v_b) + c_b v_b}{v} = c \\
  \end{multline}
  $$

  </div>
</details>

$$
c = \frac{c_a v_a + c_b (v - v_a)}{v}
$$

Commutativity means the prior expression is proved equivalently.

### In terms of _c<sub>a</sub>_ / _c<sub>b</sub>_

$$
c_a = \frac{c (v_a + v_b) - c_b v_b}{v_a}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  c_a v_a + c_b v_b - c (v_a + v_b) = 0 \\
  \iff c_b v_b - c (v_a + v_b) = -c_a v_a \\
  \iff \frac{c_b v_b - c (v_a + v_b)}{v_a} = -c_a \\
  \iff \frac{-(c_b v_b - c (v_a + v_b))}{v_a} = c_a \\
  \iff \frac{-c_b v_b + c (v_a + v_b)}{v_a} = c_a \\
  \iff \frac{c (v_a + v_b) - c_b v_b}{v_a} = c_a \\
  \end{multline}
  $$

  </div>
</details>

$$
c_a = \frac{c v - c_b (v - v_a)}{v_a}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  c_a v_a + c_b (v - v_a) - c v = 0 \\
  \iff c_b (v - v_a) - c v = -c_a v_a  \\
  \iff \frac{c_b (v - v_a) - c v}{v_a} = -c_a  \\
  \iff \frac{-(c_b (v - v_a) - c v)}{v_a} = c_a  \\
  \iff \frac{-c_b (v - v_a) + c v}{v_a} = c_a  \\
  \iff \frac{c v - c_b (v - v_a)}{v_a} = c_a  \\
  \end{multline}
  $$

  </div>
</details>

$$
c_a = \frac{c v - c_b v_b}{v - v_b}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  c_a (v - v_b) + c_b v_b - c v = 0 \\
  \iff c_b v_b - c v = -c_a (v - v_b)  \\
  \iff \frac{c_b v_b - c v}{v - v_b} = -c_a  \\
  \iff \frac{-(c_b v_b - c v)}{v - v_b} = c_a  \\
  \iff \frac{-c_b v_b + c v)}{v - v_b} = c_a  \\
  \iff \frac{c v - c_b v_b)}{v - v_b} = c_a  \\
  \end{multline}
  $$

  </div>
</details>

Commutativity means each of the next three expressions is proved equivalently to its companion among the prior three.

$$
c_b = \frac{c (v_b + v_a) - c_a v_a}{v_b}
$$

$$
c_b = \frac{c v - c_a (v - v_b)}{v_b}
$$

$$
c_b = \frac{c v - c_a v_a}{v - v_a}
$$

## Volumes

### In terms of _v_

$$
v = v_a + v_b
$$

By definition.

$$
v = \frac{v_a(c_b - c_a)}{c_b - c}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  v_a (c_a - c_b) + v (c_b - c) = 0 \\
  \iff v_a (c_a - c_b) = v (c_b - c) \\
  \iff \frac{v_a (c_a - c_b)}{c_b - c} = v \\
  \end{multline}
  $$

  </div>
</details>

$$
v = \frac{v_b(c_a - c_b)}{c_a - c}
$$

Commutativity means this is proved equivalently to the prior expression.

### In terms of _v<sub>a</sub>_ / _v<sub>b</sub>_

$$
v_a = v - v_b
$$

By definition of $v$ as $v_a + v_b$.

$$
v_a = \frac{v_b (c_b - c)}{c - c_a}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  v_a (c_a - c) + v_b (c_b - c) = 0 \\
  \iff v_a (c_a - c) = -v_b (c_b - c) \\
  \iff v_a = \frac{-v_b (c_b - c)}{c_a - c} \\
  \iff v_a = \frac{v_b (c_b - c)}{c - c_a} \\
  \end{multline}
  $$

  </div>
</details>

$$
v_a = \frac{v (c_b - c)}{c_b - c_a}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  v_a (c_a - c_b) + v (c_b - c) = 0 \\
  \iff v_a (c_a - c_b) = -v (c_b - c) \\
  \iff v_a = \frac{-v (c_b - c)}{c_a - c_b} \\
  \iff v_a = \frac{v (c_b - c)}{c_b - c_a} \\
  \end{multline}
  $$

  </div>
</details>

Commutativity means each of the next three expressions is proved equivalently to its companion among the prior three.

$$
v_b = v - v_a
$$

$$
v_b = \frac{v_a (c_a - c)}{c - c_b}
$$

$$
v_b = \frac{v (c_a - c)}{c_a - c_b}
$$


