---
title: Concentrations
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

Each of these can be used to derive other, more obviously useful, I think, equations.

$$
\begin{multline}
c_a v_a + c_b v_b = c (v_a + v_b) \\
\iff v_a (c_a - c) + v_b (c_b + c) = 0
\end{multline}
$$

$$
\begin{multline}
c_a (v - v_b) + c_b v_b = c v \\
\iff v (c_a - c) - v_b (c_a + c_b) = 0
\end{multline}
$$

$$
\begin{multline}
c_a v_a + c_b (v - v_a) = c v \\
\iff v_a (c_a - c_b) + v (c_b - c) = 0
\end{multline}
$$

From here I can enumerate all the possible equations for each variable.

## In terms of _c_

$$
c = \frac{c_a v_a + c_b v_b}{v_a + v_b}
$$

$$
c = \frac{c_a (v - v_b) + c_b v_b}{v}
$$

$$
c = \frac{c_a v_a + c_b (v - v_a)}{v}
$$

## In terms of _v_

$$
v = v_a + v_b
$$

$$
v = \frac{v_a(c_b - c_a)}{c_b - c}
$$

$$
v = \frac{v_b(c_a - c_b)}{c_a - c}
$$

## In terms of _v<sub>a</sub>_ / _v<sub>b</sub>_

$$
v_a = v - v_b
$$

$$
v_a = \frac{v_b (c_b - c)}{c - c_a}
$$

$$
v_a = \frac{v (c_b - c)}{c_b - c_a}
$$

$$
v_b = v - v_a
$$

$$
v_b = \frac{v_a (c_a - c)}{c - c_b}
$$

$$
v_b = \frac{v (c_a - c)}{c_a - c_b}
$$

## In terms of _c<sub>a</sub>_ / _c<sub>b</sub>_

$$
c_a = \frac{c (v_a + v_b) - c_b v_b}{v_a}
$$

$$
c_a = \frac{c v - c_b (v - v_a)}{v_a}
$$

$$
c_a = \frac{c v - c_b v_b}{v - v_b}
$$

$$
c_b = \frac{c (v_b + v_a) - c_a v_a}{v_b}
$$

$$
c_b = \frac{c v - c_a (v - v_b)}{v_b}
$$

$$
c_b = \frac{c v - c_a v_a}{v - v_a}
$$
