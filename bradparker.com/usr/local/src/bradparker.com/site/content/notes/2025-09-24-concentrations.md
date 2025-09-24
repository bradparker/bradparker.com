---
title: Concentrations
tags:
  - Mathematics
description: |
  In general
---

Mixing different concentrations, in general.

$$
c_a v_a + c_b v_b = c v
$$

$$
\begin{align*}
v &= v_a + v_b \\
v_a &= v - v_b \\
v_b &= v - v_a
\end{align*}
$$

## Volumes

Volume $v_a$ where $v$, $c$, $c_a$ and $c_b$ are known.

$$
v_a = \frac{v(c - c_b)}{c_a - c_b}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{multline}
  c_a v_a + c_b v_b = c v \\
  c_a v_a = c v - c_b v_b \\
  c_a v_a = c v - c_b (v - v_a) \\
  c_a v_a = c v - c_b v + c_b v_a \\
  c_a v_a = v (c - c_b) + c_b v_a \\
  c_a v_a - c_b v_a = v (c - c_b) \\
  v_a (c_a - c_b) = v (c - c_b) \\
  v_a = \frac{v (c - c_b)}{c_a - c_b} \\
  \end{multline}
  $$

  </div>
</details>


