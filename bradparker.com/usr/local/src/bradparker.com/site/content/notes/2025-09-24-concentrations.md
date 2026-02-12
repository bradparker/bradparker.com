---
title: Concentrations
tags:
  - Mathematics
description: |
  In general
---

Mixing different concentrations (mixing solutions containing the same solute), in general.

$$
c_a v_a + c_b v_b = c v, v = v_a + v_b, (c_a, v_a, c_b, v_b) \ge 0
$$

This makes intuitive sense to me.

And so...

$$
\begin{align*}
v_a &= v - v_b \\
v_b &= v - v_a
\end{align*}
$$

## Implied volumes

Volume $v_a$ where $v_b$ is unknown, and vice versa. (E.G. Given two solutions, one at $c_a \frac{\text{mol}}{\text{L}}$ and another at $c_b \frac{\text{mol}}{\text{L}}$, how much of each will be needed to produce $v \text{L}$ of a solution at $c \frac{\text{mol}}{\text{L}}$?)

$$
\begin{multline}
v_a = \frac{v(c - c_b)}{c_a - c_b}, c_a \ne c_b \\
v_b = v - v_a
\end{multline}
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

What happens if $c_a \lt c_b$? To simplify let $c_a = 0$, something of a worst case. This implies the following.

$$
c_b v_b = c v, v = v_a + v_b
$$

Because we know that $v = v_a + v_b$, and $(v_a, v_b) \ge 0$, we know that $(v_a, v_b) \le v$. Which means that, in this case, $c \le c_b$. Both numerator and denominator become negative, meaning the result is positive. I dunno if I have the chops to extend this to $0 \lt c_a \lt c_b$.

What happens if $c_a = c_b$? Well, then the denominator would be 0, implying this is somehow an invalid condition. Which makes sense, the concentration can't change when mixing two solutions with the same concentration...

$$
\begin{align*}
c_a v_a + c_a v_b &= c v, v = v_a + v_b \\
c_a(v_a + v_b) &= c v, v = v_a + v_b \\
c_a(v_a + v_b) &= c (v_a + v_b) \\
c_a &= c
\end{align*}
$$

## Implied concentrations

The final concentration where any two volumes and both initial concentrations are known. (E.G. What would be the concentration of a solution created by mixing $v_a \text{L}$ of a solution at $c_a \frac{\text{mol}}{\text{L}}$ and $v_b \text{L}$ of a solution at $c_b \frac{\text{mol}}{\text{L}}$?)

$$
c = \frac{c_a v_a + c_b v_b}{v}
$$

An initial concentration where any two volumes and the two other concentrations are known. (E.G. Given $v_b \text{L}$ of a solution at $c_b \frac{\text{mol}}{\text{L}}$ at what concentration would $v_a \text{L}$ (where $v_a = v - v_b$) of solution have to be to produce $v \text{L}$ of solution at $c \frac{\text{mol}}{\text{L}}$?)

$$
\begin{multline}
c_a v_a + c_b v_b = c v \\
c_a v_a = c v - c_b v_b \\
c_a = \frac{c v - c_b v_b}{v_a} \\
\end{multline}
$$

This one feels closely related to dilution. Something about the volumes being fixed?

If $c_a = 0$.

$$
\begin{align*}
0 &= \frac{c v - c_b v_b}{v_a} \\
0 &= c v - c_b v_b \\
c_b v_b &= c v
\end{align*}
$$

## Dilution as a special case

Given...

$$
c_a v_a + c_b v_b = c v, v = v_a + v_b
$$

And with $c_b = 0$ we get...

$$
c_a v_a = c v
$$

Which is the same as the equation for dilutions of solutions, usually expressed as...

$$
M_i V_i = M_f V_f
$$

If we know $c_a$, $v_a$ and $c$, then $v$ can be derived. (E.G. Given $v_a \text{mL}$ of a solution at $c_a \frac{\text{mol}}{\text{L}}$, how much solution would you have if diluted to $c \frac{\text{mol}}{\text{L}}$?)

$$
v = \frac{c_a v_a}{c}
$$

If we know $c_a$, $c$ and $v$ then $v_a$ can be derived. (E.G. How much solution at $c_a \frac{\text{mol}}{\text{L}}$ would be needed to produce $v \text{L}$ of solution at $c \frac{\text{mol}}{\text{L}}$?)

$$
v_a = \frac{c v}{c_a}
$$

Which can also be viewed as the more general equation above, where $c_b = 0$.

$$
\begin{align*}
v_a &= \frac{v(c - c_b)}{c_a - c_b} \\
    &= \frac{v(c - 0)}{c_a - 0} \\
    &= \frac{c v}{c_a}
\end{align*}
$$

It looks like we only ever need to know two concentrations, and two volumes, to work everything else out.
