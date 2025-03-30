---
title: Chemistry by Raymond Chang - Problem 2.130
tags:
  - mathematics
  - chemistry
description: |
  A sample problem from Raymond Chang's Chemistry Fifth Edition. I'm unsure of my approach so am writing it up.
---

A sample containing $\text{NaCl}$, $\text{Na}_2\text{SO}_4$ and $\text{NaNO}_3$ gives the following elemental analysis: Na 32.08%, O 36.01% and Cl 19.51%. Calculate the mass percent of each compound in the sample.

The relevant molar masses are as follows.

* $\text{Na}$ - $22.99\text{g}/\text{mol}$
* $\text{Cl}$ - $35.45\text{g}/\text{mol}$
* $\text{S}$ - $32.07\text{g}/\text{mol}$
* $\text{O}$ - $16.00\text{g}/\text{mol}$
* $\text{N}$ - $14.01\text{g}/\text{mol}$

We assume a $100\text{g}$ sample and can then calculate the mass of $\text{NaCl}$ as the ratio of $\text{Cl}$ to $\text{Na}$ is 1:1, only one compound contains $\text{Cl}$ and the mass of $\text{Cl}$ is known.

$$
19.51 \text{g}
  \times \frac{1 \text{mol}_{\text{Cl}}}{35.45 \text{g}}
  \times \frac{1 \text{mol}_{\text{NaCl}}}{1 \text{mol}_{\text{Cl}}}
  \times \frac{22.99 \text{g} + 35.45 \text{g}}{1 \text{mol}_{\text{NaCl}}}
$$

$$
= 0.5504 \text{mol}_{\text{Cl}}
  \times \frac{1 \text{mol}_{\text{NaCl}}}{1 \text{mol}_{\text{Cl}}}
  \times \frac{22.99 \text{g} + 35.45 \text{g}}{1 \text{mol}_{\text{NaCl}}}
$$

$$
= 32.17 \text{g}
$$

The mass of the remaining two compounds depend on each other. They each share the remaining $\text{Na}$ as well as the $36.01 \text{g}$ of $\text{O}$.

<div class="wide">
  <div class="wide-content">

  $$
  (\text{Na}_2\text{SO}_4)\text{g} =
    \frac
      {(22.99 \times 2 + 32.07 + 16.00 \times 4) \text{g}}
      {1 \text{mol}_{\text{Na}_2\text{SO}_4}}
      \times (
        36.01 \text{g}
        -
        (\text{NaNO}_3)\text{g}
          \times \frac
                    {16.00 \text{g} \times 3}
                    {
                      22.99 \text{g}
                      +
                      14.01 \text{g}
                      +
                      16.00 \text{g} \times 3
                    }
      )
      \times \frac{1 \text{mol}_{\text{O}}}{16.00 \text{g}}
      \times \frac{1 \text{mol}_{\text{Na}_2\text{SO}_4}}{4 \text{mol}_{\text{O}}}
  $$

  $$
  = \frac{142.1 \text{g}}{1 \text{mol}_{\text{Na}_2\text{SO}_4}}
    \times (
      36.01 \text{g}
      -
      (\text{NaNO}_3)\text{g}
        \times \frac
                  {16.00 \text{g} \times 3}
                  {85 \text{g}}
    )
    \times \frac{1 \text{mol}_{\text{O}}}{16.00 \text{g}}
    \times \frac{1 \text{mol}_{\text{Na}_2\text{SO}_4}}{4 \text{mol}_{\text{O}}}
  $$

  $$
  = \frac{142.1 \text{g}}{1 \text{mol}_{\text{Na}_2\text{SO}_4}}
    \times (36.01 \text{g} - (\text{NaNO}_3)\text{g} \times 0.5647)
    \times \frac{1 \text{mol}_{\text{O}}}{16.00 \text{g}}
    \times \frac{1 \text{mol}_{\text{Na}_2\text{SO}_4}}{4 \text{mol}_{\text{O}}}
  $$

  $$
  = 142.1 \text{g}
    \times (\text{mol}_{\text{Na}_2\text{SO}_4})^{-1}
    \times (
      36.01 \times \text{g}
      -
      (\text{NaNO}_3) \times \text{g} \times 0.5647
    )
    \times \text{mol}_{\text{O}}
    \times 16.00^{-1}
    \times \text{g}^{-1}
    \times \text{mol}_{\text{Na}_2\text{SO}_4}
    \times 4^{-1}
    \times (\text{mol}_{\text{O}})^{-1}
  $$

  $$
  = 142.1
    \times 16.00^{-1}
    \times 4^{-1}
    \times (
      36.01 \times \text{g}
      - (\text{NaNO}_3) \times \text{g} \times 0.5647
    )
    \times \text{g}
  $$

  $$
  = 142.1
    \times 64.00^{-1}
    \times (
      36.01 \times \text{g}
      - (\text{NaNO}_3) \times \text{g} \times 0.5647
    )
    \times \text{g}
  $$

  $$
  = 2.220
    \times (36.01 \text{g} - (\text{NaNO}_3) \times \text{g} \times 0.5647)
    \times \text{g}
  $$

  $$
  = 2.220
    \times 36.01
    \times \text{g}
    -
    2.220
    \times (\text{NaNO}_3)
    \times 0.5647
    \times \text{g}
  $$

  $$
  = 79.94\text{g} - 1.254 \times (\text{NaNO}_3) \text{g}
  $$

  </div>
</div>

From this we get an equation for $(\text{Na}_2\text{SO}_4)\text{g}$ which depends on $(\text{NaNO}_3) \text{g}$.

$$
(\text{Na}_2\text{SO}_4)\text{g} = 79.94\text{g} - 1.254 \times (\text{NaNO}_3) \text{g}
$$

We can calculate an alternative equation for $(\text{Na}_2\text{SO}_4)\text{g}$, referring instead to the remaining $\text{Na}$.

To simplyfy things a little we can calculate the $\text{Na}$ remaining after $\text{NaCl}$ has been accounted for.

$$
32.08 \text{g} -
32.17 \text{g} \times \frac{22.99 \text{g}}{22.99 \text{g} + 35.45\text{g}}
$$

$$
= 32.08 \text{g} -
  32.17 \text{g} \times 0.3934
$$

$$
= 32.08 \text{g} - 12.66 \text{g}
$$

$$
= 19.42 \text{g}
$$

Which we can now use.

<div class="wide">
  <div class="wide-content">

  $$
  (\text{Na}_2\text{SO}_4)\text{g} =
    \frac{142.1 \text{g}}{1 \text{mol}_{\text{Na}_2\text{SO}_4}}
      \times (
        19.42 \text{g}
        -
        (\text{NaNO}_3)\text{g}
          \times \frac
                    {22.99 \text{g}}
                    {
                      22.99 \text{g}
                      +
                      14.01 \text{g}
                      +
                      16.00 \text{g} \times 3
                    }
      )
      \times \frac{1 \text{mol}_{\text{Na}}}{22.99 \text{g}}
      \times \frac
                {1 \text{mol}_{\text{Na}_2\text{SO}_4}}
                {2 \text{mol}_{\text{Na}}}
  $$

  $$
  (\text{Na}_2\text{SO}_4)\text{g} =
    \frac{142.1 \text{g}}{1 \text{mol}_{\text{Na}_2\text{SO}_4}}
      \times (
        19.42 \text{g}
        -
        (\text{NaNO}_3)\text{g}
          \times \frac{22.99 \text{g}}{85 \text{g}}
      )
      \times \frac{1 \text{mol}_{\text{Na}}}{22.99 \text{g}}
      \times \frac
                {1 \text{mol}_{\text{Na}_2\text{SO}_4}}
                {2 \text{mol}_{\text{Na}}}
  $$

  $$
  = \frac{142.1 \text{g}}{1 \text{mol}_{\text{Na}_2\text{SO}_4}}
      \times (
        19.42 \text{g}
        -
        (\text{NaNO}_3)\text{g} \times 0.2705
      )
      \times \frac{1 \text{mol}_{\text{Na}}}{22.99 \text{g}}
      \times \frac
                {1 \text{mol}_{\text{Na}_2\text{SO}_4}}
                {2 \text{mol}_{\text{Na}}}
  $$

  $$
  = 142.1
    \times \text{g}
    \times (\text{mol}_{\text{Na}_2\text{SO}_4})^{-1}
    \times (19.42 \text{g} - (\text{NaNO}_3)\text{g} \times 0.2705)
    \times \text{mol}_{\text{Na}}
    \times 22.99^{-1}
    \times \text{g}^{-1}
    \times \text{mol}_{\text{Na}_2\text{SO}_4}
    \times 2^{-1}
    \times (\text{mol}_{\text{Na}})^{-1}
  $$

  $$
  = 142.1
    \times 22.99^{-1}
    \times 2^{-1}
    \times (19.42 \text{g} - (\text{NaNO}_3)\text{g} \times 0.2705)
    \times \text{g}
    \times \text{g}^{-1}
    \times \text{mol}_{\text{Na}_2\text{SO}_4}
    \times (\text{mol}_{\text{Na}_2\text{SO}_4})^{-1}
    \times \text{mol}_{\text{Na}}
    \times (\text{mol}_{\text{Na}})^{-1}
  $$

  $$
  = 142.1
    \times 45.98^{-1}
    \times (19.42 \text{g} - (\text{NaNO}_3)\text{g} \times 0.2705)
  $$

  $$
  = 3.090
    \times (19.42 \text{g} - (\text{NaNO}_3)\text{g} \times 0.2705)
  $$

  $$
  = 60.01 \text{g} -
    0.8358 \times (\text{NaNO}_3)\text{g}
  $$

  </div>
</div>

Now we have a system of two equations with two unknowns.

$$
(\text{Na}_2\text{SO}_4)\text{g}
  = 79.94\text{g} - 1.254 \times (\text{NaNO}_3) \text{g}
$$

$$
(\text{Na}_2\text{SO}_4)\text{g}
  = 60.01 \text{g} - 0.8358 \times (\text{NaNO}_3)\text{g}
$$

To solve, substitute the first equation into the second.

$$
79.94\text{g} - 1.254 \times (\text{NaNO}_3) \text{g}
  = 60.01 \text{g} - 0.8358 \times (\text{NaNO}_3)\text{g}
$$

Then get $\text{NaNO}_3$ on one side.

$$
0.8358 \times (\text{NaNO}_3)\text{g} - 1.254 \times (\text{NaNO}_3) \text{g}
  = 60.01 \text{g} - 79.94\text{g}
$$

$$
(\text{NaNO}_3) \text{g}
  = (60.01 \text{g} - 79.94\text{g}) \times (0.8358 - 1.254)^{-1}
$$

Evaluate.

$$
(\text{NaNO}_3) \text{g}
  = −19.93\text{g} \times −0.4182^{-1}
$$

$$
(\text{NaNO}_3) \text{g} = 47.66 \text{g}
$$

Put back into the first equation and evaluate.

$$
(\text{Na}_2\text{SO}_4)\text{g}
  = 79.94\text{g} - 1.254 \times 47.66 \text{g}
$$

$$
(\text{Na}_2\text{SO}_4)\text{g}
  = 79.94\text{g} - 59.77 \text{g}
$$

$$
(\text{Na}_2\text{SO}_4)\text{g}
  = 20.17 \text{g}
$$

The percentages, by this method, are as follows.

* $\text{NaCl} = 32.17\%$
* $\text{NaNO}_3 = 47.66\%$
* $\text{Na}_2\text{SO}_4 = 20.17\%$

Which is satisfying because it adds to exactly 100%.

$$
32.17\% + 47.66\% + 20.17\% = 100\%
$$

But it is different to the answers in the book.

* $\text{NaCl} = 32.17\%$
* $\text{NaNO}_3 = 47.75\%$
* $\text{Na}_2\text{SO}_4 = 20.09\%$

Alternatively we can derive equations for $\text{NaNO}_3$. First, from $\text{O}$.

<div class="wide">
  <div class="wide-content">

  $$
  (\text{NaNO}_3) \text{g} = \frac
      {(22.99 + 14.01 + 16.00 \times 3) \text{g}}
      {1 \text{mol}_{\text{NaNO}_3}}
      \times (
        36.01 \text{g}
        -
        (\text{Na}_2\text{SO}_4)\text{g}
          \times \frac
                    {16.00 \text{g} \times 4}
                    {
                      22.99 \text{g} \times 2
                      +
                      32.07 \text{g}
                      +
                      16.00 \text{g} \times 4
                    }
      )
      \times \frac{1 \text{mol}_{\text{O}}}{16.00 \text{g}}
      \times \frac{1 \text{mol}_{\text{NaNO}_3}}{3 \text{mol}_{\text{O}}}
  $$

  $$
  = \frac
      {85.00 \text{g}}
      {1 \text{mol}_{\text{NaNO}_3}}
      \times (
        36.01 \text{g}
        -
        (\text{Na}_2\text{SO}_4)\text{g}
          \times \frac{16.00 \text{g} \times 4}{142.1 \text{g}}
      )
      \times \frac{1 \text{mol}_{\text{O}}}{16.00 \text{g}}
      \times \frac{1 \text{mol}_{\text{NaNO}_3}}{3 \text{mol}_{\text{O}}}
  $$

  $$
  = \frac {85.00 \text{g}}{1 \text{mol}_{\text{NaNO}_3}}
    \times(36.01 \text{g} - (\text{Na}_2\text{SO}_4)\text{g} \times 0.4504)
    \times \frac{1 \text{mol}_{\text{O}}}{16.00 \text{g}}
    \times \frac{1 \text{mol}_{\text{NaNO}_3}}{3 \text{mol}_{\text{O}}}
  $$

  $$
  = 85.00
    \times \text{g}
    \times (\text{mol}_{\text{NaNO}_3})^{-1}
    \times(
      36.01 \times \text{g} -
      (\text{Na}_2\text{SO}_4) \times \text{g} \times 0.4504
    )
    \times \text{mol}_{\text{O}}
    \times 16.00^{-1}
    \times \text{g}^{-1}
    \times \text{mol}_{\text{NaNO}_3}
    \times 3^{-1}
    \times (\text{mol}_{\text{O}})^{-1}
  $$

  $$
  = 85.00
    \times 16.00^{-1}
    \times 3^{-1}
    \times(
      36.01 \times \text{g} -
      (\text{Na}_2\text{SO}_4) \times \text{g} \times 0.4504
    )
    \times \text{g}
    \times \text{g}^{-1}
    \times \text{mol}_{\text{NaNO}_3}
    \times (\text{mol}_{\text{NaNO}_3})^{-1}
    \times \text{mol}_{\text{O}}
    \times (\text{mol}_{\text{O}})^{-1}
  $$

  $$
  = 85.00
    \times 16.00^{-1}
    \times 3^{-1}
    \times (
      36.01 \times \text{g} -
      (\text{Na}_2\text{SO}_4) \times \text{g} \times 0.4504
    )
  $$

  $$
  = 85.00
    \times 48^{-1}
    \times (
      36.01 \times \text{g} -
      (\text{Na}_2\text{SO}_4) \times \text{g} \times 0.4504
    )
  $$

  $$
  = 1.771 \times (
      36.01 \times \text{g} -
      (\text{Na}_2\text{SO}_4) \times \text{g} \times 0.4504
    )
  $$

  $$
  = 1.771 \times 36.01 \times \text{g} -
    1.771 \times (\text{Na}_2\text{SO}_4) \times \text{g} \times 0.4504
  $$

  $$
  = 63.77 \text{g} -
    0.7977 \times (\text{Na}_2\text{SO}_4) \text{g}
  $$

  </div>
</div>

Second, from the remaining $\text{Na}$.

<div class="wide">
  <div class="wide-content">

  $$
  (\text{NaNO}_3)\text{g} =
    \frac{85.00 \text{g}}{1 \text{mol}_{\text{NaNO}_3}}
      \times (
        19.42 \text{g}
        -
        (\text{Na}_2\text{SO}_4)\text{g}
          \times \frac{22.99 \text{g} \times 2}{142.1 \text{g}}
      )
      \times \frac{1 \text{mol}_{\text{Na}}}{22.99 \text{g}}
      \times \frac
                {1 \text{mol}_{\text{NaNO}_3}}
                {1 \text{mol}_{\text{Na}}}
  $$

  $$
  = \frac{85.00 \text{g}}{1 \text{mol}_{\text{NaNO}_3}}
    \times (
      19.42 \text{g}
      -
      (\text{Na}_2\text{SO}_4)\text{g} \times 0.3236
    )
    \times \frac{1 \text{mol}_{\text{Na}}}{22.99 \text{g}}
    \times \frac{1 \text{mol}_{\text{NaNO}_3}}{1 \text{mol}_{\text{Na}}}
  $$

  $$
  = 85.00
    \times \text{g}
    \times (\text{mol}_{\text{NaNO}_3})^{-1}
    \times (
      19.42 \text{g}
      -
      (\text{Na}_2\text{SO}_4)\text{g} \times 0.3236
    )
    \times \text{mol}_{\text{Na}}
    \times 22.99^{-1}
    \times \text{g}^{-1}
    \times \text{mol}_{\text{NaNO}_3}
    \times (\text{mol}_{\text{Na}})^{-1}
  $$

  $$
  = 85.00
    \times 22.99^{-1}
    \times (
      19.42 \text{g}
      -
      (\text{Na}_2\text{SO}_4)\text{g} \times 0.3236
    )
    \times \text{g}
    \times \text{g}^{-1}
    \times \text{mol}_{\text{NaNO}_3}
    \times (\text{mol}_{\text{NaNO}_3})^{-1}
    \times \text{mol}_{\text{Na}}
    \times (\text{mol}_{\text{Na}})^{-1}
  $$

  $$
  = 85.00
    \times 22.99^{-1}
    \times (
      19.42 \text{g}
      -
      (\text{Na}_2\text{SO}_4)\text{g} \times 0.3236
    )
  $$

  $$
  = 3.697 \times (
    19.42 \text{g}
    -
    (\text{Na}_2\text{SO}_4)\text{g} \times 0.3236
  )
  $$

  $$
  = 3.697 \times 19.42 \text{g}
    -
    3.697 \times 0.3236 \times (\text{Na}_2\text{SO}_4)\text{g}
  $$

  $$
  = 71.80 \text{g}
    -
    1.196 \times (\text{Na}_2\text{SO}_4)\text{g}
  $$

  </div>
</div>

Now we have a different system of equations.

$$
(\text{NaNO}_3)\text{g} =
  63.77 \text{g} - 0.7977 \times (\text{Na}_2\text{SO}_4) \text{g}
$$

$$
(\text{NaNO}_3)\text{g} =
  71.80 \text{g} - 1.196 \times (\text{Na}_2\text{SO}_4)\text{g}
$$

To solve.

$$
63.77 \text{g} - 0.7977 \times (\text{Na}_2\text{SO}_4) \text{g} =
  71.80 \text{g} - 1.196 \times (\text{Na}_2\text{SO}_4)\text{g}
$$

$$
(\text{Na}_2\text{SO}_4)\text{g} =
  (71.80 \text{g} - 63.77 \text{g}) \times (1.196 - 0.7977)^{-1}
$$

$$
(\text{Na}_2\text{SO}_4)\text{g} =
  8.030 \text{g} \times 0.3983^{-1}
$$

$$
(\text{Na}_2\text{SO}_4)\text{g} = 20.16 \text{g}
$$

$$
(\text{NaNO}_3)\text{g} =
  63.77 \text{g} - 0.7977 \times 20.16 \text{g}
$$

$$
(\text{NaNO}_3)\text{g} =
  63.77 \text{g} - 16.08 \text{g}
$$

$$
(\text{NaNO}_3)\text{g} = 47.69 \text{g}
$$

Yielding this set of percentages.

* $\text{NaCl} = 32.17\%$
* $\text{NaNO}_3 = 47.69\%$
* $\text{Na}_2\text{SO}_4 = 20.16\%$

Mixing the two sets, so we have $(\text{Na}_2\text{SO}_4)\text{g}$ derived from the remaining $\text{Na}$ and $(\text{NaNO}_3)\text{g}$ from the $\text{O}$, we can get this system.

$$
(\text{Na}_2\text{SO}_4)\text{g}
  = 60.01 \text{g} - 0.8358 \times (\text{NaNO}_3)\text{g}
$$

$$
(\text{NaNO}_3)\text{g} =
  63.77 \text{g} - 0.7977 \times (\text{Na}_2\text{SO}_4) \text{g}
$$

With this solution.

$$
(\text{NaNO}_3)\text{g} =
  63.77 \text{g} - 0.7977 \times (60.01 \text{g} - 0.8358 \times (\text{NaNO}_3)\text{g})
$$

$$
(\text{NaNO}_3)\text{g} =
  63.77 \text{g} + (
    -0.7977 \times 60.01 \text{g} +
    -0.7977 \times -0.8358 \times (\text{NaNO}_3)\text{g}
  )
$$

$$
(\text{NaNO}_3)\text{g} =
  63.77 \text{g} +
  -47.869977 \text{g} +
  0.6667 \times (\text{NaNO}_3)\text{g}
$$

$$
1 \times (\text{NaNO}_3)\text{g} + -0.6667 \times (\text{NaNO}_3)\text{g} =
  63.77 \text{g} + -47.87 \text{g}
$$

$$
(1 + -0.6667) \times (\text{NaNO}_3)\text{g} =
  63.77 \text{g} + -47.87 \text{g}
$$

$$
(\text{NaNO}_3)\text{g} =
  63.77 \text{g} + -47.87 \text{g} \times (1 + -0.6667)^{-1}
$$

$$
(\text{NaNO}_3)\text{g} =
  15.9 \text{g} \times 0.3333^{-1}
$$

$$
(\text{NaNO}_3)\text{g} = 47.70 \text{g}
$$

$$
(\text{Na}_2\text{SO}_4)\text{g}
  = 60.01 \text{g} - 0.8358 \times 47.70 \text{g}
$$

$$
(\text{Na}_2\text{SO}_4)\text{g}
  = 20.14 \text{g}
$$

Yielding this set of percentages.

* $\text{NaCl} = 32.17\%$
* $\text{NaNO}_3 = 47.70\%$
* $\text{Na}_2\text{SO}_4 = 20.14\%$
