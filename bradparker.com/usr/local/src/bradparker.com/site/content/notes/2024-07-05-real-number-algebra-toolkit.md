---
title: Real number algebra toolkit
tags:
  - mathematics
description: |
  Toolkit for manipulating algebraic expressions dealing with Real numbers.
---

The Real numbers can be defined as a set, &#x211D;, along with two operators, + (addition) and &bullet; (multiplication).

$$
(\mathbb{R}, +, \bullet)
$$

Multiplication can alternatively be written omitting the operator (&bullet;) and its surrounding space.

$$
x \bullet y = xy
$$

## Axioms

The following 9 axioms complete the description.

### Addition is associative

$$
\forall x, y, z \in \mathbb{R} \hspace{3pt} x + (y + z) = (x + y) + z
$$

### Addition is commutative

$$
\forall x, y \in \mathbb{R} \hspace{3pt} x + y = y + x
$$

### Additive identity

There is an additive identity element.

$$
\exists 0 \in \mathbb{R} \ni \forall x \in \mathbb{R} \hspace{3pt} 0 + x = x
$$

### Additive inverse

There is an additive inverse for every element.

$$
\forall x \in \mathbb{R} \hspace{3pt} \exists -x \in \mathbb{R} \ni x + -x = 0
$$

There is special notation for addition where the right hand side is an additive inverse.

$$
x + -y = x - y
$$

### Multiplication is associative

$$
\forall x, y, z \in \mathbb{R} \hspace{3pt} x(yz) = (xy)z
$$

### Multiplication is commutative

$$
\forall x, y \in \mathbb{R} \hspace{3pt} xy = yx
$$

### Multiplicative identity

$$
\exists 1 \in \mathbb{R} \ni \forall x \in \mathbb{R} \hspace{3pt} 1 \bullet x = x
$$

### Multiplicative inverse

For every element, except 0, there is a multiplicative inverse.

$$
\forall x \in \{ \mathbb{R} - \{ 0 \} \} \hspace{3pt} \exists x^{-1} \in \mathbb{R} \ni xx^{-1} = 1
$$

There is special notation for when the right hand side of multiplication is a multiplicative inverse.

$$
xy^{-1} = \frac{x}{y}
$$

### Multiplication distributes over addition

$$
\forall x, y, z \in \mathbb{R} \hspace{3pt} x(y + z) = xy + xz
$$

## Toolkit

### Multiplication by zero

Multiplying any element by zero equals zero.

$$
\forall x \in \mathbb{R} \hspace{3pt} x0 = 0
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  0 = x0
  $$

  [The additive identity axiom](#additive-identity) tells us that $0 = 0 + 0$

  $$
  0 = x(0 + 0)
  $$

  [Multiplication distributes over addition](#multiplication-distributes-over-addition).

  $$
  0 = x0 + x0
  $$

  This matches [the additive identity axiom](#additive-identity) provided $x = x0 = 0$.

  $$
  \begin{align*}
  x + 0 &= x \\
  x0 + x0 &= 0
  \end{align*}
  $$

  Therefore

  $$
  x0 = 0
  $$

  </div>
</details>

### Multiplication by negative one

Multiplying any element by negative one yields the additive inverse for that element.

$$
\forall x \in \mathbb{R} \hspace{3pt} x(-1) = -x
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  Given the knowledge that [$0 = x0$](#multiplication-by-zero), and using the [additive inverse](#additive-inverse) for 1 we can write the following equation.

  $$
  0 = x(1 + -1)
  $$

  Which we can rewrite using the knowledge that [multiplication distributes over addition](#multiplication-distributes-over-addition)

  $$
  0 = x + x(-1)
  $$

  Which is the same as the equation for the [additive inverse](#additive-inverse) provided $-x = x(-1)$.

  $$
  \begin{align*}
    0 &= x + -x \\
    0 &= x + x(-1) \\
    \blacksquare
  \end{align*}
  $$

  </div>
</details>

### The additive inverse of an additive inverse is identity

$$
-(-x) = x
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  This is true by definition.

  $$
  x + -x = 0
  $$

  Now let $x$ be $-x$.

  $$
  -x + -(-x) = 0
  $$

  We can use the knowledge that [addition is commutative](#addition-is-commutative) to make the match to the additive inverse definition very clear.

  $$
  \begin{align*}
    0 &= -x + x \\
    0 &= -x + -(-x)
  \end{align*}
  $$

  </div>
</details>

### Additive inverse multiplied by additive inverse

When an additive inverse is multiplied by another additive inverse the result is doubly inverted.

$$
-x(-y) = xy
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  We can use the knowledge that [multiplying any element by negative one yields the additive inverse for that element](#multiplication-by-negative-one) to rewrite the expression.

  $$
  x(-1)(y(-1)) = xy
  $$

  Which we can rewrite using the knowledge that [multiplication is associative](#multiplication-is-associative).

  $$
  x(-1)y(-1) = xy
  $$

  Which we can rewrite using the knowledge that [multiplication is commutative](#multiplication-is-commutative).

  $$
  xy(-1)(-1) = xy
  $$

  Which we can rewrite using the knowledge that [multiplication by negative one is the additive inverse](#multiplication-by-negative-one).

  $$
  xy(-(-1)) = xy
  $$

  Which we can rewrite using the knowledge that [the additive inverse of an additive inverse is identity](#the-additive-inverse-of-an-additive-inverse-is-identity).

  $$
  xy1 = xy
  $$

  Which we can rewrite using the knowledge that [1 is the multiplicative identity element](#multiplicative-identity).

  $$
  xy = xy
  $$

  </div>
</details>

### Multiplication by an additive inverse produces an additive inverse

$$
x(-y) = -(xy)
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  Using the knowledge that [the additive inverse for an element is equal to multiplying that element by negative one](#multiplication-by-negative-one) we can write the following expression.

  $$
  x(y(−1)) = −(xy)
  $$

  Which we can rewrite using the knowledge that [multiplication is associative](#multiplication-is-associative).

  $$
  (xy)(−1) = −(xy)
  $$

  Which we can rewrite using the knowledge that [multiplication by negative one is equal to the additive inverse](#multiplication-by-negative-one).

  $$
  −(xy) = −(xy)
  $$

  </div>
</details>

### The additive inverse of a product is equal to either element being an additive inverse

$$
−(xy) = (−x)y = x(−y)
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  This has already been proved while proving that [multiplication by an additive inverse produces an additive inverse](#multiplication-by-an-additive-inverse-produces-an-additive-inverse).

  $$
  (xy)(−1) = −(xy)
  $$

  The extra steps that make it clearer are the use of the knowledge that [multiplication is associative](#multiplication-is-associative) &hellip;

  $$
  xy(−1) = −(xy)
  $$

  And [multiplication is commutative](#multiplication-is-commutative).

  $$
  xy(−1) = x(−1)y = −(xy)
  $$

  We can rewrite this expression using the knowledge that [multiplication by negative one is equal to the additive inverse](#multiplication-by-negative-one).

  $$
  x(−y) = (−x)y = −(xy)
  $$

  </div>
</details>

### Exponential notation

$$
\begin{align*}
n \in \mathbb{Z}. x^n
  &= 1 \overbrace{\bullet x \bullet x ...}^{n}, &0 \le n \\
  &= \overbrace{x^{-1} \bullet x^{-1} ...}^{|n|}, &n \lt 0
\end{align*}
$$

### Exponent of exponent

$$
(x^n)^m = x^{nm}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  (x^n)^m = \overbrace
    {
      1
        \overbrace{\bullet x \bullet x ...}^{n}
        \overbrace{\bullet x \bullet x ...}^{n}
        ...
    }^{m}
  $$

  </div>
</details>

### Multiplication of exponents

$$
x^n x^m = x^{(n + m)}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  In the simple case $n$ and $m$ are taken to be elements of $\mathbb{N}$.

  $$
  \begin{align*}
  x^n x^m
    &= 1 \overbrace{\bullet x \bullet x ...}^{n}
          \overbrace{\bullet x \bullet x ...}^{m} \\
    &= 1 \overbrace{\bullet x \bullet x ...}^{(n + m)}
  \end{align*}
  $$

  If either $n$ or $m$ are negative we can see that this still holds.

  First, assume $n \ge m$.

  $$
  \begin{align*}
  x^n x^{-m}
    &= 1 \overbrace{\bullet x \bullet x ...}^{n}
          \overbrace{\bullet x^{-1} \bullet x^{-1} ...}^{m} \\
    &= 1 \overbrace{\bullet x \bullet x ...}^{(n - m)}
          \overbrace{\bullet x(x^{-1}) \bullet x(x^-1) ...}^{m} \\
    &= 1 \overbrace{\bullet x \bullet x ...}^{(n - m)}
  \end{align*}
  $$

  Second, assume $n \lt m$.

  $$
  \begin{align*}
  x^n x^{-m}
    &= 1 \overbrace{\bullet x \bullet x ...}^{n}
          \overbrace{\bullet x^{-1} \bullet x^{-1} ...}^{m} \\
    &= 1 \overbrace{\bullet x \bullet x ...}^{(n - n)}
          \overbrace{\bullet x(x^{-1}) \bullet x(x^-1) ...}^{(m - n)} \\
    &= 1 \overbrace{\bullet x(x^{-1}) \bullet x(x^-1) ...}^{(m - n)} \\
    &= 1 \overbrace{\bullet x(x^{-1}) \bullet x(x^-1) ...}^{|(n - m)|}
  \end{align*}
  $$

  Last, assume both are negative.

  $$
  \begin{align*}
  x^{-n} x^{-m}
    &= 1 \overbrace{\bullet x^{-1} \bullet x^{-1} ...}^{n}
          \overbrace{\bullet x^{-1} \bullet x^{-1} ...}^{m} \\
    &= 1 \overbrace{\bullet x^{-1} \bullet x^{-1} ...}^{(n + m)}
  \end{align*}
  $$

  </div>
</details>

### Division of exponents

$$
\frac{x^n}{x^m} = x^{(n - m)}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{align*}
  \frac{x^n}{x^m} \\
    &= x^n (x^m)^{-1} \\
    &= x^n x^m(-1) \\
    &= x^n x^{-m} \\
    &= x^{(n - m)}
  \end{align*}
  $$

  </div>
</details>

### Exponentiation distributes over multiplication

$$
(xy)^n = x^n y^n
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{align*}
  (xy)^{n}
    &= 1 \overbrace{\bullet xy \bullet xy ...}^{n} \\
    &= 1
        \overbrace{\bullet x \bullet x ...}^{n}
        \overbrace{\bullet y \bullet y ...}^{n} \\
    &= x^n y^n
  \end{align*}
  $$

  </div>
</details>

### Exponentiation distributes over division

$$
\left( \frac{x}{y} \right)^n = \frac{x^n}{y^n}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{align*}
  \left( \frac{x}{y} \right)^n \\
    &= \left( xy^{-1} \right)^n \\
    &= x^n \left( y^{-1} \right)^n \\
    &= x^n y^{(-1)n} \\
    &= x^n \left( y^{n} \right)^{-1} \\
    &= \frac{x^n}{y^n}
  \end{align*}
  $$

  </div>
</details>

### Multiplying fractions

$$
\frac{x}{y}\left(\frac{a}{b}\right)
= \frac{xa}{yb}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{align*}
  \frac{x}{y}\left(\frac{a}{b}\right)
    &= xy^{-1}ab^{-1} \\
    &= xay^{-1}b^{-1} \\
    &= xa(yb)^{-1} \\
    &= \frac{xa}{yb}
  \end{align*}
  $$

  </div>
</details>

### Adding fractions

$$
\frac{x}{y} + \frac{a}{b}
= \frac{xb + ay}{yb}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{align*}
  \frac{x}{y} + \frac{a}{b}
    &= xy^{-1} + ab^{-1} \\
    &= xy^{-1}bb^{-1} + ab^{-1}yy^{-1} \\
    &= xby^{-1}b^{-1} + ayb^{-1}y^{-1} \\
    &= xb(yb)^{-1} + ay(by)^{-1} \\
    &= xb(yb)^{-1} + ay(yb)^{-1} \\
    &= (xb + ay)(yb)^{-1} \\
    &= \frac{xb + ay}{yb}
  \end{align*}
  $$

  </div>
</details>

#### Special case: Adding fractions with shared denominator

$$
\frac{(x + y)}{z} = \frac{x}{z} + \frac{y}{z}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  This is the distributive axiom.

  $$
  \begin{align*}
    \frac{(x + y)}{z}
      &= (x + y)z^{-1} \\
      &= xz^{-1} + yz^{-1} \\
      &= \frac{x}{z} + \frac{y}{z}
  \end{align*}
  $$

  Alternatively, using the addition of fractions above, and in the opposite direction.

  $$
  \begin{align*}
    \frac{x}{z} + \frac{y}{z}
      &= \frac{(xz + yz)}{(zz)} \\
      &= (xz + yz)(zz)^{-1} \\
      &= (xz + yz)z^{-1}z^{-1} \\
      &= (xzz^{-1} + yzz^{-1})z^-1 \\
      &= (x(1) + y(1))z^{-1} \\
      &= (x + y)z^{-1} \\
      &= \frac{(x + y)}{z}
  \end{align*}
  $$

  </div>
</details>

### Dividing fractions

$$
\frac
  {\left(\frac{x}{y}\right)}
  {\left(\frac{a}{b}\right)}
  = \frac{xb}{ya}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{align*}
    \frac
      {\left(\frac{x}{y}\right)}
      {\left(\frac{a}{b}\right)} \\
      &= \frac{\left( xy^{-1} \right) }{\left( ab^{-1} \right)} \\
      &= \left( xy^{-1} \right) \left( ab^{-1} \right)^{-1} \\
      &= \left( xy^{-1} \right) \left( a^{-1}\left( b^{-1} \right)^{-1} \right) \\
      &= \left( xy^{-1} \right) \left( a^{-1}b^{-1(-1)} \right) \\
      &= \left( xy^{-1} \right) \left( a^{-1}b^1 \right) \\
      &= \left( xy^{-1} \right) \left( a^{-1}b \right) \\
      &= xy^{-1}a^{-1}b \\
      &= xby^{-1}a^{-1} \\
      &= xb(ya)^{-1} \\
      &= \frac{xb}{ya}
  \end{align*}
  $$

  </div>
</details>

#### Special case: In combination with whole (or taken to be whole) numbers

$$
\frac
  {\left(\frac{x}{y}\right)}
  {a}
  = \frac{x}{ya}
$$

$$
\frac
  {x}
  {\left(\frac{a}{b}\right)}
  = \frac{xb}{a}
$$

## Radical notation

$$
\sqrt[n]{x} = x^{\frac{1}{n}} = x^{\left( n^{-1} \right)}
$$

The multiplicative inverse of exponentiation.

### Radicals distribute over multiplication

$$
\sqrt[n]{xy}
  = \sqrt[n]{x} \sqrt[n]{y}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  Because [exponents distribute over multiplication](#exponentiation-distributes-over-multiplication).

  $$
  \begin{align*}
  \sqrt[n]{xy}
    &= (xy)^{\frac{1}{n}} \\
    &= x^{\frac{1}{n}} y^{\frac{1}{n}} \\
    &= \sqrt[n]{x} \sqrt[n]{y}
  \end{align*}
  $$

  </div>
</details>

### Radicals distribute over division

$$
\sqrt[n]{\frac{x}{y}}
  = \frac{\sqrt[n]{x}}{\sqrt[n]{y}}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  Because [exponents distribute over division](#exponentiation-distributes-over-division).

  $$
  \begin{align*}
  \sqrt[n]{\frac{x}{y}}
    &= \left( \frac{x}{y} \right)^{\frac{1}{n}} \\
    &= \frac{x^{\frac{1}{n}}}{y^{\frac{1}{n}}} \\
    &= \frac{\sqrt[n]{x}}{\sqrt[n]{y}} \\
  \end{align*}
  $$

  </div>
</details>

### Exponent of a radical

$$
\sqrt[n]{x}^m = \sqrt[n]{x^m}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{align*}
  \sqrt[n]{x}^m
    &= \left( x^{\frac{1}{n}} \right)^m \\
    &= \left( x^{m \frac{1}{n}} \right) \\
    &= \left( x^{m} \right)^{\frac{1}{n}} \\
    &= \sqrt[n]{x^m}
  \end{align*}
  $$

  </div>
</details>

### Radical of a radical

$$
\sqrt[m]{\sqrt[n]{x}} = \sqrt[nm]{x}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{align*}
  \sqrt[m]{\sqrt[n]{x}}
    &= \left( x^{\frac{1}{n}} \right)^{\frac{1}{m}} \\
    &= x^{\frac{1}{n} \left( \frac{1}{m} \right)} \\
    &= x^{\frac{1}{nm}} \\
    &= \sqrt[nm]{x} \\
  \end{align*}
  $$

  </div>
</details>

### Multiplication of radicals with matching radicands

$$
\sqrt[n]{x} \sqrt[m]{x}
  = \sqrt[nm]{x^{(m + n)}}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{align*}
  \sqrt[n]{x} \sqrt[m]{x}
    &= x^{\frac{1}{n}} x^{\frac{1}{m}} \\
    &= x^{\left( \frac{1}{n} + \frac{1}{m} \right)} \\
    &= x^{\left( \frac{n + m}{nm} \right)} \\
    &= \sqrt[nm]{x^{(n + m)}}
  \end{align*}
  $$

  </div>
</details>

### Radical of a product of a number and a matching power

$$
\sqrt[n]{xy^n}
  = y \sqrt[n]{x}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  $$
  \begin{align*}
  \sqrt[n]{xy^n}
    &= \left( xy^n \right)^{\frac{1}{n}} \\
    &= x^{\frac{1}{n}} \left( y^n \right)^{\frac{1}{n}} \\
    &= x^{\frac{1}{n}} \left( y^\left( n\frac{1}{n} \right) \right) \\
    &= x^{\frac{1}{n}} ( y ) \\
    &= y \sqrt[n]{x}
  \end{align*}
  $$

  </div>
</details>

### Notes

> If an integer is not a perfect power of the index, then its root will be irrational.

From [LibreTexts Mathematics: 5.1: Roots and Radicals](https://math.libretexts.org/Bookshelves/Algebra/Advanced_Algebra/05%3A_Radical_Functions_and_Equations/5.01%3A_Roots_and_Radicals)
