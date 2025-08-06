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
−(xy) = −xy = x(−y)
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
  x(−y) = −xy = −(xy)
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

#### Proof

In the simple case <math><mi>n</mi></math> and <math><mi>m</mi></math> are taken to be elements of <math><mi normal>&#x2115;</mi></math>.

<p>
  <math>
    <mrow>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>n</mi>
        </msup>
        <msup>
          <mi>x</mi>
          <mi>m</mi>
        </msup>
      </mrow>
      <mo>=</mo>
      <mrow>
        <mrow>
          <mn>1</mn>
          <mover>
            <mover accent="true">
              <mrow>
                <mo>&bullet;</mo>
                <mi>x</mi>
                <mo>&bullet;</mo>
                <mi>x</mi>
                <mo>&hellip;</mo>
              </mrow>
              <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
            </mover>
            <mi>n</mi>
          </mover>
          <mover>
            <mover accent="true">
              <mrow>
                <mo>&bullet;</mo>
                <mi>x</mi>
                <mo>&bullet;</mo>
                <mi>x</mi>
                <mo>&hellip;</mo>
              </mrow>
              <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
            </mover>
            <mi>m</mi>
          </mover>
        </mrow>
      </mrow>
      <mo>=</mo>
      <mrow>
        <mrow>
          <mn>1</mn>
          <mover>
            <mover accent="true">
              <mrow>
                <mo>&bullet;</mo>
                <mi>x</mi>
                <mo>&bullet;</mo>
                <mi>x</mi>
                <mo>&hellip;</mo>
              </mrow>
              <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
            </mover>
            <mrow>
              <mo>(</mo>
                <mi>n</mi>
                <mo>+</mo>
                <mi>m</mi>
              <mo>)</mo>
            </mrow>
          </mover>
        </mrow>
      </mrow>
    </mrow>
  </math>
</p>

If either <math><mi>n</mi></math> or <math><mi>m</mi></math> are negative we can see that this still holds.

<p>
  <math>
    <mrow>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>n</mi>
        </msup>
        <msup>
          <mi>x</mi>
          <mrow>
            <mo>(</mo>
              <mrow>
                <mo>&minus;</mo>
                <mi>m</mi>
              </mrow>
            <mo>)</mo>
          </mrow>
        </msup>
      </mrow>
      <mo>=</mo>
      <mrow>
        <mrow>
          <mn>1</mn>
          <mover>
            <mover accent="true">
              <mrow>
                <mo>&bullet;</mo>
                <mi>x</mi>
                <mo>&bullet;</mo>
                <mi>x</mi>
                <mo>&hellip;</mo>
              </mrow>
              <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
            </mover>
            <mi>n</mi>
          </mover>
          <mover>
            <mover accent="true">
              <mrow>
                <mo>&bullet;</mo>
                <mo>(</mo><msup><mi>x</mi><mrow><mo>&minus;</mo><mn>1</mn></mrow></msup><mo>)</mo>
                <mo>&bullet;</mo>
                <mo>(</mo><msup><mi>x</mi><mrow><mo>&minus;</mo><mn>1</mn></mrow></msup><mo>)</mo>
                <mo>&hellip;</mo>
              </mrow>
              <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
            </mover>
            <mi>m</mi>
          </mover>
        </mrow>
      </mrow>
    </mrow>
  </math>
</p>

<p>
  <math>
    <mrow>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>n</mi>
        </msup>
        <msup>
          <mi>x</mi>
          <mrow>
            <mo>(</mo>
              <mrow>
                <mo>&minus;</mo>
                <mi>m</mi>
              </mrow>
            <mo>)</mo>
          </mrow>
        </msup>
      </mrow>
      <mo>=</mo>
      <mrow>
        <mrow>
          <mn>1</mn>
          <mover>
            <mover accent="true">
              <mrow>
                <mo>&bullet;</mo>
                <mi>x</mi>
                <mo>&bullet;</mo>
                <mi>x</mi>
                <mo>&hellip;</mo>
              </mrow>
              <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
            </mover>
            <mrow>
              <mo>(</mo>
              <mi>n</mi>
              <mo>&minus;</mo>
              <mi>m</mi>
              <mo>)</mo>
            </mrow>
          </mover>
          <mover>
            <mover accent="true">
              <mrow>
                <mo>&bullet;</mo>
                <mrow>
                  <mi>x</mi>
                  <mo>(</mo><msup><mi>x</mi><mrow><mo>&minus;</mo><mn>1</mn></mrow></msup><mo>)</mo>
                </mrow>
                <mo>&bullet;</mo>
                <mrow>
                  <mi>x</mi>
                  <mo>(</mo><msup><mi>x</mi><mrow><mo>&minus;</mo><mn>1</mn></mrow></msup><mo>)</mo>
                </mrow>
                <mo>&hellip;</mo>
              </mrow>
              <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
            </mover>
            <mi>m</mi>
          </mover>
        </mrow>
      </mrow>
    </mrow>
  </math>
</p>

<p>
  <math>
    <mrow>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>n</mi>
        </msup>
        <msup>
          <mi>x</mi>
          <mrow>
            <mo>(</mo>
              <mrow>
                <mo>&minus;</mo>
                <mi>m</mi>
              </mrow>
            <mo>)</mo>
          </mrow>
        </msup>
      </mrow>
      <mo>=</mo>
      <mrow>
        <mrow>
          <mn>1</mn>
          <mover>
            <mover accent="true">
              <mrow>
                <mo>&bullet;</mo>
                <mi>x</mi>
                <mo>&bullet;</mo>
                <mi>x</mi>
                <mo>&hellip;</mo>
              </mrow>
              <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
            </mover>
            <mrow>
              <mo>(</mo>
                <mi>n</mi>
                <mo>&minus;</mo>
                <mi>m</mi>
              <mo>)</mo>
            </mrow>
          </mover>
        </mrow>
      </mrow>
    </mrow>
  </math>
</p>

### Division of exponents

<p>
  <math>
    <mfrac>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>n</mi>
        </msup>
      </mrow>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>m</mi>
        </msup>
      </mrow>
    </mfrac>
    <mo>=</mo>
    <msup>
      <mi>x</mi>
      <mrow>
        <mo>(</mo>
          <mi>n</mi>
          <mo>&minus;</mo>
          <mi>m</mi>
        <mo>)</mo>
      </mrow>
    </msup>
  </math>
</p>

#### Proof

<p>
  <math>
    <mfrac>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>n</mi>
        </msup>
      </mrow>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>m</mi>
        </msup>
      </mrow>
    </mfrac>
    <mo>=</mo>
    <mrow>
      <msup>
        <mi>x</mi>
        <mi>n</mi>
      </msup>
    </mrow>
    <msup>
      <mrow>
        <mo>(</mo>
          <mrow>
            <msup>
              <mi>x</mi>
              <mi>m</mi>
            </msup>
          </mrow>
        <mo>)</mo>
      </mrow>
      <mrow><mo>&minus;</mo><mn>1</mn></mrow>
    </msup>
  </math>
</p>

<p>
  <math>
    <mfrac>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>n</mi>
        </msup>
      </mrow>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>m</mi>
        </msup>
      </mrow>
    </mfrac>
    <mo>=</mo>
    <mrow>
      <msup>
        <mi>x</mi>
        <mi>n</mi>
      </msup>
    </mrow>
    <mrow>
      <msup>
        <mi>x</mi>
        <mrow>
          <mi>m</mi>
          <mo>(</mo>
            <mrow>
              <mo>&minus;</mo><mn>1</mn>
            </mrow>
          <mo>)</mo>
        </mrow>
      </msup>
    </mrow>
  </math>
</p>

<p>
  <math>
    <mfrac>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>n</mi>
        </msup>
      </mrow>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>m</mi>
        </msup>
      </mrow>
    </mfrac>
    <mo>=</mo>
    <mrow>
      <msup>
        <mi>x</mi>
        <mi>n</mi>
      </msup>
    </mrow>
    <mrow>
      <msup>
        <mi>x</mi>
        <mrow>
          <mo>(</mo>
            <mrow>
              <mo>&minus;</mo><mi>m</mi>
            </mrow>
          <mo>)</mo>
        </mrow>
      </msup>
    </mrow>
  </math>
</p>

<p>
  <math>
    <mfrac>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>n</mi>
        </msup>
      </mrow>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>m</mi>
        </msup>
      </mrow>
    </mfrac>
    <mo>=</mo>
    <msup>
      <mi>x</mi>
      <mrow>
        <mo>(</mo>
          <mi>n</mi>
          <mo>&minus;</mo>
          <mi>m</mi>
        <mo>)</mo>
      </mrow>
    </msup>
  </math>
</p>

### Exponentiation distributes over multiplication

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
          <mi>x</mi>
          <mi>y</mi>
        <mo>)</mo>
      </mrow>
      <mi>n</mi>
    </msup>
    <mo>=</mo>
    <msup>
      <mi>x</mi>
      <mi>n</mi>
    </msup>
    <msup>
      <mi>y</mi>
      <mi>n</mi>
    </msup>
  </math>
</p>

#### Proof

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
          <mi>x</mi>
          <mi>y</mi>
        <mo>)</mo>
      </mrow>
      <mi>n</mi>
    </msup>
    <mo>=</mo>
    <mrow>
      <mn>1</mn>
      <mover>
        <mover accent="true">
          <mrow>
            <mo>&bullet;</mo>
            <mrow>
              <mo>(</mo>
                <mi>x</mi>
                <mi>y</mi>
              <mo>)</mo>
            </mrow>
            <mo>&bullet;</mo>
            <mrow>
              <mo>(</mo>
                <mi>x</mi>
                <mi>y</mi>
              <mo>)</mo>
            </mrow>
            <mo>&hellip;</mo>
          </mrow>
          <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
        </mover>
        <mi>n</mi>
      </mover>
    </mrow>
  </math>
</p>

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
          <mi>x</mi>
          <mi>y</mi>
        <mo>)</mo>
      </mrow>
      <mi>n</mi>
    </msup>
    <mo>=</mo>
    <mrow>
      <mn>1</mn>
      <mover>
        <mover accent="true">
          <mrow>
            <mo>&bullet;</mo>
            <mi>x</mi>
            <mo>&bullet;</mo>
            <mi>x</mi>
            <mo>&hellip;</mo>
          </mrow>
          <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
        </mover>
        <mi>n</mi>
      </mover>
      <mover>
        <mover accent="true">
          <mrow>
            <mo>&bullet;</mo>
            <mi>y</mi>
            <mo>&bullet;</mo>
            <mi>y</mi>
            <mo>&hellip;</mo>
          </mrow>
          <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
        </mover>
        <mi>n</mi>
      </mover>
    </mrow>
  </math>
</p>

### Exponentiation distributes over division

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
          <mfrac>
            <mi>x</mi>
            <mi>y</mi>
          </mfrac>
        <mo>)</mo>
      </mrow>
      <mi>n</mi>
    </msup>
    <mo>=</mo>
    <mfrac>
      <msup>
        <mi>x</mi>
        <mi>n</mi>
      </msup>
      <msup>
        <mi>y</mi>
        <mi>n</mi>
      </msup>
    </mfrac>
  </math>
</p>

#### Proof

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
          <mfrac>
            <mi>x</mi>
            <mi>y</mi>
          </mfrac>
        <mo>)</mo>
      </mrow>
      <mi>n</mi>
    </msup>
    <mo>=</mo>
    <mrow>
      <mn>1</mn>
      <mover>
        <mover accent="true">
          <mrow>
            <mo>&bullet;</mo>
            <mrow>
              <mi>x</mi>
              <mo>(</mo>
                <msup>
                  <mi>y</mi>
                  <mrow><mo>&minus;</mo><mn>1</mn></mrow>
                </msup>
              <mo>)</mo>
            </mrow>
            <mo>&bullet;</mo>
            <mrow>
              <mi>x</mi>
              <mo>(</mo>
                <msup>
                  <mi>y</mi>
                  <mrow><mo>&minus;</mo><mn>1</mn></mrow>
                </msup>
              <mo>)</mo>
            </mrow>
            <mo>&hellip;</mo>
          </mrow>
          <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
        </mover>
        <mi>n</mi>
      </mover>
    </mrow>
  </math>
</p>

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
          <mfrac>
            <mi>x</mi>
            <mi>y</mi>
          </mfrac>
        <mo>)</mo>
      </mrow>
      <mi>n</mi>
    </msup>
    <mo>=</mo>
    <mrow>
      <mn>1</mn>
      <mover>
        <mover accent="true">
          <mrow>
            <mo>&bullet;</mo>
            <mi>x</mi>
            <mo>&bullet;</mo>
            <mi>x</mi>
            <mo>&hellip;</mo>
          </mrow>
          <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
        </mover>
        <mi>n</mi>
      </mover>
      <mover>
        <mover accent="true">
          <mrow>
            <mo>&bullet;</mo>
            <mrow>
              <mo>(</mo>
                <msup>
                  <mi>y</mi>
                  <mrow><mo>&minus;</mo><mn>1</mn></mrow>
                </msup>
              <mo>)</mo>
            </mrow>
            <mo>&bullet;</mo>
            <mrow>
              <mo>(</mo>
                <msup>
                  <mi>y</mi>
                  <mrow><mo>&minus;</mo><mn>1</mn></mrow>
                </msup>
              <mo>)</mo>
            </mrow>
            <mo>&hellip;</mo>
          </mrow>
          <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
        </mover>
        <mi>n</mi>
      </mover>
    </mrow>
  </math>
</p>

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
          <mfrac>
            <mi>x</mi>
            <mi>y</mi>
          </mfrac>
        <mo>)</mo>
      </mrow>
      <mi>n</mi>
    </msup>
    <mo>=</mo>
    <msup>
      <mi>x</mi>
      <mi>n</mi>
    </msup>
    <msup>
      <mrow>
        <mo>(</mo>
          <msup>
            <mi>y</mi>
            <mrow><mo>&minus;</mo><mn>1</mn></mrow>
          </msup>
        <mo>)</mo>
      </mrow>
      <mi>n</mi>
    </msup>
  </math>
</p>

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
          <mfrac>
            <mi>x</mi>
            <mi>y</mi>
          </mfrac>
        <mo>)</mo>
      </mrow>
      <mi>n</mi>
    </msup>
    <mo>=</mo>
    <msup>
      <mi>x</mi>
      <mi>n</mi>
    </msup>
    <msup>
      <mi>y</mi>
      <mrow>
        <mo>(</mo>
          <mrow><mo>&minus;</mo><mn>1</mn></mrow>
        <mo>)</mo>
        <mi>n</mi>
      </mrow>
    </msup>
  </math>
</p>

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
          <mfrac>
            <mi>x</mi>
            <mi>y</mi>
          </mfrac>
        <mo>)</mo>
      </mrow>
      <mi>n</mi>
    </msup>
    <mo>=</mo>
    <msup>
      <mi>x</mi>
      <mi>n</mi>
    </msup>
    <msup>
      <mi>y</mi>
      <mrow>
        <mi>n</mi>
        <mo>(</mo>
          <mrow><mo>&minus;</mo><mn>1</mn></mrow>
        <mo>)</mo>
      </mrow>
    </msup>
  </math>
</p>

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
          <mfrac>
            <mi>x</mi>
            <mi>y</mi>
          </mfrac>
        <mo>)</mo>
      </mrow>
      <mi>n</mi>
    </msup>
    <mo>=</mo>
    <msup>
      <mi>x</mi>
      <mi>n</mi>
    </msup>
    <msup>
      <mrow>
        <mo>(</mo>
          <msup>
            <mi>y</mi>
            <mi>n</mi>
          </msup>
        <mo>)</mo>
      </mrow>
      <mrow><mo>&minus;</mo><mn>1</mn></mrow>
    </msup>
  </math>
</p>

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
          <mfrac>
            <mi>x</mi>
            <mi>y</mi>
          </mfrac>
        <mo>)</mo>
      </mrow>
      <mi>n</mi>
    </msup>
    <mo>=</mo>
    <mfrac>
      <msup>
        <mi>x</mi>
        <mi>n</mi>
      </msup>
      <msup>
        <mi>y</mi>
        <mi>n</mi>
      </msup>
    </mfrac>
  </math>
</p>

### Multiplying fractions

$$
\frac{x}{y}\left(\frac{a}{b}\right)
= \frac{xa}{yb}
$$

#### Proof

$$
\begin{align*}
\frac{x}{y}\left(\frac{a}{b}\right)
  &= xy^{-1}ab^{-1} \\
  &= xay^{-1}b^{-1} \\
  &= xa(yb)^{-1} \\
  &= \frac{xa}{yb}
\end{align*}
$$

### Adding fractions

$$
\frac{x}{y} + \frac{a}{b}
= \frac{xb + ay}{yb}
$$

#### Proof

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

#### Special case: Adding fractions with shared denominator

$$
\frac{(x + y)}{z} = \frac{x}{z} + \frac{y}{z}
$$

#### Proof

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

### Dividing fractions

$$
\frac
  {\left(\frac{x}{y}\right)}
  {\left(\frac{a}{b}\right)}
  = \frac{xb}{ya}
$$

#### Proof

$$
\begin{align*}
  \frac
    {\left(\frac{x}{y}\right)}
    {\left(\frac{a}{b}\right)} \\
    &= \frac{(xy^{-1})}{(ab^{-1})} \\
    &= (xy^{-1})(ab^{-1})^{-1} \\
    &= (xy^{-1})(a^{-1}(b^{-1})^{-1}) \\
    &= (xy^{-1})(a^{-1}b^(-1 \times -1)) \\
    &= (xy^{-1})(a^{-1}b^1) \\
    &= (xy^{-1})(a^{-1}b) \\
    &= xy^{-1}a^{-1}b \\
    &= xby^{-1}a^{-1} \\
    &= xb(ya)^{-1} \\
    &= \frac{xb}{ya}
\end{align*}
$$

## Radical notation

$$
\sqrt[n]{x^m} = x^{\frac{m}{n}}
$$

Usually.

$$
\sqrt[n]{x} = x^{\frac{1}{n}}
$$
