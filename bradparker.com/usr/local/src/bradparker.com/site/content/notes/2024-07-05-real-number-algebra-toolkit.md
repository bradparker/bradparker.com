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

#### Proof

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

### Multiplication by negative one

Multiplying any element by negative one yields the additive inverse for that element.

$$
\forall x \in \mathbb{R} \hspace{3pt} x(-1) = -x
$$

#### Proof

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

### The additive inverse of an additive inverse is identity

$$
-(-x) = x
$$

#### Proof

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

### Additive inverse multiplied by additive inverse

When an additive inverse is multiplied by another additive inverse the result is doubly inverted.

<p>
  <math>
    <mrow><mo>&minus;</mo><mi>x</mi></mrow>
    <mo>(</mo>
    <mrow><mo>&minus;</mo><mi>y</mi></mrow>
    <mo>)</mo>
    <mo>=</mo>
    <mi>x</mi>
    <mi>y</mi>
  </math>
</p>

#### Proof

We can use the knowledge that [multiplying any element by negative one yields the additive inverse for that element](#multiplication-by-negative-one) to rewrite the expression.

<p>
  <math>
    <mi>x</mi><mo>(</mo><mrow><mo>&minus;</mo><mn>1</mn></mrow><mo>)</mo>
    <mo>(</mo>
    <mi>y</mi><mo>(</mo><mrow><mo>&minus;</mo><mn>1</mn></mrow><mo>)</mo>
    <mo>)</mo>
    <mo>=</mo>
    <mi>x</mi>
    <mi>y</mi>
  </math>
</p>

Which we can rewrite using the knowledge that [multiplication is associative](#multiplication-is-associative).

<p>
  <math>
    <mi>x</mi><mo>(</mo><mrow><mo>&minus;</mo><mn>1</mn></mrow><mo>)</mo>
    <mi>y</mi><mo>(</mo><mrow><mo>&minus;</mo><mn>1</mn></mrow><mo>)</mo>
    <mo>=</mo>
    <mi>x</mi>
    <mi>y</mi>
  </math>
</p>

Which we can rewrite using the knowledge that [multiplication is commutative](#multiplication-is-commutative).

<p>
  <math>
    <mi>x</mi>
    <mi>y</mi>
    <mo>(</mo><mrow><mo>&minus;</mo><mn>1</mn></mrow><mo>)</mo>
    <mo>(</mo><mrow><mo>&minus;</mo><mn>1</mn></mrow><mo>)</mo>
    <mo>=</mo>
    <mi>x</mi>
    <mi>y</mi>
  </math>
</p>

Which we can rewrite using the knowledge that [multiplication by negative one is the additive inverse](#multiplication-by-negative-one).

<p>
  <math>
    <mi>x</mi>
    <mi>y</mi>
    <mo>(</mo>
      <mrow>
        <mo>&minus;</mo>
        <mo>(</mo>
          <mrow>
            <mo>&minus;</mo>
            <mn>1</mn>
          </mrow>
        <mo>)</mo>
      </mrow>
    <mo>)</mo>
    <mo>=</mo>
    <mi>x</mi>
    <mi>y</mi>
  </math>
</p>

Which we can rewrite using the knowledge that [the additive inverse of an additive inverse is identity](#the-additive-inverse-of-an-additive-inverse-is-identity).

<p>
  <math>
    <mi>x</mi>
    <mi>y</mi>
    <mn>1</mn>
    <mo>=</mo>
    <mi>x</mi>
    <mi>y</mi>
  </math>
</p>

Which we can rewrite using the knowledge that [1 is the multiplicative identity element](#multiplicative-identity).

<p>
  <math>
    <mi>x</mi>
    <mi>y</mi>
    <mo>=</mo>
    <mi>x</mi>
    <mi>y</mi>
  </math>
</p>

### Multiplication by an additive inverse produces an additive inverse (<a id="multiplication-by-additive-inverse" href="#multiplication-by-additive-inverse">#</a>)

<p>
  <math>
    <mi>x</mi>
    <mo>(</mo>
      <mrow>
        <mo>&minus;</mo>
        <mi>y</mi>
      </mrow>
    <mo>)</mo>
    <mo>=</mo>
    <mrow>
      <mo>&minus;</mo>
      <mo>(</mo>
        <mi>x</mi>
        <mi>y</mi>
      <mo>)</mo>
    </mrow>
  </math>
</p>

#### Proof

Using the knowledge that [the additive inverse for an element is equal to multiplying that element by negative one](#multiplication-by-negative-one) we can write the following expression.

<p>
  <math>
    <mi>x</mi>
    <mo>(</mo>
      <mi>y</mi>
      <mo>(</mo>
        <mrow>
          <mo>&minus;</mo>
          <mn>1</mn>
        </mrow>
      <mo>)</mo>
    <mo>)</mo>
    <mo>=</mo>
    <mrow>
      <mo>&minus;</mo>
      <mo>(</mo>
        <mi>x</mi>
        <mi>y</mi>
      <mo>)</mo>
    </mrow>
  </math>
</p>

Which we can rewrite using the knowledge that [multiplication is associative](#multiplication-is-associative).

<p>
  <math>
    <mo>(</mo>
      <mi>x</mi>
      <mi>y</mi>
    <mo>)</mo>
    <mo>(</mo>
      <mrow>
        <mo>&minus;</mo>
        <mn>1</mn>
      </mrow>
    <mo>)</mo>
    <mo>=</mo>
    <mrow>
      <mo>&minus;</mo>
      <mo>(</mo>
        <mi>x</mi>
        <mi>y</mi>
      <mo>)</mo>
    </mrow>
  </math>
</p>

Which we can rewrite using the knowledge that [multiplication by negative one is equal to the additive inverse](#multiplication-by-negative-one).

<p>
  <math>
    <mrow>
      <mo>&minus;</mo>
      <mo>(</mo>
        <mi>x</mi>
        <mi>y</mi>
      <mo>)</mo>
    </mrow>
    <mo>=</mo>
    <mrow>
      <mo>&minus;</mo>
      <mo>(</mo>
        <mi>x</mi>
        <mi>y</mi>
      <mo>)</mo>
    </mrow>
  </math>
</p>

### The additive inverse of a product is equal to either element being an additive inverse

<p>
  <math>
    <mrow>
      <mo>&minus;</mo>
      <mo>(</mo>
        <mi>x</mi>
        <mi>y</mi>
      <mo>)</mo>
    </mrow>
    <mo>=</mo>
    <mrow>
      <mo>&minus;</mo>
      <mi>x</mi>
      <mi>y</mi>
    </mrow>
    <mo>=</mo>
    <mi>x</mi>
    <mo>(</mo>
      <mrow>
        <mo>&minus;</mo>
        <mi>y</mi>
      </mrow>
    <mo>)</mo>
  </math>
</p>

#### Proof

This has already been proved while proving that [multiplication by an additive inverse produces an additive inverse](#multiplication-by-additive-inverse).

<p>
  <math>
    <mo>(</mo>
      <mi>x</mi>
      <mi>y</mi>
    <mo>)</mo>
    <mo>(</mo>
      <mrow>
        <mo>&minus;</mo>
        <mn>1</mn>
      </mrow>
    <mo>)</mo>
    <mo>=</mo>
    <mrow>
      <mo>&minus;</mo>
      <mo>(</mo>
        <mi>x</mi>
        <mi>y</mi>
      <mo>)</mo>
    </mrow>
  </math>
</p>

The extra steps that make it clearer are the use of the knowledge that [multiplication is associative](#multiplication-is-associative) &hellip;

<p>
  <math>
    <mi>x</mi>
    <mi>y</mi>
    <mo>(</mo>
      <mrow>
        <mo>&minus;</mo>
        <mn>1</mn>
      </mrow>
    <mo>)</mo>
    <mo>=</mo>
    <mrow>
      <mo>&minus;</mo>
      <mo>(</mo>
        <mi>x</mi>
        <mi>y</mi>
      <mo>)</mo>
    </mrow>
  </math>
</p>

And [multiplication is commutative](#multiplication-is-commutative).

<p>
  <math>
    <mi>x</mi>
    <mi>y</mi>
    <mo>(</mo>
      <mrow>
        <mo>&minus;</mo>
        <mn>1</mn>
      </mrow>
    <mo>)</mo>
    <mo>=</mo>
    <mi>x</mi>
    <mo>(</mo>
      <mrow>
        <mo>&minus;</mo>
        <mn>1</mn>
      </mrow>
    <mo>)</mo>
    <mi>y</mi>
    <mo>=</mo>
    <mrow>
      <mo>&minus;</mo>
      <mo>(</mo>
        <mi>x</mi>
        <mi>y</mi>
      <mo>)</mo>
    </mrow>
  </math>
</p>

We can rewrite this expression using the knowledge that [multiplication by negative one is equal to the additive inverse](#multiplication-by-negative-one).

<p>
  <math>
    <mi>x</mi>
    <mo>(</mo>
      <mrow>
        <mo>&minus;</mo>
        <mi>y</mi>
      </mrow>
    <mo>)</mo>
    <mo>=</mo>
    <mrow>
      <mo>&minus;</mo>
      <mi>x</mi>
    </mrow>
    <mi>y</mi>
    <mo>=</mo>
    <mrow>
      <mo>&minus;</mo>
      <mo>(</mo>
        <mi>x</mi>
        <mi>y</mi>
      <mo>)</mo>
    </mrow>
  </math>
</p>

### Exponential notation

<p>
  <math>
    <mrow>
      <mi>n</mi>
      <mo>&#x2208;<!-- in --></mo>
      <mi normal>&#x2124;<!-- Z (the set of Integers) --></mi>
      <mo>.</mo>
      <mrow>
        <msup>
          <mi>x</mi>
          <mi>n</mi>
        </msup>
      </mrow>
      <mo>{</mo>
      <mrow>
        <mtable>
          <mtr>
            <mtd>
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
              </mrow>
              <mo>,</mo>
              <mi>n</mi>
              <mo>&#x2265;</mo>
              <mn>0</mn>
            </mtd>
          </mtr>
          <mtr>
            <mtd>
              <mrow>
                <mn>1</mn>
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
                  <mrow><mo>|</mo><mi>n</mi><mo>|</mo></mrow>
                </mover>
              </mrow>
              <mo>,</mo>
              <mi>n</mi>
              <mo>&lt;</mo>
              <mn>0</mn>
            </mtd>
          </mtr>
        </mtable>
      </mrow>
    </mrow>
  </math>
</p>

### Exponent of exponent

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
        <msup>
          <mi>x</mi>
          <mi>n</mi>
        </msup>
        <mo>)</mo>
      </mrow>
      <mi>m</mi>
    </msup>
    <mo>=</mo>
    <msup>
      <mi>x</mi>
      <mrow>
        <mi>n</mi>
        <mi>m</mi>
      </mrow>
    </msup>
  </math>
</p>

#### Proof

<p>
  <math>
    <msup>
      <mrow>
        <mo>(</mo>
        <msup>
          <mi>x</mi>
          <mi>n</mi>
        </msup>
        <mo>)</mo>
      </mrow>
      <mi>m</mi>
    </msup>
    <mo>=</mo>
    <mrow>
      <mrow>
        <mn>1</mn>
        <mover>
          <mover accent="true">
            <mrow>
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
                <mi>n</mi>
              </mover>
              <mo>&hellip;</mo>
            </mrow>
            <mo>&#x23DE;<!--TOP CURLY BRACKET--></mo>
          </mover>
          <mi>m</mi>
        </mover>
      </mrow>
    </mrow>
  </math>
</p>

### Multiplication of exponents

<p>
  <math>
    <msup>
      <mi>x</mi>
      <mi>n</mi>
    </msup>
    <msup>
      <mi>x</mi>
      <mi>m</mi>
    </msup>
    <mo>=</mo>
    <msup>
      <mi>x</mi>
      <mrow>
        <mo>(</mo>
        <mi>n</mi>
        <mo>+</mo>
        <mi>m</mi>
        <mo>)</mo>
      </mrow>
    </msup>
  </math>
</p>

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
\frac{x}{y}(\frac{a}{b})
= \frac{xa}{yb}
$$

#### Proof

$$
\frac{x}{y}(\frac{a}{b})
$$

$$
= xy^(-1)ab^(-1)
$$

$$
= xay^(-1)b^(-1)
$$

$$
= xa(yb)^(-1)
$$

$$
= \frac{xa}{yb}
$$

### Adding fractions

$$
\frac{x}{y} + \frac{a}{b}
= \frac{xb + ay}{yb}
$$

#### Proof

$$
\frac{x}{y} + \frac{a}{b}
$$

$$
= xy^(-1) + ab^(-1)
$$

$$
= xy^(-1)bb^(-1) + ab^(-1)yy^(-1)
$$

$$
= xby^(-1)b^(-1) + ayb^(-1)y^(-1)
$$

$$
= xb(yb)^(-1) + ay(by)^(-1)
$$

$$
= xb(yb)^(-1) + ay(yb)^(-1)
$$

$$
= (xb + ay)(yb)^(-1)
$$

$$
= \frac{xb + ay}{yb}
$$

#### Special case: Adding fractions with shared denominator

$$
\frac{(x + y)}{z} = \frac{x}{z} + \frac{y}{z}
$$

#### Proof

This is the distributive axiom.

$$
\frac{(x + y)}{z}
$$

$$
= (x + y)z^(-1)
$$

$$
= xz^(-1) + yz^(-1)
$$

$$
= \frac{x}{z} + \frac{y}{z}
$$

Alternatively, using the addition of fractions above, and in the opposite direction.

$$
\frac{x}{z} + \frac{y}{z}
$$

$$
= \frac{(xz + yz)}{(zz)}
$$

$$
= (xz + yz)(zz)^(-1)
$$

$$
= (xz + yz)z^(-1)z^(-1)
$$

$$
= (xzz^(-1) + yzz^(-1))z^-1
$$

$$
= (x(1) + y(1))z^(-1)
$$

$$
= (x + y)z^(-1)
$$

$$
= \frac{(x + y)}{z}
$$

### Dividing fractions

$$
\frac{(\frac{x}{y})}{(\frac{a}{b})} = \frac{xb}{ya}
$$

#### Proof

$$
\frac{(\frac{x}{y})}{(\frac{a}{b})}
$$

$$
= \frac{(xy^(-1))}{(ab^(-1))}
$$

$$
= (xy^(-1))(ab^(-1))^(-1)
$$

$$
= (xy^(-1))(a^(-1)(b^(-1))^(-1))
$$

$$
= (xy^(-1))(a^(-1)b^(-1 \times -1))
$$

$$
= (xy^(-1))(a^(-1)b^1)
$$

$$
= (xy^(-1))(a^(-1)b)
$$

$$
= xy^(-1)a^(-1)b
$$

$$
= xby^(-1)a^(-1)
$$

$$
= xb(ya)^(-1)
$$

$$
= \frac{xb}{ya}
$$


