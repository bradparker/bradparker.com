---
title: Real number algebra toolkit
tags:
  - mathematics
description: |
  Toolkit for manipulating algebraic expressions dealing with Real numbers.
---

The Real numbers can be defined as a set, &#x211D;, along with two operators, + (addition) and &bullet; (multiplication).

<p>
  <math>
    <mrow>
      <mo>(</mo>
      <mi normal>&#x211D;</mi>,
      <mo>+</mo>,
      <mo>&bullet;</mo>
      <mo>)</mo>
    </mrow>
  </math>
</p>

Multiplication can alternatively be written omitting the operator (&bullet;) and its surrounding space.

<p>
  <math>
    <mrow>
      <mi>x</mi>
      <mo>&bullet;</mo>
      <mi>y</mi>
      <mo>=</mo>
      <mi>x</mi>
      <mi>y</mi>
    </mrow>
  </math>
</p>

## Axioms

The following 9 axioms complete the description.

### Addition is associative

<p>
  <math>
    <mrow>
      <mi>x</mi>,&nbsp;
      <mi>y</mi>,&nbsp;
      <mi>z</mi>
      <mo>&#x2208;</mo>
      <mi normal>&#x211D;</mi>.&nbsp;
      <mi>x</mi>
      <mo>+</mo>
      <mo>(</mo>
      <mi>y</mi>
      <mo>+</mo>
      <mi>z</mi>
      <mo>)</mo>
      <mo>=</mo>
      <mo>(</mo>
      <mi>x</mi>
      <mo>+</mo>
      <mi>y</mi>
      <mo>)</mo>
      <mo>+</mo>
      <mi>z</mi>
    </mrow>
  </math>
</p>


### Addition is commutative

<p>
  <math>
    <mrow>
      <mi>x</mi>,&nbsp;
      <mi>y</mi>
      <mo>&#x2208;</mo>
      <mi normal>&#x211D;</mi>.&nbsp;
      <mi>x</mi>
      <mo>+</mo>
      <mi>y</mi>
      <mo>=</mo>
      <mi>y</mi>
      <mo>+</mo>
      <mi>x</mi>
    </mrow>
  </math>
</p>



### Additive identity (<a id="additive-identity" href="#additive-identity">#</a>)

There is an additive identity element.

<p>
  <math>
    <mrow>
      <mo>&#x2203;</mo>
      <mn>0</mn>
      <mo>&#x2208;</mo>
      <mi normal>&#x211D;</mi>
      <mo>&#x220B;</mo>
      <mo>&#x2200;</mo>
      <mi>x</mi>
      <mo>&#x2208;</mo>
      <mi normal>&#x211D;</mi>.&nbsp;
      <mn>0</mn>
      <mo>+</mo>
      <mi>x</mi>
      <mo>=</mo>
      <mi>x</mi>
    </mrow>
  </math>
</p>

### Additive inverse

There is an additive inverse for every element.

<p>
  <math>
    <mrow>
      <mo>&#x2200;</mo>
      <mi>x</mi>
      <mo>&#x2208;</mo>
      <mi normal>&#x211D;</mi>&nbsp;
      <mo>&#x2203;</mo>
      <mo>(</mo>
      <mi>-x</mi>
      <mo>)</mo>
      <mo>&#x220B;</mo>
      <mi>x</mi>
      <mo>+</mo>
      <mo>(</mo>
      <mi>-x</mi>
      <mo>)</mo>
      <mo>=</mo>
      <mn>0</mn>
    </mrow>
  </math>
</p>

### Multiplication is associative

<p>
  <math>
    <mrow>
      <mi>x</mi>,&nbsp;
      <mi>y</mi>,&nbsp;
      <mi>z</mi>
      <mo>&#x2208;</mo>
      <mi normal>&#x211D;</mi>.&nbsp;
      <mi>x</mi>
      <mo>(</mo>
      <mi>y</mi>
      <mi>z</mi>
      <mo>)</mo>
      <mo>=</mo>
      <mo>(</mo>
      <mi>x</mi>
      <mi>y</mi>
      <mo>)</mo>
      <mi>z</mi>
    </mrow>
  </math>
</p>

### Multiplication is commutative

<p>
  <math>
    <mrow>
      <mi>x</mi>,&nbsp;
      <mi>y</mi>
      <mo>&#x2208;</mo>
      <mi normal>&#x211D;</mi>.&nbsp;
      <mi>x</mi>
      <mi>y</mi>
      <mo>=</mo>
      <mi>y</mi>
      <mi>z</mi>
    </mrow>
  </math>
</p>

### Multiplicative identity

<p>
  <math>
    <mrow>
      <mo>&#x2203;</mo>
      <mn>1</mn>
      <mo>&#x2208;</mo>
      <mi normal>&#x211D;</mi>
      <mo>&#x220B;</mo>
      <mo>&#x2200;</mo>
      <mi>x</mi>
      <mo>&#x2208;</mo>
      <mi normal>&#x211D;</mi>.&nbsp;
      <mn>1</mn>
      <mi>x</mi>
      <mo>=</mo>
      <mi>x</mi>
    </mrow>
  </math>
</p>

### Multiplicative inverse

For every element, except 0, there is a multiplicative inverse.

<p>
  <math>
    <mrow>
      <mo>&#x2200;</mo>
      <mi>x</mi>
      <mo>&#x2208;</mo>
      <mo>(</mo>
      <mi normal>&#x211D;</mi>
      <mo>-</mo>
      <mo>{</mo>
      <mn>0</mn>
      <mo>}</mo>
      <mo>)</mo>&nbsp;
      <mo>&#x2203;</mo>
      <mo>(</mo>
      <msup>
        <mi>x</mi>
        <mn>-1</mn>
      </msup>
      <mo>)</mo>
      <mo>&#x220B;</mo>
      <mi>x</mi>
      <mo>(</mo>
      <msup>
        <mi>x</mi>
        <mn>-1</mn>
      </msup>
      <mo>)</mo>
      <mo>=</mo>
      <mn>1</mn>
    </mrow>
  </math>
</p>

### Multiplication distributes over addition (<a id="multiplication-distributes-over-addition" href="#multiplication-distributes-over-addition">#</a>)

<p>
  <math>
    <mrow>
      <mi>x</mi>,&nbsp;
      <mi>y</mi>,&nbsp;
      <mi>z</mi>
      <mo>&#x2208;</mo>
      <mi normal>&#x211D;</mi>.&nbsp;
      <mi>x</mi>
      <mo>(</mo>
      <mi>y</mi>
      <mo>+</mo>
      <mi>z</mi>
      <mo>)</mo>
      <mo>=</mo>
      <mi>x</mi>
      <mi>y</mi>
      <mo>+</mo>
      <mi>x</mi>
      <mi>z</mi>
    </mrow>
  </math>
</p>

## Toolkit

### Multiplying by zero

Multiplying any element by zero equals zero.

<p>
  <math>
    <mrow>
      <mi>x</mi><mn>0</mn><mo>=</mo><mn>0</mn>
    </mrow>
  </math>
</p>

#### Proof

<p>
  <math>
    <mrow>
      <mn>0</mn><mo>=</mo><mi>x</mi><mn>0</mn>
    </mrow>
  </math>
</p>

As per the [the additive identity axiom](#additive-identity) <math><mrow><mn>0</mn><mo>=</mo><mn>0</mn><mo>+</mo><mn>0</mn></math>.

<p>
  <math>
    <mrow>
      <mn>0</mn><mo>=</mo><mi>x</mi><mo>(</mo><mn>0</mn><mo>+</mo><mn>0</mn><mo>)</mo>
    </mrow>
  </math>
</p>

[Multiplication distributes over addition](#multiplication-distributes-over-addition).

<p>
  <math>
    <mrow>
      <mn>0</mn><mo>=</mo><mi>x</mi><mn>0</mn><mo>+</mo><mi>x</mi><mn>0</mn>
    </mrow>
  </math>
</p>

This matches [the additive identity axiom](#additive-identity) where <math><mrow><mi>x</mi><mn>0</mn></mrow></math> is 0. Therefore <math><mrow><mi>x</mi><mn>0</mn><mo>=</mo><mn>0</mn></mrow></math>.
