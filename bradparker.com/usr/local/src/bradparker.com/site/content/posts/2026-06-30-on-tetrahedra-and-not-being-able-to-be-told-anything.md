---
title: On tetrahedra and not being able to be told anything
description: |
  Description...
tags:
  - Mathematics
---

![A tetrahedron made out of polished balls of hematite](/content/posts/on-tetrahedra-and-not-being-able-to-be-told-anything/assets/images/hematite-tetrahedron.webp)

At the beginning of this year I began a Diploma in Science, at the University of Queensland. For several years I've been thinking about how much I'd like to return to tertiary studies. There's so much I want to learn about the natural world. I feel unbelievably fortunate to have the opportunity to give this a go.

But, I have a lot of ground to cover. For one thing I've done no real mathematics in school or professionally. As a result I'm always looking for opportunities to apply the little I know, to really get a handle on the fundamentals, and also to try and convince myself that it's something I'm actually capable of doing.

## Five repelling and attracting points

I ended up beginning my return to studies with a chemistry bridging course. This was two years of high school chemistry squished into 6 months. I can't recall the time frames exactly, but I feel like we moved very quickly from "this is an atom" to "here are the basics of Valence Shell Electron Pair Repulsion theory." As I understand it the very basic idea is:
* There are regions in the space around an atom where the probability of finding an electron is higher than elsewhere
* These regions of high electron probability are attracted to the centre of the atom, while they repel each other
* The outermost electron containing regions of an atom are particularly important, because they _do things_ with electron containing regions from other atoms

For the atoms I'll be concerning myself with here there are four regions in this outermost set. They're all attracted with equal force to the centre of the atom. At the same time these outermost regions repel each other with equal force. So these four regions end up being held at the same distance from the centre and push each other apart at an equal distance. Drawing equal lines from the centre of the atom, to centre of each region, and equal lines between the centres of each region, gets you this image.

<div
  id="scene-wireframe"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden">
</div>

## A simple question

This combination of attractive and competing repulsive forces ends up producing a four sided solid shape, a tetrahedron. Tetrahedral geometry appears to be a pretty big deal in chemistry, or at least the parts I've been exposed to so far.

Here, look at this angle:

<div
  id="scene-wireframe-and-angle"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden">
</div>

This angle helps to explain why many molecules are the shapes they are, and the shapes of many molecules help to explain why they behave the way they do, and the behavior of molecules helps to explain an awful lot of phenomena across many areas of study. Like I said: a pretty big deal.

During a lecture introducing us to molecular geometry we were told that this angle is approximately equal to 109.5&deg;, which had me immediately thinking "I wonder if I can figure out what it _really_ is?"

## A _fairly_ simple answer

Here's how I figured it out, more or less. Actually, a lot less, but I'll come to that later. First I assumed that all the edges were equal to 1. Why not?

That makes this triangle ($\triangle{ABC}$) equilateral.

<div
  id="scene-wireframe-and-side"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden">
</div>

With a height ($\overbar{AD}$) equal to $\frac{\sqrt{3}}{2}$.

<div
  id="scene-wireframe-and-side-with-height"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden">
</div>

Because...

$$
\begin{align*}
\overbar{AB}^2 &= \overbar{AD}^2 + \overbar{BD}^2 \implies \\
\overbar{AD}^2 &= \overbar{AB}^2 - \overbar{BD}^2 \implies \\
\overbar{AD}
  &= \sqrt{\overbar{AB}^2 - \overbar{AC}^2} \\
  &= \sqrt{1 - \left(\frac{1}{2}\right)^2} \\
  &= \sqrt{1 - \frac{1}{4}} \\
  &= \sqrt{\frac{3}{4}} \\
  &= \frac{\sqrt{3}}{2}
\end{align*}
$$

Tracing the same line along another side ($\triangle EBC$) produces an isosceles triangle ($\triangle ADE$) which slices through the middle of the shape.

<div
  id="scene-wireframe-and-slice"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden">
</div>

Adding in lotsa lines...

<div
  id="scene-wireframe-and-slice-with-height"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden">
</div>

<script type="importmap">
  {
    "imports": {
      "three": "/assets/javascript/three.module.js",
      "three/addons/": "/assets/javascript/three.js/"
    }
  }
</script>

<script
  async
  type="module"
  src="/content/posts/on-tetrahedra-and-not-being-able-to-be-told-anything/assets/javascript/main.js">
</script>
