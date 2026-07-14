---
title: On tetrahedra and tuition
thumbnail: /content/posts/on-tetrahedra-and-tuition/assets/images/hematite-tetrahedron.jpg
description: |
  ![A tetrahedron made out of polished balls of hematite](/content/posts/on-tetrahedra-and-tuition/assets/images/hematite-tetrahedron.webp)

  During a chemistry lecture I was told a simple little piece of information. This, of course, kicked off weeks of thinking about triangles and how people, communities and cultures learn.
tags:
  - Mathematics
---

<figure>

![A tetrahedron made out of polished balls of hematite](/content/posts/on-tetrahedra-and-tuition/assets/images/hematite-tetrahedron.webp)

<figcaption>

My son bought me these polished hematite balls. We're both hematite fans. They're quite magnetic. If you get the rotations right they're happy enough to sit in this arrangement.

</figcaption>

</figure>

At the beginning of this year I began a Diploma in Science, at the University of Queensland. For several years I've been thinking about how much I'd like to return to tertiary studies. There's so much I want to learn about the natural world. I feel unbelievably fortunate to have the opportunity to give this a go.

But, I have a lot of ground to cover. For one thing I've done no real mathematics in school or professionally. As a result I'm always looking for opportunities to apply the little I know, to really get a handle on the fundamentals, and also to try and convince myself that it's something I'm actually capable of doing.

## Pushing and pulling

I ended up beginning my return to studies with a chemistry bridging course. This was two years of high school chemistry squished into 6 months. I can't recall the time frames exactly, but I feel like we moved very quickly from "this is an atom" to "here are the basics of Valence Shell Electron Pair Repulsion theory." As I understand it the very basic idea is:
* There are regions in the space around an atom where the probability of finding an electron is higher than elsewhere
* These regions of high electron probability are attracted to the centre of the atom, while they repel each other
* The outermost electron containing regions of an atom are particularly important, because they _do things_ with electron containing regions from other atoms

For the atoms I'll be concerning myself with here there are four regions in this outermost set. They're all attracted with equal force to the centre of the atom. At the same time they repel each other with equal force. So these four regions end up being held at an equal distance from the centre and push each other apart at an equal distance. Drawing equal lines from the centre of the atom, to the centre of each region, and equal lines between the centres of each region, can get you this image.

<div
  id="scene-wireframe"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
</div>

## A simple question

This combination of attractive and repulsive forces ends up producing a four sided solid shape, a regular tetrahedron. Tetrahedral geometry appears to be a pretty big deal in chemistry, or at least the parts I've been exposed to so far.

Here, look at this angle:

<div
  id="scene-wireframe-and-angle"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
</div>

This angle helps to explain why many molecules are the shapes they are, and the shapes of many molecules help to explain why they behave in the ways they do, and the behavior of many molecules helps to explain an awful lot of phenomena across many areas of study. So yeah, I reckon it might be a pretty big deal.

During a lecture introducing us to molecular geometry we were told that this angle is approximately equal to 109.5&deg;, which had me immediately thinking "I wonder if I can figure out what it _really_ is?"

## A not so simple answer

Here's how I figured it out, more or less. Actually, a lot less, but I'll come to that later. First I assumed that all the edges were equal to 1. Why not?

That means all the sides, such as this one ($\triangle{ABC}$), are triangles with heights (e.g. $\overbar{AD}$) equal to $\frac{\sqrt{3}}{2}$.

<div
  id="scene-wireframe-and-side-with-height"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
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
  class="rounded-lg border overflow-hidden relative">
</div>

Tracing lines ($\overbar{AF}$ and $\overbar{EG}$) at right angles from the equal sides ($\overbar{DA}$ and $\overbar{DE}$) of this triangle to their opposing vertices reveals the angle I'm after.

<div
  id="scene-lineAF-lineEG"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
</div>

To find it, I'll first find this one, $\angle DEA$.

<div
  id="scene-angleDEA"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
</div>

And to do that I can trace a line ($\overbar{DH}$) at a right angle from line $\overbar{AE}$ to point $D$.

<div
  id="scene-lineDH"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
</div>

With that I can find angle $\angle DEA$.

$$
\begin{align*}
\cos \left( \angle DEA \right) &= \frac{\left( \overbar{EH} \right)}{\left( \overbar{DE} \right)} &\implies \\
\angle DEA &= \text{cos}^{-1} \frac{\left( \overbar{EH} \right)}{\left( \overbar{DE} \right)} &\implies \\
\angle DEA &= \text{cos}^{-1} \frac{\left( \frac{1}{2} \right)}{\left( \frac{\sqrt{3}}{2} \right)} &\implies \\
\angle DEA &= \text{cos}^{-1} \frac{\sqrt{3}}{3}
\end{align*}
$$

From here I know that angles $\angle FAE$ and $\angle HDE$ are equal because the right triangles $\triangle FAE$ and $\triangle HDE$ share angle $\angle DEA$.

<div
  id="scene-angleFAE-angleHDE"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
</div>

Which then tells me that angle $\angle AOH$ is equal to angle $\angle DEA$. Because the triangles $\triangle HDE$ and $\triangle AOH$ already have two equal angles, so the remaining angles must be equal.

<div
  id="scene-angleAOH"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
</div>

This means The Angle, the one I've been looking for, is $2 \angle DEA$, or $2 \cdot \text{cos}^{-1}\frac{\sqrt{3}}{3}$. Which in degrees is about 109.47122063449069&deg;. So there you go.

## A much simpler answer

If you're familiar with this problem, or you're just much cleverer than me, you likely read the above with some powerful bemusement, knowing that there is a _much_ simpler way to solve it.

After figuring out my own method, as described above, I went and looked for other methods. What I found was something very tidy, based on a key insight that it's possible to inscribe a regular tetrahedron within a cube.

<div
  id="scene-cubeInscribedWireframe"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
</div>

Tracing lines from the centre of this cube to any two points of the tetrahedron inscribed within it reveals the angle I'm looking for.

<div
  id="scene-cubeInscribedWireframe-angleAOC"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
</div>

To find it this time I can trace a line ($\overbar{OI}$) at right angles to edge $\overbar{AC}$ to the centre.

<div
  id="scene-cubeInscribedWireframe-lineOI"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
</div>

Which makes it fairly straightforward, certainly comparatively, to find the angle $\angle AOI$.

<div
  id="scene-cubeInscribedWireframe-angleAOI"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
</div>

$$
\begin{align*}
\tan \left( \angle AOI \right) &= \frac{\left( \overbar{AI} \right)}{\left( \overbar{OI} \right)} &\implies \\
\angle AOI &= \text{tan}^{-1} \left( \frac{\left( \overbar{AI} \right)}{\left( \overbar{OI} \right)} \right)
\end{align*}
$$

To find usable values for $\overbar{AI}$ and $\overbar{OI}$ I can just say the sides of the cube in which the tetrahedron is inscribed are equal to 1. Again, why not?

$$
\begin{align*}
\overbar{OI} &= \frac{1}{2} \\
\overbar{AI} &= \frac{\sqrt{2}}{2}
\end{align*}
$$

This allows me to find a concrete value for angle $\angle AOI$.

$$
\begin{align*}
\angle AOI &= \text{tan}^{-1} \left( \frac{\left( \overbar{AI} \right)}{\left( \overbar{OI} \right)} \right) &\implies \\
\angle AOI &= \text{tan}^{-1} \frac{\left( \frac{\sqrt{2}}{2} \right)}{\left( \frac{1}{2} \right)} &\implies \\
\angle AOI &= \text{tan}^{-1} \sqrt{2}
\end{align*}
$$

And lo, $2 \cdot \text{tan}^{-1} \sqrt{2}$ is equal to roughly 109.47122063449069&deg;. How about that?

## Getting lost or exploring the territory

Now that I've seen it, if I were ever asked to explain why that angle is roughly 109.5&deg; the cube-inscribed method is how I'd go about it. Of course I would, it's so clear! But that doesn't mean that I think finding my own less elegant method was a waste of time.

My method as it's described above is missing the absolute mass of confused scratchings it took to get to it. I began by asking myself _what's everything I know, and what's everything can I derive about this shape?_ Along the way I exercised my limited understanding of trigonometry, and went off down rabbit holes such as _Wait, how do I know that [all the angles of a triangle always sum to the same value](/notes/trigonometry-angle-sum-theorem)?_ and _Hang on, [why does $a^2 = b^2 + c^2$](https://www.euclids-elements.org/elements/bookI/propI47.html)?_

All that stumbling around is incredibly valuable to me. It helped me build a scaffold in which to place this insight about molecular geometry, regular tetrahedra and cubes. And now that it's in there, there's every likelihood that if I'm presented with other related questions, and trying to recall or derive everything I can about some structure, it'll surface again.

I believe this because if I think of times where I've successfully learned something about a subject, e.g. music or programming, this is how I've done it. Additionally, this approach of adding surrounding terrain to the things I want to retain seems related to the [levels of processing model](https://en.wikipedia.org/wiki/Levels_of_processing_model) of memory recall, which I read about in [_Memory_](https://mitpress.mit.edu/9780262545204/memory/) by Fergus Craik and Larry Jacoby. So that's encouraging.

## Finding the trail

Another reason I have to value wandering off down circuitous byways is that it ... BIT ABOUT CUMULATIVE CULTURE GOES HERE ...

So, uh, thanks everyone! Keep up the cool and interesting work of progressing humanity's collective knowledge. I really appreciate it.

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
  src="/content/posts/on-tetrahedra-and-tuition/assets/javascript/main.js">
</script>
