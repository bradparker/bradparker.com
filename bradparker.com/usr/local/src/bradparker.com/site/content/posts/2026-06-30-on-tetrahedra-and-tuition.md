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

I ended up beginning my return to studies with a chemistry bridging course. This was two years of high school chemistry squished into 6 months. I can't recall the time frames exactly, but I feel like we moved very quickly from "this is an atom" to "here are the basics of Valence Shell Electron Pair Repulsion theory." As I understand it the very basics of the idea are:

* There are regions in the space around an atom where the probability of finding an electron is higher than elsewhere
* These regions of high electron probability are attracted to the centre of the atom, while at the same time they repel each other
* The outermost electron containing regions of an atom are particularly important, because they _do things_ with electron containing regions from other atoms

For the atoms I'll be concerning myself with here there are four regions in this outermost set. They're all attracted with equal force to the centre of the atom. At the same time they repel each other with equal force. So these four regions end up being held at an equal distance from the centre and push each other apart at an equal distance. Drawing equal lines from the centre of the atom, to the centre of each region, and equal lines between the centres of each region, can get you this image.

<div
  id="scene-wireframe"
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-wireframe-943x673.png">
</div>

## A simple question

This combination of attractive and repulsive forces ends up producing a four sided solid shape, a regular tetrahedron. Tetrahedral geometry appears to be a pretty big deal in chemistry, or at least the parts I've been exposed to so far.

Here, look at this angle:

<div
  id="scene-wireframe-and-angle"
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-wireframe-and-angle-943x673.png">
</div>

This angle helps to explain why many molecules are the shapes they are, and the shapes of many molecules help to explain why they behave in the ways they do, and the behavior of many molecules helps to explain an awful lot of phenomena across many areas of study. So yeah, I reckon it might be a pretty big deal.

During a lecture introducing us to molecular geometry we were told that this angle is approximately equal to 109.5&deg;, which had me immediately thinking "I wonder if I can figure out what it _really_ is?"

## A not so simple answer

I'm going to describe here how I figured it out, more or less. Actually, a lot less, but I'll come to that later. This is by no means an ideal solution, it's just the one I came to. Bear with me.

First, I assumed that all the edges were equal to 1. Why not? That means all the sides, such as this one ($\triangle{ABC}$), are triangles with heights (e.g. $\overbar{AD}$) equal to $\frac{\sqrt{3}}{2}$.

<div
  id="scene-wireframe-and-side-with-height"
  style="height: 20rem;"
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-wireframe-and-side-with-height-943x673.png">
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
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-wireframe-and-slice-943x673.png">
</div>

Tracing lines ($\overbar{AF}$ and $\overbar{EG}$) at right angles from the equal sides ($\overbar{DA}$ and $\overbar{DE}$) of this triangle to their opposing vertices reveals the angle I'm after.

<div
  id="scene-lineAF-lineEG"
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-lineAF-lineEG-943x673.png">
</div>

To find it, I'll first find this one, $\angle DEA$.

<div
  id="scene-angleDEA"
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-angleDEA-943x673.png">
</div>

And to do that I can trace a line ($\overbar{DH}$) at a right angle from line $\overbar{AE}$ to point $D$. (I could've also used the law of cosines, but I'll make use of this line $\overbar{DH}$ again.)

<div
  id="scene-lineDH"
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-lineDH-943x673.png">
</div>

With that I can find angle $\angle DEA$.

$$
\begin{align*}
\cos \left( \angle DEA \right) &= \frac{\left( \overbar{EH} \right)}{\left( \overbar{DE} \right)} \implies \\
\angle DEA
  &= \text{cos}^{-1} \frac{\left( \overbar{EH} \right)}{\left( \overbar{DE} \right)} \\
  &= \text{cos}^{-1} \frac{\left( \frac{1}{2} \right)}{\left( \frac{\sqrt{3}}{2} \right)} \\
  &= \text{cos}^{-1} \frac{\sqrt{3}}{3}
\end{align*}
$$

From here I know that angles $\angle FAE$ and $\angle HDE$ are equal because the right triangles $\triangle FAE$ and $\triangle HDE$ share angle $\angle DEA$.

<div
  id="scene-angleFAE-angleHDE"
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-angleFAE-angleHDE-943x673.png">
</div>

Which then tells me that angle $\angle AOH$ is equal to angle $\angle DEA$. Because the triangles $\triangle HDE$ and $\triangle AOH$ already have two equal angles, so the remaining angles must be equal.

<div
  id="scene-angleAOH"
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-angleAOH-943x673.png">
</div>

This means The Angle, the one I've been looking for, is $2 \angle DEA$, or $2 \cdot \text{cos}^{-1}\frac{\sqrt{3}}{3}$. Which in degrees is about 109.47122063449069&deg;. So there you go.

## A much simpler answer

If you're familiar with this problem, or you're very clever, you likely read the above with some powerful bemusement, knowing that there is a _much_ simpler way to solve it.

After figuring out my own method, as described above, I went and looked for other methods. What I found was something very tidy, based on the key insight that it's possible to inscribe a regular tetrahedron within a cube.

<div
  id="scene-cubeInscribedWireframe"
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-cubeInscribedWireframe-943x673.png">
</div>

Tracing lines from the centre of this cube to any two points of the tetrahedron inscribed within it reveals the angle I'm looking for.

<div
  id="scene-cubeInscribedWireframe-angleAOC"
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-cubeInscribedWireframe-angleAOC-943x673.png">
</div>

To find it this time I can trace a line ($\overbar{OI}$) at right angles to edge $\overbar{AC}$ to the centre.

<div
  id="scene-cubeInscribedWireframe-lineOI"
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-cubeInscribedWireframe-lineOI-943x673.png">
</div>

Which makes it fairly straightforward, certainly comparatively, to find the angle $\angle AOI$.

<div
  id="scene-cubeInscribedWireframe-angleAOI"
  class="rounded-lg border overflow-hidden relative">
  <img src="/content/posts/on-tetrahedra-and-tuition/assets/images/screencapture-scene-cubeInscribedWireframe-angleAOI-943x673.png">
</div>

$$
\begin{align*}
\tan \left( \angle AOI \right) &= \frac{\left( \overbar{AI} \right)}{\left( \overbar{OI} \right)} \implies \\
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
\angle AOI
  &= \text{tan}^{-1} \left( \frac{\left( \overbar{AI} \right)}{\left( \overbar{OI} \right)} \right) \\
  &= \text{tan}^{-1} \frac{\left( \frac{\sqrt{2}}{2} \right)}{\left( \frac{1}{2} \right)} \\
  &= \text{tan}^{-1} \sqrt{2}
\end{align*}
$$

And lo, $2 \cdot \text{tan}^{-1} \sqrt{2}$ is equal to roughly 109.47122063449069&deg;. How about that?

## Getting lost or exploring the territory

Now that I've seen it, if I were ever asked to explain why that angle is roughly 109.5&deg; the cube-inscribed method is how I'd go about it. Of course I would, it's so clear! But that doesn't mean that I think finding my own less elegant method was a waste of time.

My method as it's described above is missing the absolute mass of confused scratchings it took to get to it. I began by asking myself _what's everything I know, and what's everything can I derive about this shape?_ Along the way I exercised my limited understanding of trigonometry, and went off down rabbit holes such as _Wait, how do I know that [all the angles of a triangle always sum to the same value](/notes/trigonometry-angle-sum-theorem)?_ and _Hang on, [why does 'a' squared equal 'b' squared plus 'c' squared](https://www.euclids-elements.org/elements/bookI/propI47.html)?_

All that stumbling around is incredibly valuable to me. It helped me build a scaffold in which to place this insight about molecular geometry, regular tetrahedra and cubes. And now that it's in there, there's every likelihood that if I'm presented with other related questions, and trying to recall or derive everything I can about some structure, it'll surface again.

I believe this because if I think of times where I've successfully learned something about a subject, e.g. music or programming, this is how I've done it. Additionally, this approach of adding surrounding terrain to the things I want to retain seems related to the [levels of processing model](https://en.wikipedia.org/wiki/Levels_of_processing_model) of memory recall, an introduction to which I read in [_Memory_](https://mitpress.mit.edu/9780262545204/memory/) by Fergus Craik and Larry Jacoby. So that's encouraging.

## Finding the trail

The steps I took during my swing at a solution, and my being able to understand the clearly articulated cube-inscribed solution, are owed to the accumulation and selection of good ideas, made by many communities of inspired people, over many generations.

I know it's a bit of a trite example, but I can't help it, I get a kick out of this. Over 2,600 years ago [some Grecians](https://en.wikipedia.org/wiki/Pythagoreanism) bonded over an extreme enthusiasm for triangles. Some 300 years later their ideas, along with more from other communities, were elucidated and collected into [books](https://en.wikipedia.org/wiki/Euclid%27s_Elements) you can still read today. 157 years ago Emanuele Paternò suggested to his colleagues that they suppose "that the four atomic valences of \[carbon\] point toward the four vertices of a regular tetrahedron."[^1] 87 years ago Ryutarō Tsuchida proposes [a connection between valence electrons and molecular geometry](https://en.wikipedia.org/wiki/VSEPR_theory#History). A few months ago me and 400 others sat in a lecture theatre in Queensland, Australia and learned about valence electrons, so that we might better understand molecular geometry, so that we might better understand molecular polarity, so that we might better understand why oil and water don't mix.

So, uh, thanks everyone! Keep up the cool and interesting work of [accumulating our culture](https://oecs.mit.edu/pub/6bvu7f8m/release/1). I really appreciate it.

[^1]: [150 Years of the Tetrahedral Carbon: A Toast to Chirality](https://www.sciencedirect.com/org/science/article/pii/S0899004225000071)

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
