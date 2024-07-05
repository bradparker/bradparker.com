---
title: The danger of using models as metaphors
tags:
  - books
  - development
description: |
  ![A tangram that's not quite put together righ](/assets/images/tangram-small.webp)

  Of the many fascinating insights to be found in Elinor Ostrom's book _Governing the commons_ there are a few that have really stuck with me. My personally sticky insights aren't really related to the core argument, it's all well out of my wheel house after all. My ears most pricked up when Ostrom wrote about the social responsibilities of scientists and the social impact of social research.

---
  ![A tangram that's not quite put together righ](/assets/images/tangram-small.webp)

I read Elinor Ostrom's book _Governing the commons_ on a whim after seeing it mentioned in a discussion about open source software communities. I believe this isn't an uncommon way for software folk to become aware of this book. Of the many fascinating insights Ostrom presents there are a few that have really stuck with me. My personally sticky insights aren't really related to the core argument, it's all well out of my wheel house after all. My ears most pricked up when Ostrom wrote about the social responsibilities of scientists and the social impact of social research.

Towards the start of the book in a subsection of the first chapter entitled _The metaphorical use of models_ Ostrom writes.

> When models are used as metaphors, an author usually points to the similarity between one or two variables in a natural setting and one or two variables in a model. If calling attention to similarities is all that is intended by the metaphor, it serves the usual purpose of rapidly conveying information in graphic form. These three models have frequently been used metaphorically, however, for another purpose. The similarity between the many individuals jointly using a resource in a natural setting and the many individuals jointly producing a suboptimal result in the model has been used to convey a sense that further similarities are present.

Later she provides an example to illustrate how bad things can get when models become metaphors and are then used to create policy.

> Relying on metaphors as the foundation for policy advice can lead to results substantially different from those presumed to be likely. Nationalizing the ownership of forests in Third World countries, for example, has been advocated on the grounds that local villagers cannot manage forests as to sustain their productivity and their value in reducing soil erosion. In countries where small villages had owned and regulated their local communal forests for generations, nationalization meant expropriation.

The three models being referred to in the first quotation are prior attempts to explain the dynamics of common resource governance, but we needn't go into any more detail than that. What I seem to have taken away from this is a broader point about the danger of attempting to fit insights onto a domain over which they were never intended to apply. It's possible I'm zooming out too far, for example one of the model-cum-metaphors Ostrom is referring to above is the [prisoner's dilemma](https://en.wikipedia.org/wiki/Prisoner's_dilemma) which has a precise definition, but this starts to feel close to an impulse in my field. We software developers love a structuring metaphor, such as [architecture](https://en.wikipedia.org/wiki/Pattern_language#Application_domains) or [cellular biology](#footnote-1).

How do we know when we've inadvertently turned a model into a metaphor and misapplied it? When we've latched onto those couple of similar variables and missed all the dissimilar ones? Ostrom's suggestion is fairly simple to state: if your data contradicts the model you're trying to apply, either your data is wrong, the model is wrong, or the model doesn't fit your domain. This could perhaps be summarised, flippantly, as "do science." Indeed her argument against the inevitability of [the tragedy of the commons](https://en.wikipedia.org/wiki/Tragedy_of_the_commons) is that there are ample examples of self-organizing communities who share common resources without falling prey to the effect.

I'm not exactly sure where all this leads me. Wanting to apply the scientific method to find the boundaries of the myriad structuring metaphors we use to think and communicate sure sounds exhausting; for all involved. But on the other hand we run the risk of accidentally limiting ourselves, or causing real harm, by labouring under a seemingly well justified but ill-fitting model. I genuinely have no idea what I'm supposed to do with these thoughts, but at least now I've shared my unease.

## Notes

<ol>
  <li id="footnote-1" class="markdown">
    <blockquote>
      <p>And then there is the idea of simulation itself, where the whole idea of “data” and “state” starts to get eclipsed by “competent objects” that can cooperate, much more like biological cells and human societies can.</p>
    </blockquote>
    <p>
      <cite>
        From Alan Kay answering the question <a href="https://www.quora.com/What-thought-process-would-lead-one-to-invent-object-oriented-programming/answer/Alan-Kay-11">'What thought process would lead one to invent object-oriented programming?' on Quora</a>
      </cite>
    </p>
  </li>
</ol>
