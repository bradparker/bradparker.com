---
title: Concentrations calculator
description: |
  Calculating concentrations
tags:
  - Maffs
---

This is wildly hard to use once filled in. Best to empty everything and start again. It's like using a poorly implemented spread sheet where every cell is simultaneously a value and a formula.

Given that $v = v_a + v_b$...

<div class="founders-grotesk flex gap-3">

  <label>
    <em>c<sub>a</sub></em>
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="cv[c_a]"
      min="0"
      step="any"
    />
  </label>
  <label>
    <em>v<sub>a</sub></em>
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="cv[v_a]"
      min="0"
      step="any"
    />
  </label>
  <div>
    &plus;
  </div>
  <label>
    <em>c<sub>b</sub></em>
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="cv[c_b]"
      min="0"
      step="any"
    />
  </label>
  <label>
    <em>v<sub>b</sub></em>
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="cv[v_b]"
      min="0"
      step="any"
    />
  </label>
  <div>
    =
  </div>
  <label>
    <em>c</em>
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="cv[c]"
      min="0"
      step="any"
    />
  </label>
  <label>
    <em>v</em>
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="cv[v]"
      min="0"
      step="any"
    />
  </label>
</div>

<script type="module" async src="/content/notes/concentrations-calculator/assets/main.js"></script>
