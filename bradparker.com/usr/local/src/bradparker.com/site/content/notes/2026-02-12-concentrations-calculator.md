---
title: Concentrations calculator
description: |
  Calculating concentrations
tags:
  - Maffs
---

<div class="founders-grotesk flex gap-3">
  <label>
    <em>c<sub>a</sub></em> mol/L
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="c_a"
      min="0"
      step="any"
    />
  </label>
  <label>
    <em>v<sub>a</sub></em> L
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="v_a"
      min="0"
      step="any"
    />
  </label>
  <div>
    &plus;
  </div>
  <label>
    <em>c<sub>b</sub></em> mol/L
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="c_b"
      min="0"
      step="any"
    />
  </label>
  <label>
    <em>v<sub>b</sub></em> L
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="v_b"
      min="0"
      step="any"
    />
  </label>
  <div>
    =
  </div>
  <label>
    <em>c</em> mol/L
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="c"
      min="0"
      step="any"
    />
  </label>
  <label>
    <em>v</em> L
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="v"
      min="0"
      step="any"
    />
  </label>
</div>

Where $v = v_a + v_b$.

<button id="reset">
  Reset
</button>

The units are irrelevant, they're just there as grounding examples.

<script type="module" async src="/content/notes/concentrations-calculator/assets/main.js"></script>
