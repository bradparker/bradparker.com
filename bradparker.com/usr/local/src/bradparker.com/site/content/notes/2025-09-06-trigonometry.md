---
title: Trigonometry
tags:
  - Mathematics
description: |
  Round and round
---

<svg viewBox="-150 -150 300 300" id="unit-circle-1">
  <circle fill="none" stroke="black" stroke-width="1" r="100" cx="0" cy="0" />
  <line x1="0" y1="-50%" x2="0" y2="50%" stroke="black" stroke-width="1" stroke-dasharray="1, 1" stroke-dashoffset="0.5" />
  <line x1="-50%" y1="0" x2="50%" y2="0" stroke="black" stroke-width="1" stroke-dasharray="1,1" stroke-dashoffset="0.5" />
  <line
    data-target="tangent"
    stroke="black"
    stroke-width="1"
    x1="0"
    y1="0"
    x2="50"
    y2="-87"
  />
  <line
    data-target="cosine"
    stroke="black"
    stroke-width="1"
    x1="0"
    y1="0"
    x2="50"
    y2="0"
  />
  <line
    data-target="sine"
    stroke="black"
    stroke-width="1"
    x1="50"
    y1="0"
    x2="50"
    y2="-87"
  />
</svg>

<label class="founders-grotesk block w-full">
  &#952;
  <input
    class="p-2 rounded-lg ba b--gray w-full"
    name="theta"
    type="number"
    step="0.017453293"
    value="1.047197551"
  />
</label>

<div class="founders-grotesk flex gap-3">
  <label>
    cos &#952;
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="cosine_theta"
      value="0.5"
      type="number"
      step="0.01"
    />
  </label>
  <label>
    sin &#952;
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="sine_theta"
      value="0.866025404"
      type="number"
      step="0.01"
    />
  </label>
</div>

<script>
  const round = (n, p = 1000000000) =>
    Math.round(n * p) / p

  const point = (theta) =>
    [
      round(Math.cos(theta)),
      round(Math.sin(theta)),
    ];

  const main = () => {
    const svg = document.getElementById("unit-circle-1");
    const tangentLine = svg.querySelector("[data-target=tangent]");
    const cosineLine = svg.querySelector("[data-target=cosine]");
    const sineLine = svg.querySelector("[data-target=sine]");

    const thetaInput = document.querySelector("[name=theta]");
    const cosineInput = document.querySelector("[name=cosine_theta]");
    const sineInput = document.querySelector("[name=sine_theta]");

    const update = (theta) => {
      const [x, y] = point(theta);

      cosineInput.value = x;
      sineInput.value = y;

      tangentLine.setAttribute("x2", x * 100);
      tangentLine.setAttribute("y2", y * -100);

      cosineLine.setAttribute("x2", x * 100);

      sineLine.setAttribute("x1", x * 100);
      sineLine.setAttribute("x2", x * 100);
      sineLine.setAttribute("y2", y * -100);
    }

    const initialTheta = parseFloat(thetaInput.value);

    update(initialTheta);

    thetaInput.addEventListener("change", () => {
      const theta = parseFloat(thetaInput.value);

      update(theta);
    });

    cosineInput.addEventListener("change", () => {
      const cosine = parseFloat(cosineInput.value);
      const theta = round(Math.acos(cosine));

      thetaInput.value = theta;

      update(theta);
    });

    sineInput.addEventListener("change", () => {
      const sine = parseFloat(sineInput.value);
      const theta = round(Math.asin(sine));

      thetaInput.value = theta;

      update(theta);
    });
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", main);
  } else {
    main();
  }
</script>
