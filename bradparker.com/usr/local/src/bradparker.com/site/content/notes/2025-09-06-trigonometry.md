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
  const point = (theta) =>
    [
      Math.cos(theta),
      Math.sin(theta),
    ];

  const update = (
    { tangent, cosine, sine },
    { cosineInput, sineInput },
    theta
  ) => {
    const [x, y] = point(theta);

    cosineInput.value = x;
    sineInput.value = y;

    tangent.setAttribute("x2", x * 100);
    tangent.setAttribute("y2", y * -100);

    cosine.setAttribute("x2", x * 100);

    sine.setAttribute("x1", x * 100);
    sine.setAttribute("x2", x * 100);
    sine.setAttribute("y2", y * -100);
  }

  const main = () => {
    const svg = document.getElementById("unit-circle-1");
    const tangent = svg.querySelector("[data-target=tangent]");
    const cosine = svg.querySelector("[data-target=cosine]");
    const sine = svg.querySelector("[data-target=sine]");

    const thetaInput = document.querySelector("[name=theta]");
    const cosineInput = document.querySelector("[name=cosine_theta]");
    const sineInput = document.querySelector("[name=sine_theta]");

    const initialTheta = parseFloat(thetaInput.value);

    update(
      { tangent, cosine, sine },
      { cosineInput, sineInput },
      initialTheta
    );

    thetaInput.addEventListener("change", () => {
      const theta = parseFloat(thetaInput.value);
      update(
        { tangent, cosine, sine },
        { cosineInput, sineInput },
        theta
      );
    });
  };

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", main);
  } else {
    main();
  }
</script>
