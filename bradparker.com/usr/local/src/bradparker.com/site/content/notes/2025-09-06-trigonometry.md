---
title: Trigonometry
tags:
  - Mathematics
description: |
  Round and round
---

<script id="utils">
  const round = (n, p = 1000000000) =>
    Math.round(n * p) / p

  const onReady = (main) => {
    if (document.readyState === "loading") {
      document.addEventListener("DOMContentLoaded", main);
    } else {
      main();
    }
  }
</script>

## Periodic identities

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
    data-target="-theta"
    stroke="coral"
    stroke-width="1"
    x1="0"
    y1="0"
    x2="50"
    y2="87"
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
  <line
    data-target="pi-theta"
    stroke="seagreen"
    stroke-width="1"
    x1="0"
    y1="0"
    x2="-50"
    y2="-87"
  />
</svg>

<label class="founders-grotesk block w-full">
  &theta;
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
    cos &theta;
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="cosine_theta"
      value="0.5"
      type="number"
      step="0.01"
    />
  </label>
  <label>
    sin &theta;
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="sine_theta"
      value="0.866025404"
      type="number"
      step="0.01"
    />
  </label>
</div>

<script type="module">
  const point = (theta) =>
    [
      round(Math.cos(theta)),
      round(Math.sin(theta)),
    ];

  onReady(() => {
    const svg = document.getElementById("unit-circle-1");
    const tangentLine = svg.querySelector("[data-target=tangent]");
    const cosineLine = svg.querySelector("[data-target=cosine]");
    const sineLine = svg.querySelector("[data-target=sine]");
    const lineNegTheta = svg.querySelector("[data-target=-theta]");
    const linePiMinusTheta = svg.querySelector("[data-target=pi-theta]");

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

      lineNegTheta.setAttribute("x2", x * 100);
      lineNegTheta.setAttribute("y2", y * 100);

      linePiMinusTheta.setAttribute("x2", x * -100);
      linePiMinusTheta.setAttribute("y2", y * -100);
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
  });
</script>

## Sum identities

<svg viewBox="-150 -150 300 300" id="angle-sum-identities">
  <circle fill="none" stroke="black" stroke-width="1" r="100" cx="0" cy="0"></circle>
  <line x1="0" y1="-50%" x2="0" y2="50%" stroke="black" stroke-width="1" stroke-dasharray="1, 1" stroke-dashoffset="0.5"></line>
  <line x1="-50%" y1="0" x2="50%" y2="0" stroke="black" stroke-width="1" stroke-dasharray="1,1" stroke-dashoffset="0.5"></line>
  <line data-target="AB" stroke="black" stroke-width="1" stroke-linecap="round" x1="0" y1="0" x2="74.99999969092616" y2="0"></line>
  <line data-target="AC" stroke="black" stroke-width="1" stroke-linecap="round" x1="0" y1="0" x2="74.99999969092616" y2="-43.301270446410165"></line>
  <line data-target="AD" stroke="black" stroke-width="1" stroke-linecap="round" x1="0" y1="0" x2="49.9999992" y2="-86.6025408"></line>
  <line data-target="AF" stroke="black" stroke-width="1" stroke-linecap="round" x1="0" y1="0" x2="49.9999992" y2="0"></line>
  <line data-target="BC" stroke="black" stroke-width="1" stroke-linecap="round" x1="74.99999969092616" y1="0" x2="74.99999969092616" y2="-43.301270446410165"></line>
  <line data-target="CD" stroke="black" stroke-width="1" stroke-linecap="round" x1="74.99999969092616" y1="-43.301270446410165" x2="49.9999992" y2="-86.6025408"></line>
  <line data-target="CE" stroke="black" stroke-width="1" stroke-linecap="round" x1="74.99999969092616" y1="-43.301270446410165" x2="49.999999290926155" y2="-43.301270446410165"></line>
  <line data-target="DE" stroke="black" stroke-width="1" stroke-linecap="round" x1="49.9999992" y1="-86.6025408" x2="49.9999992" y2="-43.30127035358984"></line>
  <line data-target="DF" stroke="black" stroke-width="1" stroke-linecap="round" x1="49.9999992" y1="0" x2="49.9999992" y2="-86.6025408"></line>
</svg>

<div class="founders-grotesk flex gap-3">
  <div class="space-y-6">
    <label>
      &theta;
      <input
        class="p-2 rounded-lg ba b--gray w-full"
        name="angle-sum-identities[theta]"
        type="number"
        step="0.017453293"
        value="0.52359878"
      />
    </label>
    <label>
      cos(&theta;)
      <input
        class="p-2 rounded-lg ba b--gray w-full bg-light-gray"
        name="angle-sum-identities[cos_theta]"
        value="0.866025402"
        readonly="readonly"
      />
    </label>
    <label>
      sin(&theta;)
      <input
        class="p-2 rounded-lg ba b--gray w-full bg-light-gray"
        name="angle-sum-identities[sin_theta]"
        value="0.5"
        readonly="readonly"
      />
    </label>
  </div>

  <div class="space-y-6">
    <label>
      &gamma;
      <input
        class="p-2 rounded-lg ba b--gray w-full"
        name="angle-sum-identities[gamma]"
        type="number"
        step="0.017453293"
        value="0.52359878"
      />
    </label>
    <label>
      cos(&gamma;)
      <input
        class="p-2 rounded-lg ba b--gray w-full bg-light-gray"
        name="angle-sum-identities[cos_gamma]"
        value="0.866025402"
        readonly="readonly"
      />
    </label>
    <label>
      sin(&gamma;)
      <input
        class="p-2 rounded-lg ba b--gray w-full bg-light-gray"
        name="angle-sum-identities[sin_gamma]"
        value="0.5"
        readonly="readonly"
      />
    </label>
  </div>
</div>

<div class="founders-grotesk flex gap-3">
  <label>
    cos(&theta; + &gamma;) = cos(&theta;)cos(&gamma;) - sin(&theta;)sin(&gamma;)
    <input
      class="p-2 rounded-lg ba b--gray w-full bg-light-gray"
      name="angle-sum-identities[cos_sum]"
      value="0.5"
      readonly="readonly"
    />
  </label>
</div>

<div class="founders-grotesk flex gap-3">
  <label>
    sin(&theta; + &gamma;) = sin(&theta;)cos(&gamma;) + cos(&theta;)sin(&gamma;)
    <input
      class="p-2 rounded-lg ba b--gray w-full bg-light-gray"
      name="angle-sum-identities[sin_sum]"
      value="0.866025408"
      readonly="readonly"
    />
  </label>
</div>

<script type="module">
  onReady(() => {
    const svg = document.getElementById("angle-sum-identities");
    const abLine = svg.querySelector("[data-target=AB]");
    const acLine = svg.querySelector("[data-target=AC]");
    const adLine = svg.querySelector("[data-target=AD]");
    const afLine = svg.querySelector("[data-target=AF]");
    const bcLine = svg.querySelector("[data-target=BC]");
    const cdLine = svg.querySelector("[data-target=CD]");
    const ceLine = svg.querySelector("[data-target=CE]");
    const deLine = svg.querySelector("[data-target=DE]");
    const dfLine = svg.querySelector("[data-target=DF]");

    const thetaInput = document.querySelector("[name='angle-sum-identities[theta]']");
    const cosThetaInput = document.querySelector("[name='angle-sum-identities[cos_theta]']");
    const sinThetaInput = document.querySelector("[name='angle-sum-identities[sin_theta]']");

    const gammaInput = document.querySelector("[name='angle-sum-identities[gamma]']");
    const cosGammaInput = document.querySelector("[name='angle-sum-identities[cos_gamma]']");
    const sinGammaInput = document.querySelector("[name='angle-sum-identities[sin_gamma]']");

    const cosSumInput = document.querySelector("[name='angle-sum-identities[cos_sum]']");
    const sinSumInput = document.querySelector("[name='angle-sum-identities[sin_sum]']");

    const update = (theta, gamma) => {
      const sum = theta + gamma;
      const cos_sum = round(Math.cos(sum));
      const sin_sum = round(Math.sin(sum));

      cosSumInput.value = cos_sum;
      sinSumInput.value = sin_sum;

      const cos_theta = round(Math.cos(theta));
      const cos_gamma = round(Math.cos(gamma));
      const sin_theta = round(Math.sin(theta));
      const sin_gamma = round(Math.sin(gamma));

      cosThetaInput.value = cos_theta;
      cosGammaInput.value = cos_gamma;
      sinThetaInput.value = sin_theta;
      sinGammaInput.value = sin_gamma;

      const ab = cos_theta * cos_gamma;
      abLine.setAttribute("x2", ab * 100);
      abLine.setAttribute("y2", 0);

      const af = cos_sum;
      afLine.setAttribute("x2", af * 100);
      afLine.setAttribute("y2", 0);

      const bc = cos_theta * sin_gamma;
      bcLine.setAttribute("x1", ab * 100);
      bcLine.setAttribute("y1", 0);
      bcLine.setAttribute("x2", ab * 100);
      bcLine.setAttribute("y2", bc * -100);

      acLine.setAttribute("x2", ab * 100);
      acLine.setAttribute("y2", bc * -100);

      adLine.setAttribute("x2", cos_sum * 100);
      adLine.setAttribute("y2", sin_sum * -100);

      dfLine.setAttribute("x1", cos_sum * 100);
      dfLine.setAttribute("y1", 0);
      dfLine.setAttribute("x2", cos_sum * 100);
      dfLine.setAttribute("y2", sin_sum * -100);

      cdLine.setAttribute("x1", ab * 100);
      cdLine.setAttribute("y1", bc * -100);
      cdLine.setAttribute("x2", cos_sum * 100);
      cdLine.setAttribute("y2", sin_sum * -100);

      const ce = sin_theta * sin_gamma;
      ceLine.setAttribute("x1", ab * 100);
      ceLine.setAttribute("y1", bc * -100);
      ceLine.setAttribute("x2", (ab - ce) * 100);
      ceLine.setAttribute("y2", bc * -100);

      const de = sin_theta * cos_gamma;
      deLine.setAttribute("x1", cos_sum * 100);
      deLine.setAttribute("y1", sin_sum * -100);
      deLine.setAttribute("x2", af * 100);
      deLine.setAttribute("y2", (sin_sum - de) * -100);
    }

    const initialTheta = parseFloat(thetaInput.value);
    const initialGamma = parseFloat(gammaInput.value);

    update(initialTheta, initialGamma);

    thetaInput.addEventListener("change", () => {
      const theta = parseFloat(thetaInput.value);
      const gamma = parseFloat(gammaInput.value);

      update(theta, gamma);
    });
    gammaInput.addEventListener("change", () => {
      const theta = parseFloat(thetaInput.value);
      const gamma = parseFloat(gammaInput.value);

      update(theta, gamma);
    });
  });
</script>
