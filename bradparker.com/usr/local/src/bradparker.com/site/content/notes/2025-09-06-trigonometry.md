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

<svg viewBox="-150 -150 300 300" id="periodic-identities-unit-circle">
  <circle fill="none" stroke="black" stroke-width="1" r="100" cx="0" cy="0" />
  <line
    x1="0" y1="-50%"
    x2="0" y2="50%"
    stroke="black"
    stroke-width="1"
    stroke-dasharray="1,1"
    stroke-dashoffset="0.5"
  />
  <line
    x1="-50%" y1="0"
    x2="50%" y2="0"
    stroke="black"
    stroke-width="1"
    stroke-dasharray="1,1"
    stroke-dashoffset="0.5"
  />
  <line
    data-target="theta"
    stroke="black"
    stroke-width="1"
    x1="0"
    y1="0"
    x2="50"
    y2="-87"
  />
  <line
    data-target="cosine"
    stroke="coral"
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
    data-target="-theta_cosine"
    stroke="coral"
    stroke-width="1"
    x1="0"
    y1="87"
    x2="50"
    y2="87"
  />
  <line
    data-target="sine"
    stroke="seagreen"
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
  <line
    data-target="pi-theta_sine"
    stroke="seagreen"
    stroke-width="1"
    x1="-50"
    y1="0"
    x2="-50"
    y2="-87"
  />
</svg>

<canvas
  id="periodic-identities-graph"
  width="200"
  height="60"
  class="w-full">
</canvas>

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
    &minus;&theta;
    <input
      class="p-2 rounded-lg ba b--gray w-full bg-light-gray"
      name="-theta"
      value="-1.047197551"
      readonly="readonly"
    />
  </label>
  <label>
    &pi;&minus;&theta;
    <input
      class="p-2 rounded-lg ba b--gray w-full bg-light-gray"
      name="pi-theta"
      value="2.094395103"
      readonly="readonly"
    />
  </label>
</div>

<div class="founders-grotesk flex gap-3">
  <label>
    cos &theta; / cos &minus;&theta;
    <input
      class="p-2 rounded-lg ba b--gray w-full"
      name="cosine_theta"
      value="0.5"
      type="number"
      step="0.01"
    />
  </label>
  <label>
    sin &theta; / sin &pi;&minus;&theta;
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

  const maintainResolution = (canvas)  => {
    const dpr = window.devicePixelRatio || 1;
    const rect = canvas.getBoundingClientRect();
    canvas.width = rect.width * dpr;
    canvas.height = rect.height * dpr;
  };

  const drawWave = (f) => (ctx, {
    start,
    end,
    scale,
    x: xOffset,
    y: yOffset,
    lineWidth,
    strokeStyle,
  }) => {
    ctx.lineWidth = lineWidth;
    ctx.strokeStyle = strokeStyle;
    ctx.beginPath();
    ctx.moveTo(xOffset, yOffset);
    for (let x = start; x < end; x++) {
      ctx.lineTo(x + xOffset, scale * f(x * (1/scale)) + yOffset);
    }
    ctx.stroke();
  }

  const drawSineWave = drawWave(Math.sin);
  const drawCosineWave = drawWave(Math.cos);

  onReady(() => {
    const svg = document.getElementById("periodic-identities-unit-circle");
    const thetaLine = svg.querySelector("[data-target=theta]");
    const cosineLine = svg.querySelector("[data-target=cosine]");
    const sineLine = svg.querySelector("[data-target=sine]");
    const lineNegTheta = svg.querySelector("[data-target=-theta]");
    const lineNegThetaCosine = svg.querySelector("[data-target=-theta_cosine]");
    const linePiMinusTheta = svg.querySelector("[data-target=pi-theta]");
    const linePiMinusThetaSine = svg.querySelector("[data-target=pi-theta_sine]");

    const thetaInput = document.querySelector("[name=theta]");
    const cosineInput = document.querySelector("[name=cosine_theta]");
    const sineInput = document.querySelector("[name=sine_theta]");
    const negThetaInput = document.querySelector("[name=-theta]");
    const piMinusThetaInput = document.querySelector("[name=pi-theta]");

    const updateUnitCircle = (theta) => {
      const [x, y] = point(theta);

      cosineInput.value = x;
      sineInput.value = y;
      negThetaInput.value = round(-1 * theta);
      piMinusThetaInput.value = round(Math.PI - theta);

      thetaLine.setAttribute("x2", x * 100);
      thetaLine.setAttribute("y2", y * -100);

      cosineLine.setAttribute("x2", x * 100);

      sineLine.setAttribute("x1", x * 100);
      sineLine.setAttribute("x2", x * 100);
      sineLine.setAttribute("y2", y * -100);

      lineNegTheta.setAttribute("x2", x * 100);
      lineNegTheta.setAttribute("y2", y * 100);

      lineNegThetaCosine.setAttribute("x1", 0);
      lineNegThetaCosine.setAttribute("y1", y * 100);
      lineNegThetaCosine.setAttribute("x2", x * 100);
      lineNegThetaCosine.setAttribute("y2", y * 100);

      linePiMinusTheta.setAttribute("x2", x * -100);
      linePiMinusTheta.setAttribute("y2", y * -100);

      linePiMinusThetaSine.setAttribute("x1", x * -100);
      linePiMinusThetaSine.setAttribute("y1", 0);
      linePiMinusThetaSine.setAttribute("x2", x * -100);
      linePiMinusThetaSine.setAttribute("y2", y * -100);
    }

    const canvas = document.getElementById("periodic-identities-graph");
    maintainResolution(canvas);

    const updateGraph = (theta) => {
      const height = canvas.height;
      const width = canvas.width;
      const scale = height / 4;
      const lineWidth = 3;
      const zero = Math.PI;

      const ctx = canvas.getContext("2d");

      ctx.clearRect(0, 0, canvas.width, canvas.height);

      ctx.lineWidth = lineWidth;
      ctx.strokeStyle = "black";

      ctx.beginPath();
      ctx.setLineDash([lineWidth, lineWidth]);
      ctx.moveTo(zero * scale, 0);
      ctx.lineTo(zero * scale, height);
      ctx.stroke();
      ctx.setLineDash([]);

      ctx.beginPath();
      ctx.setLineDash([lineWidth, lineWidth]);
      ctx.moveTo(0, height / 2);
      ctx.lineTo(width, height / 2);
      ctx.stroke();
      ctx.setLineDash([]);

      ctx.beginPath();
      ctx.moveTo((theta + zero) * scale, 0);
      ctx.lineTo((theta + zero) * scale, height);
      ctx.stroke();

      drawSineWave(ctx, {
        start: -(zero * scale),
        end: width,
        scale,
        x: 0,
        y: height / 2,
        lineWidth,
        strokeStyle: "seagreen",
      });
      drawCosineWave(ctx, {
        start: -(zero * scale),
        end: width,
        scale,
        x: 0,
        y: height / 2,
        lineWidth,
        strokeStyle: "coral",
      });
    };

    const initialTheta = parseFloat(thetaInput.value);

    updateUnitCircle(initialTheta);
    updateGraph(initialTheta);

    thetaInput.addEventListener("change", () => {
      const theta = parseFloat(thetaInput.value);

      updateUnitCircle(theta);
      updateGraph(theta);
    });

    cosineInput.addEventListener("change", () => {
      const cosine = parseFloat(cosineInput.value);
      const theta = round(Math.acos(cosine));

      thetaInput.value = theta;

      updateUnitCircle(theta);
      updateGraph(theta);
    });

    sineInput.addEventListener("change", () => {
      const sine = parseFloat(sineInput.value);
      const theta = round(Math.asin(sine));

      thetaInput.value = theta;

      updateUnitCircle(theta);
      updateGraph(theta);
    });

    const resizeObserver = new ResizeObserver(() => {
      requestAnimationFrame(() => {
        maintainResolution(canvas);
        const theta = parseFloat(thetaInput.value);
        updateGraph(theta);
      });
    });
    resizeObserver.observe(canvas);
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

## Law of sines

Given a triangle $ABC$.

<svg viewBox="0 0 100 50">
  <style>
    text { font: italic 8px serif; }
  </style>
  <polygon
    fill="none"
    stroke="black"
    stroke-width="0.5"
    points="
      10,40
      30,10
      90,40
    "
  />
  <text x="8" y="42" text-anchor="end">A</text>
  <text x="30" y="8">B</text>
  <text x="92" y="42">C</text>
</svg>

$$
\frac{\sin{ \left( \angle CAB \right) }}{\overline{BC}}
  = \frac{\sin{ \left( \angle BCA \right) }}{\overline{AB}}
$$

<details>
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  Given a line $\overline{BD}$ perpendicular to, and meeting, $\overline{AC}$.

  <svg viewBox="0 0 100 50">
    <style>
      text { font: italic 8px serif; }
    </style>
    <polygon
      fill="none"
      stroke="black"
      stroke-width="0.5"
      points="
        10,40
        30,10
        90,40
      "
    />
    <text x="8" y="42" text-anchor="end">A</text>
    <text x="30" y="8">B</text>
    <text x="92" y="42">C</text>
    <line x1="30" y1="10" x2="30" y2="40" fill="none" stroke="black" stroke-width="0.5" />
    <text x="32" y="38">D</text>
  </svg>

  $$
  \begin{multline}
  \sin{ \left( \angle CAB \right) } = \frac{\overline{BD}}{\overline{AB}} \\
  \left( \overline{AB} \right) \left( \sin{ \left( \angle CAB \right) } \right) = \overline{BD} \\
  \sin{ \left( \angle BCA \right) } = \frac{\overline{BD}}{\overline{BC}} \\
  \left( \overline{BC} \right) \left( \sin{ \left( \angle BCA \right) } \right) = \overline{BD} \\
  \left( \overline{AB} \right) \left( \sin{ \left( \angle CAB \right) } \right)
    = \left( \overline{BC} \right) \left( \sin{ \left( \angle BCA \right) } \right) \\
  \sin{ \left( \angle CAB \right) }
    = \frac
      {\left( \overline{BC} \right) \left( \sin{ \left( \angle BCA \right) } \right)}
      {\overline{AB}} \\
  \frac
    {\sin{ \left( \angle CAB \right) }}
    {\overline{BC}}
    = \frac
      {\sin{ \left( \angle BCA \right) }}
      {\overline{AB}}
  \end{multline}
  $$

  </div>
</details>

## Law of cosines

Given a triangle $ABC$. With lines $a = \overline{BC}$, $b = \overline{CA}$ and $c = \overline{AB}$; and angles $\alpha = \angle CAB$, $\beta = \angle ABC$ and $\gamma = \angle BCA$.

<svg viewBox="0 0 100 52">
  <style>
    text { font: italic 6px serif; }
  </style>
  <polygon
    fill="none"
    stroke="black"
    stroke-width="0.5"
    points="
      10,40
      30,10
      90,40
    "
  />
  <text x="8" y="42" text-anchor="end">A</text>
  <text x="15" y="38" text-anchor="start">&alpha;</text>
  <text x="62" y="22" text-anchor="middle">a</text>
  <text x="30" y="8" text-anchor="middle">C</text>
  <text x="30" y="20" text-anchor="middle">&gamma;</text>
  <text x="50" y="48" text-anchor="middle">c</text>
  <text x="92" y="42">B</text>
  <text x="72" y="37" text-anchor="end">&beta;</text>
  <text x="15" y="25" text-anchor="middle">b</text>
</svg>

$$
a^2 = b^2 + c^2 - 2bc\cos\alpha
$$

<details open="open">
  <summary>
    Proof
  </summary>

  <div class="p-4 rounded-lg ba b--light-gray markdown">

  Given a line $x = \overline{BD}$ perpendicular to, and meeting, $b = \overline{AC}$.

  <svg viewBox="0 0 100 52">
    <style>
      text { font: italic 6px serif; }
    </style>
    <polygon
      fill="none"
      stroke="black"
      stroke-width="0.5"
      points="
        10,40
        30,10
        90,40
      "
    />
    <text x="8" y="42" text-anchor="end">A</text>
    <text x="15" y="38" text-anchor="start">&alpha;</text>
    <text x="62" y="22" text-anchor="middle">a</text>
    <text x="30" y="8" text-anchor="middle">C</text>
    <text x="48" y="47" text-anchor="middle">c</text>
    <text x="92" y="42">B</text>
    <text x="15" y="25" text-anchor="middle">b</text>
    <line x1="30" y1="10" x2="30" y2="40" fill="none" stroke="black" stroke-width="0.5" />
    <text x="30" y="48"  text-anchor="middle">D</text>
    <text x="32" y="28" text-anchor="start">x</text>
  </svg>

  <figure class="wide">

  $$
  \begin{multline}
  \cos \alpha = \frac{\overline{AD}}{b} \\
  b \cos \alpha = \overline{AD} \\
  \overline{BD} = c - \overline{AD} \\
  \overline{BD} = c - b \cos \alpha \\
  \sin \alpha = \frac
    {x}
    {b} \\
  b \sin \alpha = x \\
  a^2 = x^2 + \overline{BD}^2 \\
  a^2
    = \left( b \sin \alpha \right)^2 +
      \overline{BD}^2 \\
  a^2
    = \left( b \sin \alpha \right)^2 +
      \left( c - b \cos \alpha \right)^2 \\
  a^2
    = b^2 \left(\sin \alpha \right)^2 +
      \left( c - b \cos \alpha \right)^2 \\
  a^2
    = b^2 \left(\sin \alpha \right)^2 +
      c^2 - 2bc\cos\alpha + b^2 \left( \cos \alpha \right)^2 \\
  a^2
    = b^2
        \left(
          \left( \sin \alpha \right)^2 +
          \left( \cos \alpha \right)^2
        \right) +
      c^2 -
      2bc\cos\alpha \\
  a^2 = b^2 + c^2 - 2bc\cos\alpha \\
  \end{multline}
  $$

  </figure>
  </div>
</details>
