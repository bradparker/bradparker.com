---
title: String quartet voicings
tags:
  - mathematics
  - music
description: |
  All the four note voicings a string quartet can play
---

## Linear

<button id="string-quartet-linear-control">
  Play
</button>

## Pseudo random

<button id="string-quartet-random-control">
  Play
</button>

<details>
  <summary>Source code</summary>

  ```javascript
  const iterate = function* (initial, f) {
    let x = initial;
    while (true) {
      yield x;
      x = f(x);
    }
  }

  const zip = function* (ia, ib) {
    let a = ia.next();
    let b = ib.next();
    while (!a.done && !b.done) {
      yield [a.value, b.value];
      a = ia.next();
      b = ib.next();
    }
  }

  const zipAsync = async function* (ia, ib) {
    let a = await ia.next();
    let b = await ib.next();
    while (!a.done && !b.done) {
      yield [a.value, b.value];
      a = await ia.next();
      b = await ib.next();
    }
  }

  const animationFrame = () => new Promise(resolve => {
    requestAnimationFrame(() => {
      resolve()
    });
  });

  const animationFrames = async function* () {
    while (true) {
      yield await animationFrame();
    }
  }

  const notes = [
    {
      "pitch_class": "C",
      "octave": 0,
      "frequency": 16.351
    },
    {
      "pitch_class": "C#",
      "octave": 0,
      "frequency": 17.324
    },
    {
      "pitch_class": "D",
      "octave": 0,
      "frequency": 18.354
    },
    {
      "pitch_class": "D#",
      "octave": 0,
      "frequency": 19.445
    },
    {
      "pitch_class": "E",
      "octave": 0,
      "frequency": 20.601
    },
    {
      "pitch_class": "F",
      "octave": 0,
      "frequency": 21.827
    },
    {
      "pitch_class": "F#",
      "octave": 0,
      "frequency": 23.124
    },
    {
      "pitch_class": "G",
      "octave": 0,
      "frequency": 24.499
    },
    {
      "pitch_class": "G#",
      "octave": 0,
      "frequency": 25.956
    },
    {
      "pitch_class": "A",
      "octave": 0,
      "frequency": 27.5
    },
    {
      "pitch_class": "A#",
      "octave": 0,
      "frequency": 29.135
    },
    {
      "pitch_class": "B",
      "octave": 0,
      "frequency": 30.868
    },
    {
      "pitch_class": "C",
      "octave": 1,
      "frequency": 32.703
    },
    {
      "pitch_class": "C#",
      "octave": 1,
      "frequency": 34.648
    },
    {
      "pitch_class": "D",
      "octave": 1,
      "frequency": 36.708
    },
    {
      "pitch_class": "D#",
      "octave": 1,
      "frequency": 38.891
    },
    {
      "pitch_class": "E",
      "octave": 1,
      "frequency": 41.203
    },
    {
      "pitch_class": "F",
      "octave": 1,
      "frequency": 43.654
    },
    {
      "pitch_class": "F#",
      "octave": 1,
      "frequency": 46.249
    },
    {
      "pitch_class": "G",
      "octave": 1,
      "frequency": 48.999
    },
    {
      "pitch_class": "G#",
      "octave": 1,
      "frequency": 51.913
    },
    {
      "pitch_class": "A",
      "octave": 1,
      "frequency": 55
    },
    {
      "pitch_class": "A#",
      "octave": 1,
      "frequency": 58.27
    },
    {
      "pitch_class": "B",
      "octave": 1,
      "frequency": 61.735
    },
    {
      "pitch_class": "C",
      "octave": 2,
      "frequency": 65.406
    },
    {
      "pitch_class": "C#",
      "octave": 2,
      "frequency": 69.296
    },
    {
      "pitch_class": "D",
      "octave": 2,
      "frequency": 73.416
    },
    {
      "pitch_class": "D#",
      "octave": 2,
      "frequency": 77.782
    },
    {
      "pitch_class": "E",
      "octave": 2,
      "frequency": 82.407
    },
    {
      "pitch_class": "F",
      "octave": 2,
      "frequency": 87.307
    },
    {
      "pitch_class": "F#",
      "octave": 2,
      "frequency": 92.499
    },
    {
      "pitch_class": "G",
      "octave": 2,
      "frequency": 97.999
    },
    {
      "pitch_class": "G#",
      "octave": 2,
      "frequency": 103.826
    },
    {
      "pitch_class": "A",
      "octave": 2,
      "frequency": 110
    },
    {
      "pitch_class": "A#",
      "octave": 2,
      "frequency": 116.541
    },
    {
      "pitch_class": "B",
      "octave": 2,
      "frequency": 123.471
    },
    {
      "pitch_class": "C",
      "octave": 3,
      "frequency": 130.813
    },
    {
      "pitch_class": "C#",
      "octave": 3,
      "frequency": 138.591
    },
    {
      "pitch_class": "D",
      "octave": 3,
      "frequency": 146.832
    },
    {
      "pitch_class": "D#",
      "octave": 3,
      "frequency": 155.563
    },
    {
      "pitch_class": "E",
      "octave": 3,
      "frequency": 164.814
    },
    {
      "pitch_class": "F",
      "octave": 3,
      "frequency": 174.614
    },
    {
      "pitch_class": "F#",
      "octave": 3,
      "frequency": 184.997
    },
    {
      "pitch_class": "G",
      "octave": 3,
      "frequency": 195.998
    },
    {
      "pitch_class": "G#",
      "octave": 3,
      "frequency": 207.652
    },
    {
      "pitch_class": "A",
      "octave": 3,
      "frequency": 220
    },
    {
      "pitch_class": "A#",
      "octave": 3,
      "frequency": 233.082
    },
    {
      "pitch_class": "B",
      "octave": 3,
      "frequency": 246.942
    },
    {
      "pitch_class": "C",
      "octave": 4,
      "frequency": 261.626
    },
    {
      "pitch_class": "C#",
      "octave": 4,
      "frequency": 277.183
    },
    {
      "pitch_class": "D",
      "octave": 4,
      "frequency": 293.665
    },
    {
      "pitch_class": "D#",
      "octave": 4,
      "frequency": 311.127
    },
    {
      "pitch_class": "E",
      "octave": 4,
      "frequency": 329.628
    },
    {
      "pitch_class": "F",
      "octave": 4,
      "frequency": 349.228
    },
    {
      "pitch_class": "F#",
      "octave": 4,
      "frequency": 369.994
    },
    {
      "pitch_class": "G",
      "octave": 4,
      "frequency": 391.995
    },
    {
      "pitch_class": "G#",
      "octave": 4,
      "frequency": 415.305
    },
    {
      "pitch_class": "A",
      "octave": 4,
      "frequency": 440
    },
    {
      "pitch_class": "A#",
      "octave": 4,
      "frequency": 466.164
    },
    {
      "pitch_class": "B",
      "octave": 4,
      "frequency": 493.883
    },
    {
      "pitch_class": "C",
      "octave": 5,
      "frequency": 523.251
    },
    {
      "pitch_class": "C#",
      "octave": 5,
      "frequency": 554.365
    },
    {
      "pitch_class": "D",
      "octave": 5,
      "frequency": 587.33
    },
    {
      "pitch_class": "D#",
      "octave": 5,
      "frequency": 622.254
    },
    {
      "pitch_class": "E",
      "octave": 5,
      "frequency": 659.255
    },
    {
      "pitch_class": "F",
      "octave": 5,
      "frequency": 698.456
    },
    {
      "pitch_class": "F#",
      "octave": 5,
      "frequency": 739.989
    },
    {
      "pitch_class": "G",
      "octave": 5,
      "frequency": 783.991
    },
    {
      "pitch_class": "G#",
      "octave": 5,
      "frequency": 830.609
    },
    {
      "pitch_class": "A",
      "octave": 5,
      "frequency": 880
    },
    {
      "pitch_class": "A#",
      "octave": 5,
      "frequency": 932.328
    },
    {
      "pitch_class": "B",
      "octave": 5,
      "frequency": 987.767
    },
    {
      "pitch_class": "C",
      "octave": 6,
      "frequency": 1046.502
    },
    {
      "pitch_class": "C#",
      "octave": 6,
      "frequency": 1108.731
    },
    {
      "pitch_class": "D",
      "octave": 6,
      "frequency": 1174.659
    },
    {
      "pitch_class": "D#",
      "octave": 6,
      "frequency": 1244.508
    },
    {
      "pitch_class": "E",
      "octave": 6,
      "frequency": 1318.51
    },
    {
      "pitch_class": "F",
      "octave": 6,
      "frequency": 1396.913
    },
    {
      "pitch_class": "F#",
      "octave": 6,
      "frequency": 1479.978
    },
    {
      "pitch_class": "G",
      "octave": 6,
      "frequency": 1567.982
    },
    {
      "pitch_class": "G#",
      "octave": 6,
      "frequency": 1661.219
    },
    {
      "pitch_class": "A",
      "octave": 6,
      "frequency": 1760
    },
    {
      "pitch_class": "A#",
      "octave": 6,
      "frequency": 1864.655
    },
    {
      "pitch_class": "B",
      "octave": 6,
      "frequency": 1975.533
    },
    {
      "pitch_class": "C",
      "octave": 7,
      "frequency": 2093.005
    },
    {
      "pitch_class": "C#",
      "octave": 7,
      "frequency": 2217.461
    },
    {
      "pitch_class": "D",
      "octave": 7,
      "frequency": 2349.318
    },
    {
      "pitch_class": "D#",
      "octave": 7,
      "frequency": 2489.016
    },
    {
      "pitch_class": "E",
      "octave": 7,
      "frequency": 2637.021
    },
    {
      "pitch_class": "F",
      "octave": 7,
      "frequency": 2793.826
    },
    {
      "pitch_class": "F#",
      "octave": 7,
      "frequency": 2959.955
    },
    {
      "pitch_class": "G",
      "octave": 7,
      "frequency": 3135.964
    },
    {
      "pitch_class": "G#",
      "octave": 7,
      "frequency": 3322.438
    },
    {
      "pitch_class": "A",
      "octave": 7,
      "frequency": 3520
    },
    {
      "pitch_class": "A#",
      "octave": 7,
      "frequency": 3729.31
    },
    {
      "pitch_class": "B",
      "octave": 7,
      "frequency": 3951.066
    },
    {
      "pitch_class": "C",
      "octave": 8,
      "frequency": 4186.009
    },
    {
      "pitch_class": "C#",
      "octave": 8,
      "frequency": 4434.922
    },
    {
      "pitch_class": "D",
      "octave": 8,
      "frequency": 4698.636
    },
    {
      "pitch_class": "D#",
      "octave": 8,
      "frequency": 4978.032
    },
    {
      "pitch_class": "E",
      "octave": 8,
      "frequency": 5274.042
    },
    {
      "pitch_class": "F",
      "octave": 8,
      "frequency": 5587.652
    },
    {
      "pitch_class": "F#",
      "octave": 8,
      "frequency": 5919.91
    },
    {
      "pitch_class": "G",
      "octave": 8,
      "frequency": 6271.928
    },
    {
      "pitch_class": "G#",
      "octave": 8,
      "frequency": 6644.876
    },
    {
      "pitch_class": "A",
      "octave": 8,
      "frequency": 7040
    },
    {
      "pitch_class": "A#",
      "octave": 8,
      "frequency": 7458.62
    },
    {
      "pitch_class": "B",
      "octave": 8,
      "frequency": 7902.132
    },
    {
      "pitch_class": "C",
      "octave": 9,
      "frequency": 8372.018
    },
    {
      "pitch_class": "C#",
      "octave": 9,
      "frequency": 8869.844
    },
    {
      "pitch_class": "D",
      "octave": 9,
      "frequency": 9397.272
    },
    {
      "pitch_class": "D#",
      "octave": 9,
      "frequency": 9956.064
    },
    {
      "pitch_class": "E",
      "octave": 9,
      "frequency": 10548.084
    },
    {
      "pitch_class": "F",
      "octave": 9,
      "frequency": 11175.304
    },
    {
      "pitch_class": "F#",
      "octave": 9,
      "frequency": 11839.82
    },
    {
      "pitch_class": "G",
      "octave": 9,
      "frequency": 12543.856
    },
    {
      "pitch_class": "G#",
      "octave": 9,
      "frequency": 13289.752
    },
    {
      "pitch_class": "A",
      "octave": 9,
      "frequency": 14080
    },
    {
      "pitch_class": "A#",
      "octave": 9,
      "frequency": 14917.24
    },
    {
      "pitch_class": "B",
      "octave": 9,
      "frequency": 15804.264
    }
  ];

  const noteEqual = (a) => (b) =>
    a.pitch_class === b.pitch_class && a.octave === b.octave

  const range = (start, end) => {
    const startIndex = notes.findIndex(noteEqual(start));
    const endIndex = notes.findIndex(noteEqual(end)) + 1;

    if (startIndex === -1) {
      throw new Error(`Note not found: ${start}`)
    }

    if (endIndex === -1) {
      throw new Error(`Note not found: ${end}`)
    }

    return {
      length: endIndex - startIndex,
      at (index) {
        const offsetIndex = startIndex + index;

        if (endIndex < offsetIndex) {
          throw new Error(`Index out of bounds: ${startIndex} <= ${offsetIndex} <= ${endIndex}`)
        }

        return notes[offsetIndex];
      },
      index (note) {
        const noteIndex = notes.findIndex(noteEqual(note));

        if (noteIndex < startIndex || noteIndex > endIndex) {
          throw new Error(`Note out of range: ${start.pitch_class}${start.octave} <= ${note.pitch_class}${note.octave} <= ${end.pitch_class}${note.octave}`)
        }

        return noteIndex - startIndex;
      }
    };
  };

  const violinRange = range(
    { pitch_class: "G", octave: 3 },
    { pitch_class: "A", octave: 7 }
  );

  const violaRange = range(
    { pitch_class: "C", octave: 3 },
    { pitch_class: "E", octave: 6 }
  );

  const celloRange = range(
    { pitch_class: "C", octave: 2 },
    { pitch_class: "A", octave: 5 }
  );

  const divMod = (n, d) => [
    Math.floor(n / d),
    n % d
  ];

  const encode = ([violin_1, violin_2, viola, cello]) =>
    cello +
      celloRange.length * (viola +
        violaRange.length * (violin_2 +
          violinRange.length * violin_1));

  const decode = (index) => {
    let i, celloIndex, violaIndex, firstViolinIndex, secondViolinIndex;
    [i, celloIndex] = divMod(index, celloRange.length);
    [i, violaIndex] = divMod(i, violaRange.length);
    [firstViolinIndex, secondViolinIndex] = divMod(i, violinRange.length);
    return [firstViolinIndex, secondViolinIndex, violaIndex, celloIndex];
  }

  const init = () => {
    const audioContext = new AudioContext();
    const gainNode = audioContext.createGain();

    gainNode.gain.value = 0.1;
    gainNode.connect(audioContext.destination);

    const firstViolin = audioContext.createOscillator();
    const secondViolin = audioContext.createOscillator();
    const viola = audioContext.createOscillator();
    const cello = audioContext.createOscillator();

    firstViolin.connect(gainNode);
    secondViolin.connect(gainNode);
    viola.connect(gainNode);
    cello.connect(gainNode);

    return {
      audioContext,
      gainNode,
      firstViolin,
      secondViolin,
      viola,
      cello
    }
  }

  const generate = async ({
    sequence,
    startTime,
    buffer,
    audioContext,
    firstViolin,
    secondViolin,
    viola,
    cello
  }) => {
    const indexedSequence = zip(iterate(0, n => n + 1), sequence);
    const spacedIndexedSequence = zipAsync(indexedSequence, animationFrames());

    return new Promise(async (resolve) => {
      for await (const [value] of spacedIndexedSequence) {
        const [i, x] = value;

        if (i > m) { return; }

        const [
          firstViolinIndex,
          secondViolinIndex,
          violaIndex,
          celloIndex
        ] = decode(x);

        const offsetTime = i * 0.1 + startTime;

        firstViolin.frequency.setValueAtTime(
          violinRange.at(firstViolinIndex).frequency,
          offsetTime,
        );
        secondViolin.frequency.setValueAtTime(
          violinRange.at(secondViolinIndex).frequency,
          offsetTime,
        );
        viola.frequency.setValueAtTime(
          violaRange.at(violaIndex).frequency,
          offsetTime,
        );
        cello.frequency.setValueAtTime(
          celloRange.at(celloIndex).frequency,
          offsetTime,
        );

        if (i === buffer) {
          resolve();
        }
      }
    });
  };

  const start = async (sequence) => {
    const {
      audioContext,
      firstViolin,
      secondViolin,
      viola,
      cello
    } = init();

    const startTime = audioContext.currentTime + 0.1;

    await generate({
      buffer: 5,
      sequence,
      startTime,
      firstViolin,
      secondViolin,
      viola,
      cello
    });

    firstViolin.start(startTime);
    secondViolin.start(startTime);
    viola.start(startTime);
    cello.start(startTime);

    return {
      audioContext,
      firstViolin,
      secondViolin,
      viola,
      cello
    };
  }

  const initControl = (control, sequence) => {
    let context = null;

    control.addEventListener("click", () => {
      if (!context) {
        start(sequence).then(({ audioContext }) => {
          context = audioContext;
          control.textContent = "Pause";
        })
      } else if (context.state === "running") {
        context.suspend().then(() => {
          control.textContent = "Play";
        });
      } else if (context.state === "suspended") {
        context.resume().then(() => {
          control.textContent = "Pause";
        });
      }
    });
  }

  const randomControl = document.getElementById("string-quartet-random-control");
  const n = 4905486; // Cello * Viola * Violin * Violin
  const m = n + 1;
  // This is heavily dependant on the value of `m`. It must be a primitive
  // root modulo `m` to work.
  const a = 4905306;
  initControl(randomControl, iterate(Math.floor(m / 2), x => a * x % m));

  const linearControl = document.getElementById("string-quartet-linear-control");
  initControl(linearControl, iterate(0, n => n + 1));
  ```
</details>

<script type="module" src="/content/posts/cracking-open-the-window-to-the-cosmos-of-combinatorics/assets/javascript/string-quartet-voicings.js"></script>
