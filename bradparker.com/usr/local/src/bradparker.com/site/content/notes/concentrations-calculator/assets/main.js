const round = (n, p = 1000000000) =>
  Math.round(n * p) / p;

const isEmptyInput = (input) =>
  input.value === "";

const floatInputValue = (input) =>
  !isEmptyInput(input) ? parseFloat(input.value, 10) : null;

const mapObject = (f, o) =>
  Object.fromEntries(
    Object.entries(o).map(([k, v]) => [k, f(v)]),
  );

const filterObject = (p, o) =>
  Object.fromEntries(
    Object.entries(o).filter(([k, v]) => p(k, v)),
  );

const getInput = (name) =>
  document.querySelector(`[name=${name}]`);

const completions = {
  v: [
    {
      require: new Set(["v_a", "v_b"]),
      complete ({ v_a, v_b }) {
        return v_a + v_b;
      },
    },
  ],
  c: [
    {
      require: new Set(["c_a", "v_a", "c_b", "v_b", "v"]),
      complete ({ c_a, v_a, c_b, v_b, v }) {
        return (c_a * v_a + c_b * v_b) / v;
      }
    }
  ],
  c_a: [
    {
      require: new Set(["c", "c_b", "v", "v_b", "v_a"]),
      complete ({ c, c_b, v, v_b, v_a }) {
        return (c * v - c_b * v_b) / v_a;
      },
    },
  ],
  v_a: [
    {
      require: new Set(["v", "v_b"]),
      complete ({ v, v_b }) {
        return v - v_b;
      }
    },
    {
      require: new Set(["v", "c", "c_a", "c_b"]),
      complete ({ v, c, c_a, c_b }) {
        return v * (c - c_b) / (c_a - c_b);
      }
    },
  ],
  c_b: [
    {
      require: new Set(["c", "c_a", "v", "v_a", "v_b"]),
      complete ({ c, c_a, v, v_a, v_b }) {
        return (c * v - c_a * v_a) / v_b;
      },
    },
  ],
  v_b: [
    {
      require: new Set(["v", "v_a"]),
      complete ({ v, v_a }) {
        return v - v_a;
      }
    },
    {
      require: new Set(["v", "c", "c_b", "c_a"]),
      complete ({ v, c, c_b, c_a }) {
        return v * (c - c_a) / (c_b - c_a);
      }
    },
  ],
};

const main = () => {
  const inputs = mapObject(getInput, {
    c_a: "c_a",
    v_a: "v_a",
    c_b: "c_b",
    v_b: "v_b",
    c: "c",
    v: "v",
  });

  const currentValues = () =>
    mapObject(
      floatInputValue,
      filterObject(
        (_, v) => !isEmptyInput(v),
        inputs,
      ),
    );

  const completeInput = (name) => {
    const values = currentValues();
    const completedNames = new Set(Object.keys(values));
    const completion = completions[name].find((candidate) => {
      return candidate.require.isSubsetOf(completedNames);
    });

    if (!completion) return;

    const value = round(completion.complete(values));
    inputs[name].value = value;
  }

  const complete = () => {
    const start = new Set(Object.keys(currentValues()));

    const emptyInputNames = Object.keys(
      filterObject(
        (_, v) => isEmptyInput(v),
        inputs,
      ),
    );

    emptyInputNames.forEach(completeInput);

    const end = new Set(Object.keys(currentValues()));
    const changed = end.difference(start);

    if (changed.size === 0) return;

    complete();
  };

  Object.values(inputs).forEach(input => {
    input.addEventListener("change", complete);
  });

  document.getElementById("reset").addEventListener("click", () => {
    Object.values(inputs).forEach(input => {
      input.value = "";
    });
  });
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main);
} else {
  main();
}
