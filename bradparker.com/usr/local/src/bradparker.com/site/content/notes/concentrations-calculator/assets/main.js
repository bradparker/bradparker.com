const round = (n, p = 1000) => Math.round(n * p) / p;

const isEmptyInput = (input) => input.value === "";

const floatInputValue = (input) => {
  const value = parseFloat(input.value, 10);
  if (Number.isNaN(value)) return null;
  return value;
};

const mapObject = (f, o) =>
  Object.fromEntries(Object.entries(o).map(([k, v]) => [k, f(v)]));

const filterObject = (p, o) =>
  Object.fromEntries(Object.entries(o).filter(([k, v]) => p(k, v)));

const scopeName = (scope, name) => {
  return `${scope}[${name}]`;
};

const unscopeName = (scope, scopedName) => {
  return scopedName.replace(scope, "").replace("[", "").replace("]", "");
};

const getElementByScopedName = (scope) => (name) =>
  document.querySelector(`[name='${scopeName(scope, name)}']`);

const approxEqual = (a, b) => Math.abs(a - b) <= 0.001;

const completions = {
  v: [
    {
      needs: new Set(["v_a", "v_b"]),
      complete({ v_a, v_b }) {
        return v_a + v_b;
      },
    },
  ],
  c: [
    {
      needs: new Set(["c_a", "v_a", "c_b", "v_b", "v"]),
      complete({ c_a, v_a, c_b, v_b, v }) {
        if (v === 0) return null;
        return (c_a * v_a + c_b * v_b) / v;
      },
    },
  ],
  c_a: [
    {
      needs: new Set(["c", "v", "c_b", "v_b", "v_a"]),
      complete({ c, v, c_b, v_b, v_a }) {
        if (v_a === 0) return null;
        return (c * v - c_b * v_b) / v_a;
      },
    },
  ],
  v_a: [
    {
      needs: new Set(["v", "v_b"]),
      complete({ v, v_b }) {
        if (v < v_b) return null;
        return v - v_b;
      },
    },
    {
      needs: new Set(["v_b", "c_b", "c", "c_a"]),
      complete({ v_b, c_b, c, c_a }) {
        if (c === c_b) return null;
        return (-v_b * (c_b - c)) / (c_a - c);
      },
    },
    {
      needs: new Set(["v", "c", "c_a", "c_b"]),
      complete({ v, c, c_a, c_b }) {
        if (c_a === c_b) return null;
        return (v * (c - c_b)) / (c_a - c_b);
      },
    },
  ],
  c_b: [
    {
      needs: new Set(["c", "v", "c_a", "v_a", "v_b"]),
      complete({ c, v, c_a, v_a, v_b }) {
        if (v_b === 0) return null;
        return (c * v - c_a * v_a) / v_b;
      },
    },
  ],
  v_b: [
    {
      needs: new Set(["v", "v_a"]),
      complete({ v, v_a }) {
        if (v < v_a) return null;
        return v - v_a;
      },
    },
    {
      needs: new Set(["v_a", "c_a", "c", "c_b"]),
      complete({ v_a, c_a, c, c_b }) {
        if (c === c_a) return null;
        return (-v_a * (c_a - c)) / (c_b - c);
      },
    },
    {
      needs: new Set(["v", "c", "c_b", "c_a"]),
      complete({ v, c, c_b, c_a }) {
        if (c_a === c_b) return null;
        return (v * (c - c_a)) / (c_b - c_a);
      },
    },
  ],
};

const equation = (inputs) => {
  const reset = () => {
    Object.values(inputs).forEach((input) => {
      input.value = "";
    });
  };

  const currentValues = () =>
    mapObject(
      floatInputValue,
      filterObject((_, v) => !isEmptyInput(v), inputs),
    );

  const complete = (changedVariableName, depth = 0) => {
    if (depth > 5) return;

    const values = currentValues();
    const populatedVariables = new Set(Object.keys(values));
    const evaluableDependants = Object.entries(completions)
      .map(([name, candidateFormulae]) => {
        const formula =
          candidateFormulae
            .sort((a, b) => a.needs.size - b.needs.size)
            .find(({ needs }) => {
              return (
                needs.has(changedVariableName) &&
                needs.isSubsetOf(populatedVariables)
              );
            }) || null;

        return {
          name,
          formula,
        };
      })
      .filter(({ formula }) => !!formula);

    evaluableDependants.forEach(({ name, formula }) => {
      const computedValue = formula.complete(values);
      if (computedValue === null || computedValue < 0) {
        inputs[name].value = "";
        return;
      }

      const currentValue = values[name];
      const nextValue = round(computedValue);
      if (approxEqual(currentValue, nextValue)) return;

      inputs[name].value = nextValue;
      complete(name, depth + 1);
    });
  };

  return complete;
};

const inTermsOfCV = () => {
  const scope = "cv";
  const getElementByName = getElementByScopedName(scope);

  const inputs = mapObject(getElementByName, {
    c_a: "c_a",
    v_a: "v_a",
    c_b: "c_b",
    v_b: "v_b",
    c: "c",
    v: "v",
  });

  const complete = equation(inputs);

  const handleChange = (event) => {
    complete(unscopeName(scope, event.target.name));
  };

  Object.values(inputs).forEach((input) => {
    input.addEventListener("change", handleChange);
  });
};

const main = () => {
  inTermsOfCV();
};

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", main);
} else {
  main();
}
