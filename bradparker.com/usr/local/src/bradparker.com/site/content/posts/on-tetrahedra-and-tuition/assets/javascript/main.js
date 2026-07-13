import {
  BufferGeometry,
  CircleGeometry,
  Color,
  DoubleSide,
  Fog,
  Group,
  Line,
  LineBasicMaterial,
  LineSegments,
  Mesh,
  MeshBasicMaterial,
  PerspectiveCamera,
  Scene,
  ShapeGeometry,
  Vector3,
  WebGLRenderer,
} from "three";
import { OrbitControls } from "three/addons/controls/OrbitControls.js";
import { Font } from "three/addons/loaders/FontLoader.js";
import helvetikerRegularFont from "../fonts/helvetiker_regular.typeface.json" with { type: "json" };

const colors = {
  green: 0x137752,
  orange: 0xFF6300,
  yellow: 0xFFB700,
};

const origin = new Vector3(0, 0, 0);
const radius = Math.sqrt(6)/4;
const angle = Math.acos(Math.sqrt(3)/3);
const points = {
  A: new Vector3(0, radius, 0),
  B: new Vector3(-Math.sqrt(3)/6, -Math.sqrt(6)/12, -1/2),
  C: new Vector3(-Math.sqrt(3)/6, -Math.sqrt(6)/12, 1/2),
  D: new Vector3(-Math.sqrt(3)/6, -Math.sqrt(6)/12, 0),
  E: new Vector3(Math.sqrt(3)/3, -Math.sqrt(6)/12, 0),
  F: new Vector3(0, -(Math.sqrt(6)/3 - radius), 0),
  G: new Vector3(-(Math.sqrt(3)/9), (Math.sqrt(6)/36), 0),
  H: new Vector3(Math.sqrt(3)/6, Math.sqrt(6)/12, 0),
  O: origin,
};
const vertices = [
  points.A,
  points.B,
  points.C,
  points.E,
];

const displayPause = (button) => {
  button.innerHTML = "&#x23F8;";
  button.setAttribute("title", "Pause");
};

const displayPlay = (button) => {
  button.innerHTML = "&#x23F5;";
  button.setAttribute("title", "Play");
};

const createScene = (id, group, animate) => {
  const mount = document.getElementById(id);

  if (!mount) return;

  const scene = new Scene();
  scene.background = new Color(0xFFFFFF);

  const camera = new PerspectiveCamera(
    25,
    mount.clientWidth / mount.clientHeight,
    0.1,
    50,
  );
  camera.position.set(0, 2, 3);

  const renderer = new WebGLRenderer({ antialias: true });
  renderer.setPixelRatio(window.devicePixelRatio);
  renderer.setSize(mount.clientWidth, mount.clientHeight);

  let paused = false;
  let pausedDuration = 0;
  let lastPausedAt = null;
  const animation = (time) => {
    const animationTime = time - pausedDuration;
    animate({
      camera,
      paused,
      time: animationTime,
    });
    renderer.render(scene, camera);
  };
  renderer.setAnimationLoop(animation);

  mount.appendChild(renderer.domElement);

  const controls = new OrbitControls(camera, renderer.domElement);
  controls.enablePan = false;
  controls.enableZoom = false;

  const fog = new Fog(0xFFFFFF, 2.5, 5);
  scene.fog = fog;

  scene.add(group);

  window.addEventListener(
    "resize",
    function () {
      camera.aspect = mount.clientWidth / mount.clientHeight;
      camera.updateProjectionMatrix();

      renderer.setSize(mount.clientWidth, mount.clientHeight);
    },
    false,
  );

  const button = document.createElement("button");
  button.classList.add(
    "absolute",
    "right-4",
    "bottom-4",
    "border",
    "rounded-full",
    "min-w-8",
    "p-0",
    "text-center",
    "leading-8",
  );
  displayPause(button);
  button.addEventListener("click", () => {
    if (paused) {
      displayPause(button);
      pausedDuration += performance.now() - lastPausedAt;
    } else {
      lastPausedAt = performance.now();
      displayPlay(button);
    }
    paused = !paused;
  });
  mount.appendChild(button);
}

const lineMaterial = new LineBasicMaterial({
  color: 0x000000,
  fog: true,
});
const greyLineMaterial = new LineBasicMaterial({
  color: 0xBBBBBB,
  fog: true,
});

const surfaceMaterial = (color) =>
  new MeshBasicMaterial({
    color,
    transparent: true,
    opacity: 0.25,
    side: DoubleSide,
    fog: false,
  });

const greenSurfaceMaterial = surfaceMaterial(colors.green);
const orangeSurfaceMaterial = surfaceMaterial(colors.orange);
const yellowSurfaceMaterial = surfaceMaterial(colors.yellow);

const createTetrahedronWireframe = () => {
  const group = new Group();
  const lines = [
    line(points.A, points.B),
    line(points.A, points.C),
    line(points.A, points.E),
    line(points.B, points.C),
    line(points.B, points.E),
    line(points.C, points.E),
  ];
  lines.forEach(l => {
    group.add(l);
  });
  return group;
};

const line = (start, end, material = lineMaterial) =>
  new Line(
    new BufferGeometry().setFromPoints([
      start,
      end,
    ]),
    material,
  );

const vectorLine = (vector, material = lineMaterial) =>
  line(
    new Vector3(0, 0, 0),
    vector,
  );

const createVertexVectorLines = () =>
  vertices.map(v => vectorLine(v));

const font = new Font(helvetikerRegularFont);
const solidBlackMaterial = new MeshBasicMaterial({
  color: 0x000000,
  side: DoubleSide,
  fog: false,
});
const label = (content, position) => {
  const shapes = font.generateShapes(content, 0.06);
  const geometry = new ShapeGeometry(shapes);
  geometry.computeBoundingBox();
  const { boundingBox } = geometry;
  geometry.translate(
    -0.5 * (boundingBox.max.x - boundingBox.min.x),
    -0.5 * (boundingBox.max.y - boundingBox.min.y),
    0
  );
  const mesh = new Mesh(
    geometry,
    solidBlackMaterial,
  );
  mesh.position.copy(position);
  return mesh;
}

const createLabels = () => ({
  A: label("A", points.A.clone().multiplyScalar(1.1)),
  B: label("B", points.B.clone().multiplyScalar(1.1)),
  C: label("C", points.C.clone().multiplyScalar(1.1)),
  D: label("D", points.D.clone().multiplyScalar(1.2)),
  E: label("E", points.E.clone().multiplyScalar(1.1)),
  F: label("F", points.F.clone().multiplyScalar(1.3)),
  G: label("G", points.G.clone().multiplyScalar(1.3)),
  H: label("H", points.H.clone().multiplyScalar(1.2)),
  O: label("O", points.O.clone().add(new Vector3(0.1, 0.015, 0))),
});

const tetrahedronWireframe = (group = new Group()) =>  {
  group.add(createTetrahedronWireframe());
  return group;
}

const vectors = (group = new Group()) => {
  createVertexVectorLines().forEach(line => {
    group.add(line);
  });
  return group;
}

const tetrahedronWireframeWithVectors = () => {
  const group = new Group();
  tetrahedronWireframe(group);
  vectors(group);
  return group;
}

const rotating = ({ group, paused, time }) => {
  if (paused) return;
  group.rotation.y = time/2000;
}

const wireframeScene = () => {
  const group = tetrahedronWireframeWithVectors();

  createScene("scene-wireframe", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
  });
}

wireframeScene();

const wireframeAndAngleScene = () => {
  const group = tetrahedronWireframeWithVectors();

  const angleWedge = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      -2 * angle + Math.PI/2,
      2 * angle,
    ),
    orangeSurfaceMaterial,
  );

  group.add(angleWedge);

  createScene("scene-wireframe-and-angle", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
  });
}

wireframeAndAngleScene();

const createSide = () =>
  new Mesh(
    new BufferGeometry().setFromPoints([
      points.A,
      points.B,
      points.C,
    ]),
    orangeSurfaceMaterial,
  );

const wireframeAndSideWithHeightScene = () => {
  const group = tetrahedronWireframe();

  const side = createSide();
  group.add(side);

  const { A, B, C, D } = createLabels();
  const labels = [A, B, C, D];
  labels.forEach(label => {
    group.add(label);
  });

  const height = line(points.A, points.D);
  group.add(height);

  createScene("scene-wireframe-and-side-with-height", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
    labels.forEach(label => {
      label.lookAt(camera.position);
    });
  });
}

wireframeAndSideWithHeightScene();

const wireframeAndSliceScene = () => {
  const group = tetrahedronWireframe();

  const ade = new Mesh(
    new BufferGeometry().setFromPoints([
      points.A,
      points.D,
      points.E,
    ]),
    greenSurfaceMaterial,
  );
  group.add(ade);

  const { A, B, C, D, E } = createLabels();
  const labels = [A, B, C, D, E];
  labels.forEach(label => {
    group.add(label);
  });

  const ad = line(points.A, points.D);
  group.add(ad);

  const de = line(points.D, points.E);
  group.add(de);

  createScene("scene-wireframe-and-slice", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
    labels.forEach(label => {
      label.lookAt(camera.position);
    });
  });
}

wireframeAndSliceScene();

const lineAF_lineEG_Scene = () => {
  const group = tetrahedronWireframe();

  const { A, D, E, F, G } = createLabels();
  const labels = [A, D, E, F, G];
  labels.forEach(label => {
    group.add(label);
  });

  const lineAD = line(points.A, points.D);
  group.add(lineAD);

  const lineDE = line(points.D, points.E);
  group.add(lineDE);

  const lineAF = line(points.A, points.F);
  group.add(lineAF);

  const lineEG = line(points.E, points.G);
  group.add(lineEG);

  const angleAOE = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      -2 * angle + Math.PI/2,
      2 * angle,
    ),
    orangeSurfaceMaterial,
  );

  group.add(angleAOE);

  createScene("scene-lineAF-lineEG", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
    labels.forEach(label => {
      label.lookAt(camera.position);
    });
  });
}

lineAF_lineEG_Scene();

const angleDEA_Scene = () => {
  const group = tetrahedronWireframe();

  const { A, D, E, F, G } = createLabels();
  const labels = [A, D, E, F, G];
  labels.forEach(label => {
    group.add(label);
  });

  const lineAD = line(points.A, points.D);
  group.add(lineAD);

  const lineDE = line(points.D, points.E);
  group.add(lineDE);

  const lineAF = line(points.A, points.F);
  group.add(lineAF);

  const lineEG = line(points.E, points.G);
  group.add(lineEG);

  const angleDEA = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      0,
      angle,
    ),
    greenSurfaceMaterial,
  );
  angleDEA.position.copy(points.E);
  angleDEA.rotation.y = Math.PI;
  group.add(angleDEA);

  createScene("scene-angleDEA", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
    labels.forEach(label => {
      label.lookAt(camera.position);
    });
  });
}

angleDEA_Scene();

const lineDH_Scene = () => {
  const group = tetrahedronWireframe();

  const { A, D, E, F, G, H } = createLabels();
  const labels = [A, D, E, F, G, H];
  labels.forEach(label => {
    group.add(label);
  });

  const lineAD = line(points.A, points.D);
  group.add(lineAD);

  const lineDE = line(points.D, points.E);
  group.add(lineDE);

  const lineAF = line(points.A, points.F);
  group.add(lineAF);

  const lineEG = line(points.E, points.G);
  group.add(lineEG);

  const lineDH = line(points.D, points.H);
  group.add(lineDH);

  const angleDEA = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      0,
      angle,
    ),
    greenSurfaceMaterial,
  );
  angleDEA.position.copy(points.E);
  angleDEA.rotation.y = Math.PI;
  group.add(angleDEA);

  createScene("scene-lineDH", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
    labels.forEach(label => {
      label.lookAt(camera.position);
    });
  });
}

lineDH_Scene();

const angleFAE_angleHDE_Scene = () => {
  const group = tetrahedronWireframe();

  const { A, D, E, F, G, H } = createLabels();
  const labels = [A, D, E, F, G, H];
  labels.forEach(label => {
    group.add(label);
  });

  const lineAD = line(points.A, points.D);
  group.add(lineAD);

  const lineDE = line(points.D, points.E);
  group.add(lineDE);

  const lineAF = line(points.A, points.F);
  group.add(lineAF);

  const lineEG = line(points.E, points.G);
  group.add(lineEG);

  const lineDH = line(points.D, points.H);
  group.add(lineDH);

  const angleDEA = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      0,
      angle,
    ),
    greenSurfaceMaterial,
  );
  angleDEA.position.copy(points.E);
  angleDEA.rotation.y = Math.PI;
  group.add(angleDEA);

  const angleFAE = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      0,
      Math.acos(Math.sqrt(6)/3),
    ),
    yellowSurfaceMaterial,
  );
  angleFAE.position.copy(points.A);
  angleFAE.rotation.z = -Math.PI/2;
  group.add(angleFAE);

  const angleHDE = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      0,
      Math.acos(Math.sqrt(6)/3),
    ),
    yellowSurfaceMaterial,
  );
  angleHDE.position.copy(points.D);
  group.add(angleHDE);

  createScene("scene-angleFAE-angleHDE", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
    labels.forEach(label => {
      label.lookAt(camera.position);
    });
  });
}

angleFAE_angleHDE_Scene();

const angleAOH_Scene = () => {
  const group = tetrahedronWireframe();

  const { A, D, E, F, G, H, O } = createLabels();
  const labels = [A, D, E, F, G, H, O];
  labels.forEach(label => {
    group.add(label);
  });

  const lineAD = line(points.A, points.D);
  group.add(lineAD);

  const lineDE = line(points.D, points.E);
  group.add(lineDE);

  const lineAF = line(points.A, points.F);
  group.add(lineAF);

  const lineEG = line(points.E, points.G);
  group.add(lineEG);

  const lineDH = line(points.D, points.H);
  group.add(lineDH);

  const angleDEA = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      0,
      angle,
    ),
    greenSurfaceMaterial,
  );
  angleDEA.position.copy(points.E);
  angleDEA.rotation.y = Math.PI;
  group.add(angleDEA);

  const angleFAE = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      0,
      Math.acos(Math.sqrt(6)/3),
    ),
    yellowSurfaceMaterial,
  );
  angleFAE.position.copy(points.A);
  angleFAE.rotation.z = -Math.PI/2;
  group.add(angleFAE);

  const angleHDE = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      0,
      Math.acos(Math.sqrt(6)/3),
    ),
    yellowSurfaceMaterial,
  );
  angleHDE.position.copy(points.D);
  group.add(angleHDE);

  const angleAOH = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      Math.acos(Math.sqrt(6)/3),
      angle,
    ),
    greenSurfaceMaterial,
  );
  group.add(angleAOH);

  createScene("scene-angleAOH", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
    labels.forEach(label => {
      label.lookAt(camera.position);
    });
  });
}

angleAOH_Scene();

const cubeSize = Math.sqrt(2)/(6 * radius);
const cubePoints = {
  A: new Vector3(cubeSize, cubeSize, cubeSize),
  B: new Vector3(-cubeSize, -cubeSize, cubeSize),
  C: new Vector3(-cubeSize, cubeSize, -cubeSize),
  D: new Vector3(cubeSize, -cubeSize, -cubeSize),
  E: new Vector3(cubeSize, cubeSize, -cubeSize),
  F: new Vector3(-cubeSize, -cubeSize, -cubeSize),
  G: new Vector3(-cubeSize, cubeSize, cubeSize),
  H: new Vector3(cubeSize, -cubeSize, cubeSize),
  I: new Vector3(0, cubeSize, 0),
  O: new Vector3(0, 0, 0),
};
const cubeInscribedPoints = {
  A: cubePoints.A,
  B: cubePoints.B,
  C: cubePoints.C,
  D: cubePoints.D,
  O: cubePoints.O,
  I: cubePoints.I,
};

const createCubeInscribedTetrahedronWireframe = () => {
  const group = new Group();
  const lines = [
    line(cubeInscribedPoints.A, cubeInscribedPoints.B),
    line(cubeInscribedPoints.A, cubeInscribedPoints.C),
    line(cubeInscribedPoints.A, cubeInscribedPoints.D),
    line(cubeInscribedPoints.B, cubeInscribedPoints.C),
    line(cubeInscribedPoints.B, cubeInscribedPoints.D),
    line(cubeInscribedPoints.C, cubeInscribedPoints.D),
  ];
  lines.forEach(l => {
    group.add(l);
  });
  return group;
}

const createCubeWireframe = (group = new Group()) => {
  const lines = [
    line(cubePoints.A, cubePoints.E, greyLineMaterial),
    line(cubePoints.E, cubePoints.C, greyLineMaterial),
    line(cubePoints.A, cubePoints.G, greyLineMaterial),
    line(cubePoints.G, cubePoints.C, greyLineMaterial),
    line(cubePoints.B, cubePoints.F, greyLineMaterial),
    line(cubePoints.F, cubePoints.D, greyLineMaterial),
    line(cubePoints.B, cubePoints.H, greyLineMaterial),
    line(cubePoints.H, cubePoints.D, greyLineMaterial),
    line(cubePoints.A, cubePoints.H, greyLineMaterial),
    line(cubePoints.C, cubePoints.F, greyLineMaterial),
    line(cubePoints.G, cubePoints.B, greyLineMaterial),
    line(cubePoints.E, cubePoints.D, greyLineMaterial),
  ];
  lines.forEach(l => {
    group.add(l);
  });
  return group;
};

const createCubeInscibedLabels = () => ({
  A: label("A", cubeInscribedPoints.A.clone().multiplyScalar(1.1)),
  B: label("B", cubeInscribedPoints.B.clone().multiplyScalar(1.1)),
  C: label("C", cubeInscribedPoints.C.clone().multiplyScalar(1.1)),
  D: label("D", cubeInscribedPoints.D.clone().multiplyScalar(1.1)),
  O: label("O", cubeInscribedPoints.O.clone().add(new Vector3(0, -0.05, 0))),
  I: label("I", cubeInscribedPoints.I.clone().add(new Vector3(0, 0.05, 0))),
});

const cubeInscribedWireframeScene = () => {
  const group = createCubeInscribedTetrahedronWireframe();
  createCubeWireframe(group);

  const { A, B, C, D } = createCubeInscibedLabels();
  const labels = [A, B, C, D];
  labels.forEach(l => {
    group.add(l);
  });

  createScene("scene-cubeInscribedWireframe", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
    labels.forEach(l => {
      l.lookAt(camera.position);
    });
  });
}

cubeInscribedWireframeScene();

const cubeInscribedWireframe_angleAOC_Scene = () => {
  const group = createCubeInscribedTetrahedronWireframe();
  createCubeWireframe(group);

  const lineOA = line(cubeInscribedPoints.O, cubeInscribedPoints.A);
  group.add(lineOA);
  const lineOC = line(cubeInscribedPoints.O, cubeInscribedPoints.C);
  group.add(lineOC);

  const angleAOC = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      (Math.PI - 2 * angle) / 2,
      2 * angle,
    ),
    orangeSurfaceMaterial,
  );
  angleAOC.rotation.y = -(Math.PI/4);

  group.add(angleAOC);

  const { A, B, C, D, O } = createCubeInscibedLabels();
  const labels = [A, B, C, D, O];
  labels.forEach(l => {
    group.add(l);
  });

  createScene("scene-cubeInscribedWireframe-angleAOC", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
    labels.forEach(l => {
      l.lookAt(camera.position);
    });
  });
}

cubeInscribedWireframe_angleAOC_Scene();

const cubeInscribedWireframe_lineOI_Scene = () => {
  const group = createCubeInscribedTetrahedronWireframe();
  createCubeWireframe(group);

  const lineOA = line(cubeInscribedPoints.O, cubeInscribedPoints.A);
  group.add(lineOA);
  const lineOC = line(cubeInscribedPoints.O, cubeInscribedPoints.C);
  group.add(lineOC);
  const lineOI = line(cubeInscribedPoints.O, cubeInscribedPoints.I);
  group.add(lineOI);

  const { A, C, O, I } = createCubeInscibedLabels();
  const labels = [A, C, O, I];
  labels.forEach(l => {
    group.add(l);
  });

  createScene("scene-cubeInscribedWireframe-lineOI", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
    labels.forEach(l => {
      l.lookAt(camera.position);
    });
  });
}

cubeInscribedWireframe_lineOI_Scene();

const cubeInscribedWireframe_angleAOI_Scene = () => {
  const group = createCubeInscribedTetrahedronWireframe();
  createCubeWireframe(group);

  const lineOA = line(cubeInscribedPoints.O, cubeInscribedPoints.A);
  group.add(lineOA);
  const lineOC = line(cubeInscribedPoints.O, cubeInscribedPoints.C);
  group.add(lineOC);
  const lineOI = line(cubeInscribedPoints.O, cubeInscribedPoints.I);
  group.add(lineOI);

  const angleAOI = new Mesh(
    new CircleGeometry(
      radius / 3,
      32,
      (Math.PI - 2 * angle) / 2,
      angle,
    ),
    greenSurfaceMaterial,
  );
  angleAOI.rotation.y = -(Math.PI/4);
  group.add(angleAOI);

  const { A, C, O, I } = createCubeInscibedLabels();
  const labels = [A, C, O, I];
  labels.forEach(l => {
    group.add(l);
  });

  createScene("scene-cubeInscribedWireframe-angleAOI", group, ({ camera, paused, time }) => {
    rotating({ group, paused, time });
    labels.forEach(l => {
      l.lookAt(camera.position);
    });
  });
}

cubeInscribedWireframe_angleAOI_Scene();
