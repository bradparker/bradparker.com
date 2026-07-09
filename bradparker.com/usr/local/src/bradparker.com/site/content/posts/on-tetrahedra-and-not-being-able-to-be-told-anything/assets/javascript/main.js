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
  PolyhedronGeometry,
  Scene,
  ShapeGeometry,
  Vector3,
  WebGLRenderer,
  WireframeGeometry,
} from "three";
import { OrbitControls } from "three/addons/controls/OrbitControls.js";
import { Font } from "three/addons/loaders/FontLoader.js";
import helvetikerRegularFont from "../fonts/helvetiker_regular.typeface.json" with { type: "json" };

const mount = document.getElementById("scene");

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
renderer.setAnimationLoop(animate);
mount.appendChild(renderer.domElement);

const controls = new OrbitControls(camera, renderer.domElement);
controls.enablePan = false;
controls.enableZoom = false;

const fog = new Fog(0xFFFFFF, 2.5, 5);

const radius = Math.sqrt(6)/4;
const vertexVectors = [
  new Vector3( 0,               Math.sqrt(6)/4,   0),
  new Vector3( Math.sqrt(3)/3, -Math.sqrt(6)/12,  0),
  new Vector3(-Math.sqrt(3)/6, -Math.sqrt(6)/12, -1/2),
  new Vector3(-Math.sqrt(3)/6, -Math.sqrt(6)/12,  1/2)
];

const lineMaterial = new LineBasicMaterial({
  color: 0x000000,
  fog: true,
});

const tetrahedronGeo = new PolyhedronGeometry(
  vertexVectors.flatMap(v => v.toArray()),
  [
    2, 1, 0,
    0, 3, 2,
    1, 3, 0,
    2, 3, 1,
  ],
  radius,
  0,
);
const wireframeGeo = new WireframeGeometry(tetrahedronGeo);
const wireframe = new LineSegments(
  wireframeGeo,
  lineMaterial,
);

const vectorLine = (vector, material = lineMaterial) =>
  new Line(
    new BufferGeometry().setFromPoints([
      new Vector3(0, 0, 0),
      vector,
    ]),
    material,
  );
const vertexVectorLines = vertexVectors.map(v => vectorLine(v));

const triangle = new Mesh(
  new BufferGeometry().setFromPoints([
    new Vector3(0, Math.sqrt(6)/4, 0),
    new Vector3(Math.sqrt(3)/3, -Math.sqrt(6)/12, 0),
    new Vector3(-Math.sqrt(3)/6, -Math.sqrt(6)/12, 0),
  ]),
  new MeshBasicMaterial({
    color: 0x137752,
    transparent: true,
    opacity: 0.25,
    side: DoubleSide,
    fog: false,
  }),
);

const angle = new Mesh(
  new CircleGeometry(
    radius / 3, 32,
    -2 * Math.acos(Math.sqrt(3)/3) + Math.PI/2,
    2 * Math.acos(Math.sqrt(3)/3),
  ),
  new MeshBasicMaterial({
    color: 0xFF6300,
    transparent: true,
    opacity: 0.25,
    side: DoubleSide,
    fog: false,
  })
);

const font = new Font(helvetikerRegularFont);

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
    new MeshBasicMaterial({
      color: 0x000000,
      side: DoubleSide,
      fog: false,
    })
  );
  mesh.position.copy(position);
  return mesh;
}

const vertexLabels = [
  label(
    "A",
    new Vector3(0, Math.sqrt(6)/4, 0)
      .multiplyScalar(1.1),
  ),
  label(
    "B",
    new Vector3(-Math.sqrt(3)/6, -Math.sqrt(6)/12, -1/2)
      .multiplyScalar(1.1),
  ),
  label(
    "C",
    new Vector3(-Math.sqrt(3)/6, -Math.sqrt(6)/12, 1/2)
      .multiplyScalar(1.1),
  ),
];

const tetrahedron = new Group();
tetrahedron.add(wireframe);
vertexVectorLines.forEach(line => {
  tetrahedron.add(line);
});
vertexLabels.forEach(label => {
  tetrahedron.add(label);
});
// tetrahedron.add(triangle);
tetrahedron.add(angle);
scene.add(tetrahedron);

scene.fog = fog;

function animate(time) {
  tetrahedron.rotation.y = time / 2000;
  vertexLabels.forEach(label => {
    label.lookAt(camera.position);
  });

  renderer.render(scene, camera);
}

window.addEventListener(
  "resize",
  function () {
    camera.aspect = mount.clientWidth / mount.clientHeight;
    camera.updateProjectionMatrix();

    renderer.setSize(mount.clientWidth, mount.clientHeight);
  },
  false,
);
