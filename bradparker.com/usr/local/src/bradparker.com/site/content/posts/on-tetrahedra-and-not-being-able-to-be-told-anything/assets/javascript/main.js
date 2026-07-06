import {
  BufferGeometry,
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
  Vector3,
  WebGLRenderer,
  WireframeGeometry,
} from "three";
import { OrbitControls } from "three/addons/controls/OrbitControls.js";

const mount = document.getElementById("scene");

const scene = new Scene();
scene.background = new Color(0xFFFFFF);

const camera = new PerspectiveCamera(
  25,
  mount.clientWidth / mount.clientHeight,
  0.1,
  50,
);
camera.position.y = 2;
camera.position.z = 2.5;

const renderer = new WebGLRenderer({ antialias: true });
renderer.setPixelRatio(window.devicePixelRatio);
renderer.setSize(mount.clientWidth, mount.clientHeight);
renderer.setAnimationLoop(animate);
mount.appendChild(renderer.domElement);

const controls = new OrbitControls( camera, renderer.domElement );
controls.enablePan = false;
controls.enableZoom = false;

const fog = new Fog(0xFFFFFF, 1.75, 4.5);

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

const polyhedronGeo = new PolyhedronGeometry(
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
const wireframeGeo = new WireframeGeometry(polyhedronGeo);
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

const tetrahedron = new Group();
tetrahedron.add(wireframe);
vertexVectorLines.forEach(line => {
  tetrahedron.add(line);
});
tetrahedron.add(triangle);
scene.add(tetrahedron);

scene.fog = fog;

function animate(time) {
  tetrahedron.rotation.y = time / 2000;

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
