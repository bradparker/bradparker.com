import {
  Color,
  Fog,
  Group,
  LineBasicMaterial,
  LineSegments,
  PerspectiveCamera,
  PolyhedronGeometry,
  Scene,
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
camera.position.y = 3;
camera.position.z = 5;

const renderer = new WebGLRenderer({ antialias: true });
renderer.setPixelRatio(window.devicePixelRatio);
renderer.setSize(mount.clientWidth, mount.clientHeight);
renderer.setAnimationLoop(animate);
mount.appendChild(renderer.domElement);

const controls = new OrbitControls( camera, renderer.domElement );
controls.enablePan = false;
controls.enableZoom = false;

const fog = new Fog(0xFFFFFF, 3, 8);

const polyhedronGeo = new PolyhedronGeometry(
  [
     0,               Math.sqrt(6)/4,   0,
     Math.sqrt(3)/3, -Math.sqrt(6)/12,  0,
    -Math.sqrt(3)/6, -Math.sqrt(6)/12, -1/2,
    -Math.sqrt(3)/6, -Math.sqrt(6)/12,  1/2
  ],
  [
    2, 1, 0,
    0, 3, 2,
    1, 3, 0,
    2, 3, 1,
  ],
  1,
  0,
);

const wireframeGeo = new WireframeGeometry(polyhedronGeo);
const wireframe = new LineSegments(
  wireframeGeo,
  new LineBasicMaterial({
    color: 0x000000,
    fog: true,
  }),
);

const tetrahedron = new Group();
tetrahedron.add(wireframe);
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
