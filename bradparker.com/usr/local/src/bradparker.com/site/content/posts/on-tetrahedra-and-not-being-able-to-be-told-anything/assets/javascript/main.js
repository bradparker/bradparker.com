import {
  TetrahedronGeometry,
  Scene,
  PerspectiveCamera,
  WebGLRenderer,
  MeshPhongMaterial,
  Mesh,
  Color,
  PointLight,
  AmbientLight,
  DoubleSide,
} from "/assets/javascript/three.module.js";

const mount = document.getElementById("scene");

const scene = new Scene();
scene.background = new Color(0xFFFFFF);

const camera = new PerspectiveCamera(
  25,
  mount.clientWidth / mount.clientHeight,
  0.1,
  50,
);
camera.position.z = 5;

const renderer = new WebGLRenderer({ antialias: true });
renderer.setPixelRatio(window.devicePixelRatio);
renderer.setSize(mount.clientWidth, mount.clientHeight);
renderer.setAnimationLoop(animate);
mount.appendChild(renderer.domElement);

const pointLight = new PointLight(0xFFFFFF, 200);
pointLight.position.set(0, 10, 5);
scene.add(pointLight);

const ambientLight = new AmbientLight(0xFFFFFF, 1);
scene.add(ambientLight);

const geometry = new TetrahedronGeometry();
const material = new MeshPhongMaterial({
  color: 0x137752,
  opacity: 0.50,
  transparent: true,
  depthWrite: false,
  side: DoubleSide,
});
const tetrahedron = new Mesh(geometry, material);

scene.add(tetrahedron);

function animate(time) {
  tetrahedron.rotation.x = time / 2000;
  tetrahedron.rotation.y = time / 1000;

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
