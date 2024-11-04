// let qtree;

// let count;
// function setup() {
//   createCanvas(400, 400);

//   // cen, ter, width, height;
//   let boundary = new Rectangle(200,200,200,200);
//   qtree = new QuadTree(boundary, 4);
//   // console.log(qt);

//   for (let i = 0; i < 300; i++) {
//     // let p = new Point(random(width), random(height));
//     let x = randomGaussian(width / 2, width / 8);
//     let y = randomGaussian(height / 2, height / 8);
//     let p = new Point(x, y);
//     qtree.insert(p);
//   }
// }

// function draw(){
//   background(0);
//   qtree.show();

//   stroke(0, 255, 0);
//   rectMode(CENTER);
//   count = 0;
//   let range = new Rectangle(mouseX, mouseY, 25, 25);
//   rect(range.x, range.y, range.w * 2, range.h * 2);
//   let points = qtree.query(range);
//   console.log(points);
//   for (let p of points){
//     strokeWeight(4);
//     point(p.x, p.y);
//   }
//   console.log(count);
//  }


// // function draw() {
// //   if (mouseIsPressed) {
// //     let m = new Point(mouseX, mouseY);
// //     qtree.insert(m);
// //   }
// //   background(0);
// //   qtree.show();

// // }

// Pt.3

let particles = [];
let count;
function setup() {
  createCanvas(600, 400);
  for (let i = 0; i < 1000; i++){
    particles[i] = new Particle(random(width), random(height));
  }
}

function draw(){
  background(0);
  let boundary = new Rectangle(300, 200, 600, 400);
  count = 0;
  qtree = new QuadTree(boundary, 4);
  for (let p of particles)
  {
    let point = new Point(p.x, p.y, p);
    qtree.insert(point);
    p.move();
    p.render();
    p.setHighlight(false);
  }

  for (let p of particles){
    let range = new Circle(p.x, p.y, p.r * 2);
    let points = qtree.query(range);
    // for (let point of points) {
    //   let other = point.userData;
    //   if (p !== other && p.intersects(other)){
    //     p.setHighlight(true);
    //   }
    // }
  }
}
