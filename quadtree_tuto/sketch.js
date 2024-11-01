let qtree;

function setup() {
  createCanvas(400, 400);

  // cen, ter, width, height;
  let boundary = new Rectangle(200,200,200,200);
  qtree = new QuadTree(boundary, 4);
  // console.log(qt);

  // for (let i = 0; i < 5; i++) {
  //   let p = new Point(random(width), random(height));
  //   qt.insert(p);
  // }


  console.log(qtree);
 }


 function draw() {
  if (mouseIsPressed) {
    let m = new Point(mouseX, mouseY);
    qtree.insert(m);
  }
  background(0);
  qtree.show()
 }
