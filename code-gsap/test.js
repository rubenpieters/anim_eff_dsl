/*const rightThenDown = new TimelineMax()
  .add(new TweenMax("#box1", 1, { x: 50 }))
  .add(new TweenMax("#box1", 1, { y: 50 }));

const both = new TimelineMax()
  .add(new TweenMax("#box1", 1, { x: 50 }), 0)
  .add(new TweenMax("#box2", 1, { x: 50 }), 0);*/

function sequential(tl1, tl2) {
  return new TimelineMax({ paused: true })
    .add(tl1.play())
    .add(tl2.play());
}

function parallel(tl1, tl2) {
  return new TimelineMax({ paused: true })
    .add(tl1.play(), 0)
    .add(tl2.play(), 0);
}

const bothTo50 = parallel(
  new TweenMax("#box1", 1, { x: 50 }),
  new TweenMax("#box2", 1, { x: 50 }));

const bothTo100 = parallel(
  new TweenMax("#box1", 1, { x: 50 }),
  new TweenMax("#box2", 1, { x: 50 }));

const bothTo0 = parallel(
  new TweenMax("#box1", 1, { x: 0 }),
  new TweenMax("#box2", 1, { x: 0 }));

const bothAnimation = sequential(bothTo50, bothTo0);

//bothAnimation.play();
console.log(bothAnimation.totalDuration());

let cond = true;

const conditionalAnim = new TimelineMax({ paused: true })
  .add(bothTo50.play())
  .add(() => { if (cond) { bothTo0.play() } else { bothTo100.play() } });

conditionalAnim.play();
console.log(conditionalAnim.totalDuration());

/*const cond = true;

    const both = new TimelineMax({ paused: true })
      .add(new TweenMax("#box1", 1, { x: 50 }), 0)
      .add(new TweenMax("#box2", 1, { x: 50 }), 0);

    const moveBackBox1 = new TimelineMax({ paused: true })
      .add(new TweenMax("#box1", 1, { x: 0 }), 0)
      .add(new TweenMax("#box2", 1, { x: 0 }), 0);

function sequential(tl1, tl2) {
  return new TimelineMax({ paused: true })
    .add(tl1.play())
    .add(() => { if (cond) { tl2.play() } else { tl2.play() }})
    .add(both.play());
}


    const bothThenBack = sequential(both, moveBackBox1).play();

console.log(bothThenBack.totalDuration());*/


    /*var tl1 = new TimelineMax();
    tl1
      .to("#box1", 1, {x: 200}, 0)
      .to("#box1", 1, {y: 200}, 0)
      ;
    var tl2 = new TimelineMax({ paused: true });
    tl2
      .to("#box2", 1, {x: 200}, 0)
      .to("#box2", 1, {y: "+=200"}, 0)
    ;
    tl1.eventCallback("onComplete", function() {
      tl2.play(0);
    });
    tl2.eventCallback("onComplete", function() {
      tl1.reverse();
    });
   console.log("duration: " + tl1.totalDuration());*/

   /*var tl1 = new TimelineMax();
   tl1
     .staggerTo(["#box1", "#box2"], 1, {x: 200})
     .to("#box3", 1, {x: 200})
     ;
   console.log("duration: " + tl1.totalDuration());*/
