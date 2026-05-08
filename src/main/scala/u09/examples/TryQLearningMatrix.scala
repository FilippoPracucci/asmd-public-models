package u09.examples

import u09.model.QMatrix

object TryQMatrix extends App :

  import u09.model.QMatrix.Move.*
  import u09.model.QMatrix.*

  val rl: QMatrix.Facade = Facade(
    width = 5,
    height = 5,
    initial = (0,0),
    terminal = {case _=>false},
    reward = { case ((1,0),_) => 10; case ((3,0),_) => 5 },
    jumps = { case ((1,0),_) => (1,4); case ((3,0),_) => (3,2) },
    obstacles = Seq((0, 1), (2, 0), (3, 1)),
    gamma = 0.9, // discount rate: how much a reward now is better than in the future
    alpha = 0.5, // how much consider old value respect to the new one
    epsilon = 0.3, // sort of noise, to add randomness to the movement
    v0 = 1
  )

  val q0 = rl.qFunction
  println(rl.show(q0.vFunction,"%2.2f"))
  val q1 = rl.makeLearningInstance().learn(10000,100,q0)
  println(rl.show(q1.vFunction,"%2.2f"))
  println(rl.show(s => q1.bestPolicy(s).toString,"%7s"))