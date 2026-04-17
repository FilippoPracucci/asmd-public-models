package scala.u07.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

class StochasticReadersWriters extends AnyFunSuite:

  import scala.u07.examples.StochasticReadersWriters.*
  import u07.utils.MSet
  import java.util.Random

  test("Stochastic readers and writers"):
    toCTMC(spn).newSimulationTrace(MSet(P1, P1, P1, P5), new Random)
      .take(100)
      .foreach(m =>
        m.state.matches(MSet(P7, P7)) should be (false)
        m.state.matches(MSet(P7, P7)) should be (false)
      )
