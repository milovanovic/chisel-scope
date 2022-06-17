// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._

import dsptools._
import dsptools.numbers._

import scala.util.Random

// TODO: FIX test to work with signed values, scale = 16 instead of 15

//  InterpolatorV2
// |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾|
// |                      ___   u[n]   _______________   v[n]   ___      z[n]      ___               |
// |   x[n] ---- * ----➛ | + | -----➛ |zero-order hold| -----➛ | + | ---- * ----➛ | + | ----➛ y[n]   |
// |             |        ‾↑‾          ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾          ‾↑‾       |        ‾↑‾               |
// |             |        _|_                                    |        |         |                |
// |             |       |-1 |                                   |        |         |                |
// |             |        ‾↑‾                                    |        |         |                |
// |             |        _|_                                   _|_       |        _|_               |
// |              -----➛ |1/z|                                 |1/z| <----        |1/L|              |
// |                      ‾‾‾                                   ‾‾‾                ‾‾‾               |
// |_________________________________________________________________________________________________|

class InterpolatorV2_Tester[T <: chisel3.Data]
(
  dut: AXI4InterpolatorV2Block[T] with AXI4InterpolatorV2StandaloneBlock,
  params: InterpolatorV2Params[T],
  silentFail: Boolean = false
) extends DspTester(dut.module) with AXI4StreamModel{  
    
  def seqToString(c: Seq[Double]): String = "[" + c.mkString(", ") + "]"

  val mod = dut.module

  val dataSize = 10 // Data size
  val tol   = 1      // tolerance
  val scale = 2     // scale factor
  val iterations = 1//params.scalerSize // number of iterations for testing
  val hold = params.zoh.size  // zoh default hold time

  
  // Expected data
  var expectedData = Seq[Int]()

  // counters
  var counter = 0
  var expectedCounter = 0

  for (k <- 0 until iterations) {
    // print iteration
    println(s"INTERATION: ${k}\n\n\n\n")

    var outputData = Seq[Double]()
    var data = Seq[Int]()

    // data
    for (i <- 0 until dataSize) {
      data  = data :+ Random.nextInt(1<<15)
    }
    expectedData = Utils.interpolatedSeq(data, hold << scale)

    poke(mod.scaler, scale)
    step(1)
    poke(mod.loadReg, 1)
    step(2)
    poke(mod.loadReg, 0)
    step(3)

    // init counters
    counter = 0
    expectedCounter = 0

    while (expectedCounter < expectedData.length) {
      // println(s"counter: ${counter}")
      // println(s"expcounter: ${expectedCounter}")

      poke(dut.out.ready, Random.nextInt(2))

      // poke input data
      if (counter < data.length) {
        poke(dut.in.valid, 1)
        // If in.ready and in.valid, poke input data
        if (peek(dut.in.ready) && peek(dut.in.valid)) {
          poke(dut.in.bits.data, data(counter))
          counter = counter + 1
        }
      }
      else {
        poke(dut.in.valid, 0)
      }

      // if out.ready and out.valid, read output data
      if (peek(dut.out.valid) && peek(dut.out.ready)){
        outputData = outputData :+ peek(dut.out.bits.data).toShort.toDouble
        params.proto match {
          case dspR: DspReal => realTolDecPts.withValue(tol) { expect(dut.out.bits.data, expectedData(expectedCounter)) }
          case _ => fixTolLSBs.withValue(tol) { expect(dut.out.bits.data,  expectedData(expectedCounter).toShort.toDouble) }
        }
        // require(scala.math.abs(peek(dut.out.bits.data).toShort.toDouble - expectedData(expectedCounter)) <= tol, s"Tolerance is not satisfied, expected: ${expectedData(expectedCounter)}, got ${peek(dut.out.bits.data).toShort}")
        if (expectedCounter < expectedData.length) {
          expectedCounter = expectedCounter + 1
        }
      }
      step(1)
    }

    for (i <- 0 until outputData.length) {
      println(s"dataOut: ${outputData(i)}")
    }
    for (i <- 0 until data.length) {
      println(s"data: ${data(i)}")
    }
  }
}

