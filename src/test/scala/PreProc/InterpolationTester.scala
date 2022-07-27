// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._

import dsptools._

import scala.util.Random

// TODO: FIX test to work with signed values, scale = 16 instead of 15

//  Interpolation
// |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾|
// |                      ___   u[n]   _______________   v[n]   ___      z[n]      ___               |
// |   x[n] ---- * ----➛ | + | -----➛ |zero-order hold| -----➛ | + | ---- * ----➛ | x | ----➛ y[n]   |
// |             |        ‾↑‾          ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾          ‾↑‾       |        ‾↑‾               |
// |             |        _|_                                    |        |         |                |
// |             |       |-1 |                                   |        |         |                |
// |             |        ‾↑‾                                    |        |         |                |
// |             |        _|_                                   _|_       |        _|_               |
// |              -----➛ |1/z|                                 |1/z| <----        |1/L|              |
// |                      ‾‾‾                                   ‾‾‾                ‾‾‾               |
// |_________________________________________________________________________________________________|

class InterpolationTester[T <: chisel3.Data]
(
  dut: AXI4InterpolationBlock[T] with AXI4InterpolationStandaloneBlock,
  params: InterpolationParams[T],
  dataSize : Int,
  tol: Int,
  scale: Int,
  silentFail: Boolean = false
) extends DspTester(dut.module) with AXI4StreamModel{  
    
  def seqToString(c: Seq[Double]): String = "[" + c.mkString(", ") + "]"

  val mod = dut.module

  val iterations = 1 // number of iterations for testing
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
    var dataIN = Seq[Int]()

    // data
    for (i <- 0 until dataSize) {
      data  = data :+ (Random.nextInt(1<<15))
    }
    for (i <- 0 until dataSize) {
      dataIN  = dataIN :+ data(i)
      dataIN  = dataIN :+ data(i)
    }
    expectedData = Utils.interpolatedSeq(data, hold << scale)

    poke(mod.scaler, scale)
    step(100)

    // init counters
    counter = 0
    expectedCounter = 0
    // Valid 2 clk before data
    poke(dut.in.valid, 1)
    step(2)

    while (expectedCounter < expectedData.length) {
      poke(dut.out.ready, 1)

      // poke input data
      if (counter < dataIN.length) {
        if (peek(dut.in.ready) && peek(dut.in.valid)) {
          poke(dut.in.bits.data, dataIN(counter))
          counter = counter + 1
        }
      }
      else { poke(dut.in.valid, 0) }
      // if out.ready and out.valid, read output data
      if (peek(dut.out.valid) && peek(dut.out.ready)){
        outputData = outputData :+ peek(dut.out.bits.data).toShort.toDouble
        if (expectedCounter < expectedData.length) {
          expectedCounter = expectedCounter + 1
        }
      }
      step(1)
    }

    outputData = outputData.drop(1)
    Utils.checkDataError(expected = expectedData, received = outputData.map(m => m.toInt), tolerance = tol)
    Utils.plot_data(dataIN, plotName = "InputData", fileName = "InputData.pdf")
    Utils.plot_data_double(outputData, plotName = "outputData", fileName = "outputData.pdf")
    Utils.plot_data(expectedData, plotName = "expectedData", fileName = "expectedData.pdf")
  }
}

