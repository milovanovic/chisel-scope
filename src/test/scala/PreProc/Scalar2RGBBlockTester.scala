// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import freechips.rocketchip.diplomacy._
import dsptools._
import scala.util.Random

class Scaler2RGBBlockTester
(
  dut: Scaler2RGBBlock with Scaler2RGBBlockPins,
  beatBytes: Int,
  silentFail: Boolean = false
) extends DspTester(dut.module) {
    
  def seqToString(c: Seq[Double]): String = "[" + c.mkString(", ") + "]"

  val mod = dut.module

  val dataSize = 10 // Data size
  val maxIterations = 10 // Data size

  // for loop for max value
  for (j <- 0 until maxIterations) {
    step(100)
    // Data and expected data
    var expectedData = Seq[Int]()
    var data = Seq[Int]()

    // data
    for (i <- 0 until dataSize) {
      data = data :+ Random.nextInt((1 << (8*beatBytes - j)) - 1)
      expectedData = expectedData :+ Utils.scaler2rgb(data(i), 8*beatBytes - j)
    }

    // max value
    println(s"max value: ${(8*beatBytes - j)}")
    poke(dut.ioBlock.i_max, (8*beatBytes - j))

    step(2)
    while(!expectedData.isEmpty) {

      if (!data.isEmpty) {
        poke(dut.ioBlock.i_data, data.head)
        data = data.tail
      }

      expect(dut.ioBlock.o_data, expectedData.head)
      expectedData = expectedData.tail
      step(1)
    }
  }
  // stepToCompletion(maxCycles = 10*dataSize, silentFail = silentFail)
}

