// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import chisel3._

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.diplomacy._

import dsptools._
import dsptools.numbers._

import scala.util.Random

class Scaler2RGBTester[T <: Data: Real : BinaryRepresentation]
(
  dut: Scaler2RGB[T] with Scaler2RGBStandaloneBlock[T],
  params: Scaler2RGBParams[T],
  silentFail: Boolean = false
) extends DspTester(dut.module) with AXI4StreamModel{
    
  def seqToString(c: Seq[Double]): String = "[" + c.mkString(", ") + "]"

  val mod = dut.module

  val dataSize = 10000 // Data size

  // Data and expected data
  var expectedData = Seq[Int]()
  var data = Seq[Int]()

  // data
  for (i <- 0 until dataSize) {
    data = data :+ Random.nextInt((1 << params.proto.getWidth) - 1)
    expectedData = expectedData :+ Utils.scaler2rgb(data(i), params.proto.getWidth)
  }


  step(1)
  var inValid = 0
  while(!expectedData.isEmpty) {

    inValid = Random.nextInt(2)
    poke(dut.out.ready, Random.nextInt(2))
    if (!data.isEmpty) {
      poke(dut.in.valid, inValid)
      if (peek(dut.in.ready) == true && peek(dut.in.valid) == true) {
        poke(dut.in.bits.data, data.head)
        data = data.tail
      }
    }
    if (peek(dut.out.ready) == true && peek(dut.out.valid) == true) {
      expect(dut.out.bits.data, expectedData.head)
      expectedData = expectedData.tail
    }
    step(1)
  }
  stepToCompletion(maxCycles = 10*dataSize, silentFail = silentFail)
}

