// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import chisel3._
import chisel3.experimental._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import org.scalatest.FlatSpec
import org.scalatest.Matchers

//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// SPEC
//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
class InterpolationSpec extends FlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty

  //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // Interpolation
  //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  it should "test Interpolation functionality" in {
    val params: InterpolationParams[FixedPoint] = InterpolationParams(
      proto      = FixedPoint(16.W, 14.BP),
      scalerSize = 7,
      zoh        = ZOHParams(
        width = 16,
        size  = 1
      )
    )
    val lazyDut = LazyModule(new AXI4InterpolationBlock(params, 3) with AXI4InterpolationStandaloneBlock)
    dsptools.Driver.execute(() => lazyDut.module, Array("--backend-name", "verilator", "--target-dir", "test_run_dir/Oscillator/interpolator", "--top-name", "Interpolation")) {
      c => new InterpolationTester(lazyDut, params, true)
    } should be (true)
  }
}