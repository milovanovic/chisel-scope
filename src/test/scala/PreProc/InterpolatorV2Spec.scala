// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import chisel3._
import chisel3.experimental._

import dsptools.numbers._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// SPEC
//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
class InterpolatorV2Spec extends AnyFlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty

  // //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // // Zero-Order Hold
  // //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // it should "test Zero-Order Hold functionality" in {
  //   val scalerSize = 3
  //   val params: ZeroOrderHoldParams[FixedPoint] = ZeroOrderHoldParams(
  //     proto = FixedPoint(16.W, 14.BP),
  //     size  = 4
  //   )
  //   dsptools.Driver.execute(() => new ZeroOrderHold(params, scalerSize, 4), Array("--backend-name", "verilator", "--target-dir", "test_run_dir/Oscillator/zero_order_hold", "--top-name", "ZeroOrderHold")) {
  //     c => new ZeroOrderHold_Tester(c, params, scalerSize, true)
  //   } should be (true)
  // }

  //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // InterpolatorV2
  //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  it should "test InterpolatorV2 functionality" in {
    val params: InterpolatorV2Params[FixedPoint] = InterpolatorV2Params(
      proto      = FixedPoint(16.W, 14.BP),
      scalerSize = 7,
      zoh        = ZeroOrderHoldParams(
        proto = FixedPoint(16.W, 14.BP),
        size  = 1
      )
    )
    val lazyDut = LazyModule(new AXI4InterpolatorV2Block(params, 3) with AXI4InterpolatorV2StandaloneBlock)
    dsptools.Driver.execute(() => lazyDut.module, Array("--backend-name", "verilator", "--target-dir", "test_run_dir/Oscillator/interpolator", "--top-name", "InterpolatorV2")) {
      c => new InterpolatorV2_Tester(lazyDut, params, true)
    } should be (true)
  }

  
}