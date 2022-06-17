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
class Scaler2RGBSpec extends AnyFlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty

  //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // Scaler2RGB
  //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  it should "test Scaler2RGB functionality" in {
    val params = Scaler2RGBParams[FixedPoint] (
      proto = FixedPoint(16.W, 14.BP)
    )
    val lazyDut = LazyModule(new Scaler2RGB(params) with Scaler2RGBStandaloneBlock[FixedPoint])
    dsptools.Driver.execute(() => lazyDut.module, Array("--backend-name", "verilator", "--target-dir", "test_run_dir/Scaler2RGB", "--top-name", "Scaler2RGB")) {
      c => new Scaler2RGBTester(lazyDut, params, true)
    } should be (true)
  }


}