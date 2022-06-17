// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// SPEC
//-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
class Scaler2RGBBlockSpec extends AnyFlatSpec with Matchers {
  implicit val p: Parameters = Parameters.empty

  //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  // Scaler2RGBBlock
  //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  it should "test Scaler2RGBBlock functionality" in {
    val beatBytes = 2
    val lazyDut = LazyModule(new Scaler2RGBBlock(beatBytes) with Scaler2RGBBlockPins)
    dsptools.Driver.execute(() => lazyDut.module, Array("--backend-name", "verilator", "--target-dir", "test_run_dir/Scaler2RGBBlock", "--top-name", "Scaler2RGBBlock")) {
      c => new Scaler2RGBBlockTester(lazyDut, beatBytes, true)
    } should be (true)
  }
}