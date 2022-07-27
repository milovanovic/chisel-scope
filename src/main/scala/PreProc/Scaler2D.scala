// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._


// Scaler2D Bundle
class Scaler2DIO extends Bundle {
    val o_scalerData = Output(UInt(5.W))
    val o_scalerAxis = Output(UInt(10.W))
}

abstract class Scaler2D(beatBytes: Int) extends LazyModule()(Parameters.empty) with HasCSR {

    lazy val module = new LazyModuleImp(this) {
        // IO
        val io = IO(new Scaler2DIO)

        // Control registers
        val r_scaleData = RegInit(9.U(5.W))
        val r_scaleAxis = RegInit(0.U(10.W))
        io.o_scalerData := r_scaleData
        io.o_scalerAxis := r_scaleAxis

        // Define register fields
        val fields = Seq(RegField(r_scaleData.getWidth, r_scaleData, RegFieldDesc(name = "r_scaleData", desc = "Register used to set the data scaler")),
                         RegField(r_scaleAxis.getWidth, r_scaleAxis, RegFieldDesc(name = "r_scaleAxis", desc = "Register used to set the axis scaler")))
                         
        // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)
    }
}

class AXI4Scaler2DBlock(address: AddressSet, beatBytes: Int = 4)(implicit p: Parameters) extends Scaler2D(beatBytes) {
  val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
  override def regmap(mapping: (Int, Seq[RegField])*): Unit = mem.get.regmap(mapping:_*)
}


trait AXI4Scaler2DPins extends AXI4Scaler2DBlock {
    def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
    val ioMem = mem.map { m => {
        val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

        m :=
        BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
        ioMemNode

        val ioMem = InModuleBody { ioMemNode.makeIO() }
        ioMem
    }}
}


object Scaler2DApp extends App
{
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AXI4Scaler2DBlock(AddressSet(0x00, 0xF), beatBytes = 4) with AXI4Scaler2DPins)
  (new ChiselStage).execute(Array("--target-dir", "verilog/Scaler2D"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}