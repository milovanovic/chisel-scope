package hdmi.preproc

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

// Decimator parameters
case class DecimatorParams(
  skipSize : Int,
) { require(skipSize >= 1, "skipSize must be greater or equal to 1") }

abstract class Decimator [D, U, E, O, B <: Data] (params: DecimatorParams, beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR{

  val streamNode = AXI4StreamIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val (in, _)  = streamNode.in(0)
    val (out, _) = streamNode.out(0)

    // Additional IOs
    val loadReg = IO(Input(Bool()))

    // Signal definitions
    val read  = in.fire()
    val write = out.fire()

    val counter  = RegInit(0.U((beatBytes*8).W))
    val skipSize = RegInit((params.skipSize-1).U((beatBytes*8).W))
    val skipSizeReg = RegInit((params.skipSize-1).U((beatBytes*8).W))

    // Define register fields
    val fields = Seq(RegField(beatBytes*8, skipSizeReg, RegFieldDesc(name = "skipSize", desc = "Register used to set the number of data to skip")))
                 
    // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
    regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)

    // pass input to output
    out.bits := in.bits

    // When loadReg is active, load register and reset counter
    when (loadReg) {
      skipSize  := skipSizeReg
      counter   := 0.U
      out.valid := false.B
      in.ready  := false.B
    }
    .otherwise {
      when (skipSize === 0.U) {
        out.valid := in.valid
        in.ready  := out.ready
      }
      .otherwise {
        when(counter === 0.U) {
          out.valid := in.valid
          in.ready  := out.ready
        }
        .otherwise{
          out.valid := false.B
          in.ready  := true.B
        }
        when(read) {
          when(counter < skipSize - 1.U) {
            counter := counter + 1.U
          }
          .otherwise {
           counter := 0.U
          }
        }
      }
    }
  }
}

class AXI4DecimatorBlock(params: DecimatorParams, address: AddressSet, beatBytes: Int)(implicit p: Parameters) extends Decimator[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
}

object DecimatorApp extends App
{
  val params = DecimatorParams(
    skipSize = 2
  )
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AXI4DecimatorBlock(params, AddressSet(0x0, 0xF), 4) with AXI4StandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/Decimator"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
