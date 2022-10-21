package hdmi.preproc  

import chisel3._ 
import chisel3.util.log2Ceil
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental.FixedPoint
import chisel3.internal.requireIsChiselType

import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

//  Interpolation
// https://www.dsprelated.com/showarticle/1123.php
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

// Interpolation parameters
case class InterpolationParams[T <: Data: Real](
  proto : T,            // input/output data type
  scalerSize: Int = 7,
  zoh: ZOHParams        // Zero-order hold parameters
) {
  requireIsChiselType(proto,  s"($proto) must be chisel type")
}

abstract class Interpolation [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: InterpolationParams[T]) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {

    val streamNode = AXI4StreamIdentityNode()

    lazy val module = new LazyModuleImp(this) {
      val (in, _)  = streamNode.in(0)
      val (out, _) = streamNode.out(0)

      // Additional IOs
      val scaler  = IO(Input(UInt((params.scalerSize).W)))

      // Additional interpolation factor register
      val r_scaler = RegInit(log2Ceil(params.zoh.size).U((params.scalerSize).W))
      r_scaler := (log2Ceil(params.zoh.size).U << scaler)

      // ZOH
      val zoh       = Module(new ZOH(params.zoh, params.scalerSize))
      // Signal definitions
      val x         = RegNext(in.bits.data.asTypeOf(params.proto), 0.U.asTypeOf(params.proto))
      val x_delayed = RegNext(x, 0.U.asTypeOf(x.cloneType))
      val u         = Wire(params.proto)
      val sumT: T   = (zoh.io.o_data.asTypeOf(params.proto) * (params.zoh.size << params.scalerSize)).cloneType
      val z         = Wire(sumT)
      val z_delayed = RegNext(z, 0.U.asTypeOf(z.cloneType))

      // Connect signals
      zoh.io.start := RegNext(in.valid, false.B)
      in.ready := ~(reset.asBool)
      zoh.io.scaler   := scaler
      u := x - x_delayed
      zoh.io.i_data := u.asUInt
      z := zoh.io.o_data.asTypeOf(z.cloneType) + z_delayed

      val binPos = (params.proto match {
        case fp: FixedPoint => fp.binaryPoint.get
        case _ => 0
      })

      out.bits.data := (z >> r_scaler).asTypeOf(out.bits.data.cloneType)
      out.valid     := true.B
    }
}

class AXI4InterpolationBlock[T <: Data : Real: BinaryRepresentation](params: InterpolationParams[T])(implicit p: Parameters) extends Interpolation[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params) with AXI4DspBlock {
  override val mem = None
}


trait AXI4InterpolationStandaloneBlock extends AXI4DspBlock {

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

object InterpolationApp extends App
{
  val params: InterpolationParams[FixedPoint] = InterpolationParams(
    proto = FixedPoint(16.W, 14.BP),
    scalerSize = 7,
    zoh   = ZOHParams(
        width = 16,
        size  = 1
    )
  )
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AXI4InterpolationBlock(params) with AXI4InterpolationStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/Interpolation"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}