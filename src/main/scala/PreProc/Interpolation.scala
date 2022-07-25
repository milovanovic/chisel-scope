package hdmi.preproc  

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.FixedPoint
import chisel3.internal.requireIsChiselType

import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

//  Interpolation
// |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾|
// |                      ___   u[n]   _______________   v[n]   ___      z[n]      ___               |
// |   x[n] ---- * ----➛ | + | -----➛ |zero-order hold| -----➛ | + | ---- * ----➛ | * | ----➛ y[n]   |
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

abstract class Interpolation [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: InterpolationParams[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {

    val streamNode = AXI4StreamIdentityNode()

    lazy val module = new LazyModuleImp(this) {
      val (in, _)  = streamNode.in(0)
      val (out, _) = streamNode.out(0)

      // Additional IOs
      val loadReg = IO(Input(Bool()))
      val scaler  = IO(Input(UInt(log2Ceil(params.scalerSize).W)))

      // Additional interpolation factor register
      val r_scaler = RegInit(0.U(log2Ceil(params.scalerSize).W))

      // Signal definitions
      val x         = in.bits.data.asTypeOf(params.proto)
      val x_delayed = RegInit(0.U.asTypeOf(params.proto))
      val sumT: T   = (x * (params.zoh.size << params.scalerSize)).cloneType
      
      val u         = Wire(params.proto)
      val v         = Wire(params.proto)

      val z         = Wire(sumT)
      val z_delayed = RegNext(z, 0.U.asTypeOf(z.cloneType))
      val zoh       = Module(new ZOH(params.zoh, params.scalerSize, beatBytes))

      // Connect signals
      when(in.fire()){
        x_delayed := x
      }

      zoh.io.loadReg  := loadReg
      zoh.io.scaler   := scaler
      in.ready        := zoh.io.i_ready
      zoh.io.i_valid := in.valid
      u := x - x_delayed
      zoh.io.i_data  := u.asUInt
      v := zoh.io.o_data.asTypeOf(v.cloneType)
      z := z_delayed + v

      val binPos = (params.proto match {
        case fp: FixedPoint => fp.binaryPoint.get
        case _ => 0
      })

      out.bits.data := (z >> r_scaler).asTypeOf(out.bits.data.cloneType)
      out.valid     := true.B

      // When loadReg is active, load register
      when(loadReg) {
        r_scaler := scaler
      }
    }
}

class AXI4InterpolationBlock[T <: Data : Real: BinaryRepresentation](params: InterpolationParams[T], beatBytes: Int = 2)(implicit p: Parameters) extends Interpolation[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock {
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
        size  = 4
    )
  )
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AXI4InterpolationBlock(params, 2) with AXI4InterpolationStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/Interpolation"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}