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

//  InterpolatorV2
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

// InterpolatorV2 parameters
case class InterpolatorV2Params[T <: Data: Real](
  proto : T,                            // input/output data type
  scalerSize: Int = 7,
  zoh   : ZeroOrderHoldParams[T]        // Zero-order hold parameters
) {
  requireIsChiselType(proto,  s"($proto) must be chisel type")
}

abstract class InterpolatorV2 [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: InterpolatorV2Params[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {

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
      val read  = in.fire()
      val write = out.fire()
      
      val x         = in.bits.data.asTypeOf(params.proto)
      val x_delayed = RegInit(0.U.asTypeOf(params.proto))
      val sumT: T   = (x * (params.zoh.size << params.scalerSize)).cloneType
      
      val u         = Wire(params.proto)
      val v         = Wire(params.proto)

      val z         = Wire(sumT)
      val z_delayed = RegInit(0.U.asTypeOf(sumT))
      val zoh       = Module(new ZeroOrderHold(params.zoh, params.scalerSize, beatBytes))

      // Connect signals
      zoh.io.loadReg  := loadReg
      zoh.io.scaler   := scaler
      in.ready        := zoh.io.in.ready
      zoh.io.in.valid := in.valid
      u := x - x_delayed
      zoh.io.in.bits  := u
      v := zoh.io.out.bits
      z := z_delayed + v

      val binPos = (params.proto match {
        case fp: FixedPoint => fp.binaryPoint.get
        case _ => 0
      })

      out.bits.data    := (z >> r_scaler).asTypeOf(out.bits.data.cloneType)
      out.valid        := zoh.io.out.valid
      zoh.io.out.ready := out.ready

      // When loadReg is active, load register and reset data
      when(loadReg) {
        r_scaler := scaler
        x_delayed := 0.U.asTypeOf(params.proto)
        z_delayed := 0.U.asTypeOf(sumT)
      }
      .otherwise {
        r_scaler := r_scaler
        when(read){
          x_delayed := x
        }
        when(write){
          z_delayed := z
        }
      }
    }
}

class AXI4InterpolatorV2Block[T <: Data : Real: BinaryRepresentation](params: InterpolatorV2Params[T], beatBytes: Int = 2)(implicit p: Parameters) extends InterpolatorV2[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock {
  override val mem = None
}


trait AXI4InterpolatorV2StandaloneBlock extends AXI4DspBlock {

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

object InterpolatorV2App extends App
{
  val params: InterpolatorV2Params[FixedPoint] = InterpolatorV2Params(
    proto = FixedPoint(16.W, 14.BP),
    scalerSize = 7,
    zoh   = ZeroOrderHoldParams(
        proto = FixedPoint(16.W, 14.BP),
        size  = 4
    )
  )
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AXI4InterpolatorV2Block(params, 2) with AXI4InterpolatorV2StandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/InterpolatorV2"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}