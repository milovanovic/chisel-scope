package hdmi.preproc

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.FixedPoint

import dsptools.numbers._
import dspblocks._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

// ProcFFT1D parameters
case class ProcFFT1DParameters[T <: Data: Real: BinaryRepresentation] (
  flowControlParams   : FlowControlParams,
  scalerParams        : Scaler1DParams,
  interpolatorParams  : InterpolatorV2Params[T],
  interpolatorParams2 : InterpolatorV2Params[T],
  dataCounterParams   : DataCounterParams,
)

class AXI4ProcFFT1D[T <: Data : Real: BinaryRepresentation](params: ProcFFT1DParameters[T], beatBytes: Int)(implicit p: Parameters) extends ProcFFT1D[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock {
  override val mem = None
}

abstract class ProcFFT1D[T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data](params: ProcFFT1DParameters[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {
  // blocks
  val interpolator0 = LazyModule(new AXI4InterpolatorV2Block(params.interpolatorParams, beatBytes = beatBytes))
  val interpolator1 = LazyModule(new AXI4InterpolatorV2Block(params.interpolatorParams, beatBytes = beatBytes))
  // val interpolator2 = LazyModule(new AXI4InterpolatorV2Block(params.interpolatorParams2, beatBytes = beatBytes))
  val datacounter0  = LazyModule(new AXI4DataCounterBlock(params.dataCounterParams, beatBytes))
  val datacounter1  = LazyModule(new AXI4DataCounterBlock(params.dataCounterParams, beatBytes))
  val datacounter2  = LazyModule(new AXI4DataCounterBlock(params.dataCounterParams, beatBytes))
  val flowcontrol   = LazyModule(new FlowControl(params.flowControlParams, beatBytes = beatBytes))

  val zohNode = AXI4StreamIdentityNode()

  // StreamNode
  val streamNode = flowcontrol.inNode

  // connect nodes
  datacounter0.streamNode := interpolator0.streamNode := AXI4StreamBuffer(BufferParams(depth = params.flowControlParams.dataSize)) := flowcontrol.outNode0
  datacounter1.streamNode := interpolator1.streamNode := AXI4StreamBuffer(BufferParams(depth = params.flowControlParams.dataSize)) := flowcontrol.outNode1
  datacounter2.streamNode := zohNode := AXI4StreamBuffer(BufferParams(depth = params.flowControlParams.dataSize)) := flowcontrol.outNode2

  lazy val module = new LazyModuleImp(this) {
    // IOs
    val start    = IO(Input(Bool()))
    val loadRegs = IO(Input(Bool()))

    val width = log2Ceil(params.scalerParams.scale)
    val i_scaler_x = IO(Input((UInt((width+1).W))))

    // Registers
    val loadRegs_delayed   = RegNext(loadRegs, false.B)
    val start_delayed      = RegNext(start, false.B)

    // ZoH pins
    val (zin, _)  = zohNode.in(0)
    val (zout, _) = zohNode.out(0)

    // Zero order hold for peak
    val zoh = Module(new ZeroOrderHold(params.interpolatorParams2.zoh, params.interpolatorParams2.scalerSize, 4))
    zoh.io.loadReg   := loadRegs && (loadRegs_delayed === false.B)
    zoh.io.scaler    := i_scaler_x
    zin.ready        := zoh.io.in.ready
    zoh.io.in.valid  := zin.valid
    zoh.io.in.bits   := zin.bits.data.asTypeOf(params.interpolatorParams2.zoh.proto)
    zout.valid       := zoh.io.out.valid
    zout.bits.data   := zoh.io.out.bits.asUInt
    zoh.io.out.ready := zout.ready

    interpolator0.module.scaler  := i_scaler_x
    interpolator0.module.loadReg := loadRegs && (loadRegs_delayed === false.B)
    interpolator1.module.scaler  := i_scaler_x
    interpolator1.module.loadReg := loadRegs && (loadRegs_delayed === false.B)
    // interpolator2.module.scaler  := scaler2.module.scalerX
    // interpolator2.module.loadReg := loadRegs && (loadRegs_delayed === false.B)

    datacounter0.module.scaler := i_scaler_x
    datacounter1.module.scaler := i_scaler_x
    datacounter2.module.scaler := i_scaler_x

    datacounter0.module.start := flowcontrol.module.sync
    datacounter1.module.start := flowcontrol.module.sync
    datacounter2.module.start := flowcontrol.module.sync

    flowcontrol.module.start := start && (start_delayed === false.B)
  }
}

trait AXI4ProcFFT1DPins extends AXI4ProcFFT1D[FixedPoint] {
  def beatBytes: Int = 4

  {
    implicit val valName = ValName(s"out_0")
    val ioOutNode0 = BundleBridgeSink[AXI4StreamBundle]()
    ioOutNode0 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := datacounter0.streamNode
    val out0 = InModuleBody { ioOutNode0.makeIO() }
  }
  {
    implicit val valName = ValName(s"out_1")
    val ioOutNode1 = BundleBridgeSink[AXI4StreamBundle]()
    ioOutNode1 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := datacounter1.streamNode
    val out1 = InModuleBody { ioOutNode1.makeIO() }
  }
  {
    implicit val valName = ValName(s"out_2")
    val ioOutNode2 = BundleBridgeSink[AXI4StreamBundle]()
    ioOutNode2 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := datacounter2.streamNode
    val out2 = InModuleBody { ioOutNode2.makeIO() }
  }
  {
    implicit val valName = ValName(s"in")
    val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 8)))
    streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 8)) := ioInNode
    val in0 = InModuleBody { ioInNode.makeIO() }
  }
}

class ProcFFT1DParams(fftSize: Int = 1024) {
  val params : ProcFFT1DParameters[FixedPoint] = ProcFFT1DParameters (
    flowControlParams = FlowControlParams(
      dataSize = fftSize
    ),
    scalerParams = Scaler1DParams(
      scale   = log2Ceil(128)
    ),
    interpolatorParams = InterpolatorV2Params(
      proto = FixedPoint(16.W, 10.BP),
      scalerSize = log2Ceil(128),
      zoh = ZeroOrderHoldParams(
        proto = FixedPoint(16.W, 10.BP),
        size  = 1
      )
    ),
    interpolatorParams2 = InterpolatorV2Params(
      proto = FixedPoint(8.W, 0.BP),
      scalerSize = log2Ceil(128),
      zoh = ZeroOrderHoldParams(
        proto = FixedPoint(8.W, 0.BP),
        size  = 1
      )
    ),
    dataCounterParams = DataCounterParams(
      dataSize = fftSize,
      scalerSize = log2Ceil(128)
    ),
  )
}

object ProcFFT1DApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = (new ProcFFT1DParams).params
  val lazyDut = LazyModule(new AXI4ProcFFT1D(params, 4) with AXI4ProcFFT1DPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/ProcFFT1D"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

