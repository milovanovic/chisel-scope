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

// Proc1D parameters
case class Proc1DParameters[T <: Data: Real: BinaryRepresentation] (
  dataSplitterParams  : FFTDataSplitterParams,
  interpolatorParams  : InterpolationParams[T],
  interpolatorParams2 : InterpolationParams[T]
)

class AXI4Proc1D[T <: Data : Real: BinaryRepresentation](params: Proc1DParameters[T])(implicit p: Parameters) extends Proc1D[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params) with AXI4DspBlock {
  override val mem = None
}

abstract class Proc1D[T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data](params: Proc1DParameters[T]) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {
  // blocks
  val interpolator0 = LazyModule(new AXI4InterpolationBlock(params.interpolatorParams))
  val interpolator1 = LazyModule(new AXI4InterpolationBlock(params.interpolatorParams))
  val interpolator2 = LazyModule(new AXI4InterpolationBlock(params.interpolatorParams2))
  val datasplit     = LazyModule(new FFTDataSplitter(params.dataSplitterParams))

  // StreamNode
  val streamNode = datasplit.inNode

  // connect nodes
  interpolator0.streamNode := datasplit.outNode0
  interpolator1.streamNode := datasplit.outNode1
  interpolator2.streamNode := datasplit.outNode2

  lazy val module = new LazyModuleImp(this) {
    // IOs
    val read_1D  = IO(Input(Bool()))
    val i_addr_x = IO(Input(UInt(log2Ceil(params.dataSplitterParams.dataSize).W)))
    val i_scaler_x = IO(Input((UInt((params.interpolatorParams.scalerSize).W))))

    interpolator0.module.scaler := i_scaler_x
    interpolator1.module.scaler := i_scaler_x
    interpolator2.module.scaler := i_scaler_x

    // Start and address for data splitter
    datasplit.module.read     := read_1D
    datasplit.module.i_addr_x := i_addr_x
  }
}

trait AXI4Proc1DPins extends AXI4Proc1D[FixedPoint] {
  def beatBytes: Int = 6

  val ioOutNode0 = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode0 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := interpolator0.streamNode
  val out0 = InModuleBody { ioOutNode0.makeIO() }

  val ioOutNode1 = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode1 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := interpolator1.streamNode
  val out1 = InModuleBody { ioOutNode1.makeIO() }


  val ioOutNode2 = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode2 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := interpolator2.streamNode
  val out2 = InModuleBody { ioOutNode2.makeIO() }

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
  streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode
  val in0 = InModuleBody { ioInNode.makeIO() }
}

trait AXI4Proc1DOutputPins extends AXI4Proc1D[FixedPoint] {
  val ioOutNode0 = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode0 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := interpolator0.streamNode
  val out0 = InModuleBody { ioOutNode0.makeIO() }

  val ioOutNode1 = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode1 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := interpolator1.streamNode
  val out1 = InModuleBody { ioOutNode1.makeIO() }

  val ioOutNode2 = BundleBridgeSink[AXI4StreamBundle]()
  ioOutNode2 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := interpolator2.streamNode
  val out2 = InModuleBody { ioOutNode2.makeIO() }
}

class Proc1DParams(fftSize: Int = 1024, scalerSize: Int = log2Ceil(128)) {
  val params : Proc1DParameters[FixedPoint] = Proc1DParameters (
    dataSplitterParams = FFTDataSplitterParams(
      dataSize = fftSize
    ),
    interpolatorParams = InterpolationParams(
      proto = FixedPoint(16.W, 10.BP),
      scalerSize = scalerSize,
      zoh = ZOHParams(
        width = 16,
        size  = 1
      )
    ),
    interpolatorParams2 = InterpolationParams(
      proto = FixedPoint(8.W, 0.BP),
      scalerSize = scalerSize,
      zoh = ZOHParams(
        width = 8,
        size  = 1
      )
    ),
  )
}

object Proc1DApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val params = (new Proc1DParams).params
  val lazyDut = LazyModule(new AXI4Proc1D(params) with AXI4Proc1DPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/Proc1D"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
