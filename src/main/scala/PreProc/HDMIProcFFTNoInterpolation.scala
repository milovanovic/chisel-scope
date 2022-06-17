package hdmi.preproc

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.FixedPoint

import dsptools.numbers._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import scala.math._

// HDMIProcFFTNoInterpolation parameters
case class HDMIProcFFTNoInterpolationParameters[T <: Data: Real: BinaryRepresentation] (
  flowControlParams   : FlowControlParams,
  scalerParams        : ScalerParams,
  interpolatorParams  : InterpolatorV2Params[T],
  interpolatorParams2 : InterpolatorV2Params[T],
  dataCounterParams   : DataCounterParams,
)

// HDMIProcFFTNoInterpolation Addresses
case class HDMIProcFFTNoInterpolationAddresses (
  scalerAddress0    : AddressSet,
  scalerAddress1    : AddressSet,
  scalerAddress2    : AddressSet,
)

class HDMIProcFFTNoInterpolation[T <: Data: Real: BinaryRepresentation](params: HDMIProcFFTNoInterpolationParameters[T], address: HDMIProcFFTNoInterpolationAddresses, beatBytes: Int) extends LazyModule()(Parameters.empty) {

  val scaler0       = LazyModule(new AXI4ScalerBlock(params.scalerParams, address.scalerAddress0, beatBytes = beatBytes))
  val scaler1       = LazyModule(new AXI4ScalerBlock(params.scalerParams, address.scalerAddress1, beatBytes = beatBytes))
  val scaler2       = LazyModule(new AXI4ScalerBlock(params.scalerParams, address.scalerAddress2, beatBytes = beatBytes))
  val interpolator0 = LazyModule(new AXI4InterpolatorV2Block(params.interpolatorParams, beatBytes = beatBytes))
  val interpolator1 = LazyModule(new AXI4InterpolatorV2Block(params.interpolatorParams, beatBytes = beatBytes))
  // val interpolator2 = LazyModule(new AXI4InterpolatorV2Block(params.interpolatorParams2, beatBytes = beatBytes))
  val datacounter0  = LazyModule(new AXI4DataCounterBlock(params.dataCounterParams, beatBytes))
  val datacounter1  = LazyModule(new AXI4DataCounterBlock(params.dataCounterParams, beatBytes))
  val datacounter2  = LazyModule(new AXI4DataCounterBlock(params.dataCounterParams, beatBytes))
  val flowcontrol   = LazyModule(new FlowControl(params.flowControlParams, beatBytes = beatBytes))

  val zohNode = AXI4StreamIdentityNode()
  

  // define mem
  lazy val blocks = Seq(scaler0, scaler1, scaler2)
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)
  for (b <- blocks) {
    b.mem.foreach { _ := AXI4Buffer() := bus.node }
  }

  // connect nodes
  datacounter0.streamNode := interpolator0.streamNode := AXI4StreamBuffer(BufferParams(depth = params.flowControlParams.dataSize)) := scaler0.streamNode := flowcontrol.outNode0
  datacounter1.streamNode := interpolator1.streamNode := AXI4StreamBuffer(BufferParams(depth = params.flowControlParams.dataSize)) := scaler1.streamNode := flowcontrol.outNode1
  datacounter2.streamNode := zohNode := AXI4StreamBuffer(BufferParams(depth = params.flowControlParams.dataSize)) := scaler2.streamNode := flowcontrol.outNode2

  lazy val module = new LazyModuleImp(this) {
    // IOs
    val start    = IO(Input(Bool()))
    val sync     = IO(Output(Bool()))
    val loadRegs = IO(Input(Bool()))

    val width = log2Ceil(params.scalerParams.scale)
    val cut_scaler_y = IO(Output(UInt((width+1).W)))
    val cut_scaler_x = IO(Output(UInt((width).W)))

    // Registers
    val loadRegs_delayed   = RegNext(loadRegs, false.B)
    val start_delayed      = RegNext(start, false.B)

    // ZoH pins
    val (zin, _)  = zohNode.in(0)
    val (zout, _) = zohNode.out(0)

    // Zero order hold for peak
    val zoh = Module(new ZeroOrderHold(params.interpolatorParams2.zoh, params.interpolatorParams2.scalerSize, 4))
    zoh.io.loadReg   := loadRegs && (loadRegs_delayed === false.B)
    zoh.io.scaler    := scaler2.module.scalerX
    zin.ready        := zoh.io.in.ready
    zoh.io.in.valid  := zin.valid
    zoh.io.in.bits   := zin.bits.data.asTypeOf(params.interpolatorParams2.zoh.proto)
    zout.valid       := zoh.io.out.valid
    zout.bits.data   := zoh.io.out.bits.asUInt
    zoh.io.out.ready := zout.ready

    cut_scaler_y := scaler0.module.scalerY
    cut_scaler_x := scaler0.module.scalerX

    interpolator0.module.scaler  := scaler0.module.scalerX
    interpolator0.module.loadReg := loadRegs && (loadRegs_delayed === false.B)
    interpolator1.module.scaler  := scaler1.module.scalerX
    interpolator1.module.loadReg := loadRegs && (loadRegs_delayed === false.B)
    // interpolator2.module.scaler  := scaler2.module.scalerX
    // interpolator2.module.loadReg := loadRegs && (loadRegs_delayed === false.B)

    datacounter0.module.scaler := scaler0.module.scalerX
    datacounter1.module.scaler := scaler1.module.scalerX
    datacounter2.module.scaler := scaler2.module.scalerX

    datacounter0.module.start := flowcontrol.module.sync
    datacounter1.module.start := flowcontrol.module.sync
    datacounter2.module.start := flowcontrol.module.sync

    flowcontrol.module.start := start && (start_delayed === false.B)
    sync := flowcontrol.module.sync
  }
}

trait HDMIProcFFTNoInterpolationPins extends HDMIProcFFTNoInterpolation[FixedPoint] {
  def beatBytes: Int = 4

  // Generate AXI4 slave output
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

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
    flowcontrol.inNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 8)) := ioInNode
    val in0 = InModuleBody { ioInNode.makeIO() }
  }
}

class HDMIProcFFTNoInterpolationParams(fftSize: Int = 1024) {
  val params : HDMIProcFFTNoInterpolationParameters[FixedPoint] = HDMIProcFFTNoInterpolationParameters (
    flowControlParams = FlowControlParams(
      dataSize = fftSize
    ),
    scalerParams = ScalerParams(
      scale   = log2Ceil(128),
      complex = false
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

class HDMIProcFFTNoInterpolationAddr(startAddress: BigInt = 0x0000) {
  val addresses : HDMIProcFFTNoInterpolationAddresses = HDMIProcFFTNoInterpolationAddresses (
    scalerAddress0    = AddressSet(startAddress + 0x000, 0xFF),
    scalerAddress1    = AddressSet(startAddress + 0x100, 0xFF),
    scalerAddress2    = AddressSet(startAddress + 0x200, 0xFF),
  )
}

object HDMIProcFFTNoInterpolationApp extends App
{
  val params = (new HDMIProcFFTNoInterpolationParams).params
  val address = (new HDMIProcFFTNoInterpolationAddr(0x0000)).addresses
  val lazyDut = LazyModule(new HDMIProcFFTNoInterpolation(params, address, 4) with HDMIProcFFTNoInterpolationPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/HDMIProcFFTNoInterpolation"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

