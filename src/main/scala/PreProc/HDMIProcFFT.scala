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

// HDMIProcFFT parameters
case class HDMIProcFFTParameters[T <: Data: Real: BinaryRepresentation] (
  flowControlParams   : FlowControlParams,
  scalerParams        : ScalerParams,
  interpolatorParams  : InterpolatorParams[T],
  interpolatorParams2 : InterpolatorParams[T],
  decimatorParams     : DecimatorParams,
  dataCounterParams   : DataCounterParams,
)

// HDMIProcFFT Addresses
case class HDMIProcFFTAddresses (
  scalerAddress0    : AddressSet,
  scalerAddress1    : AddressSet,
  scalerAddress2    : AddressSet,
  decimatorAddress0 : AddressSet,
  decimatorAddress1 : AddressSet,
  decimatorAddress2 : AddressSet
)

class HDMIProcFFT[T <: Data: Real: BinaryRepresentation](params: HDMIProcFFTParameters[T], address: HDMIProcFFTAddresses, beatBytes: Int) extends LazyModule()(Parameters.empty) {

  val scaler0       = LazyModule(new AXI4ScalerBlock(params.scalerParams, address.scalerAddress0, beatBytes = beatBytes))
  val scaler1       = LazyModule(new AXI4ScalerBlock(params.scalerParams, address.scalerAddress1, beatBytes = beatBytes))
  val scaler2       = LazyModule(new AXI4ScalerBlock(params.scalerParams, address.scalerAddress2, beatBytes = beatBytes))
  val interpolator0 = LazyModule(new AXI4InterpolatorBlock(params.interpolatorParams, beatBytes = beatBytes))
  val interpolator1 = LazyModule(new AXI4InterpolatorBlock(params.interpolatorParams, beatBytes = beatBytes))
  val interpolator2 = LazyModule(new AXI4InterpolatorBlock(params.interpolatorParams2, beatBytes = beatBytes))
  val decimator0    = LazyModule(new AXI4DecimatorBlock(params.decimatorParams, address.decimatorAddress0, beatBytes = beatBytes))
  val decimator1    = LazyModule(new AXI4DecimatorBlock(params.decimatorParams, address.decimatorAddress1, beatBytes = beatBytes))
  val decimator2    = LazyModule(new AXI4DecimatorBlock(params.decimatorParams, address.decimatorAddress2, beatBytes = beatBytes))
  val datacounter0  = LazyModule(new AXI4DataCounterBlock(params.dataCounterParams, beatBytes))
  val datacounter1  = LazyModule(new AXI4DataCounterBlock(params.dataCounterParams, beatBytes))
  val datacounter2  = LazyModule(new AXI4DataCounterBlock(params.dataCounterParams, beatBytes))
  val flowcontrol   = LazyModule(new FlowControl(params.flowControlParams, beatBytes = beatBytes))
  

  // define mem
  lazy val blocks = Seq(scaler0, scaler1, scaler2, decimator0, decimator1, decimator2)
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)
  for (b <- blocks) {
    b.mem.foreach { _ := AXI4Buffer() := bus.node }
  }

  // connect nodes
  datacounter0.streamNode := decimator0.streamNode := interpolator0.streamNode := AXI4StreamBuffer(BufferParams(depth = params.flowControlParams.dataSize)) := scaler0.streamNode := flowcontrol.outNode0
  datacounter1.streamNode := decimator1.streamNode := interpolator1.streamNode := AXI4StreamBuffer(BufferParams(depth = params.flowControlParams.dataSize)) := scaler1.streamNode := flowcontrol.outNode1
  datacounter2.streamNode := decimator2.streamNode := interpolator2.streamNode := AXI4StreamBuffer(BufferParams(depth = params.flowControlParams.dataSize)) := scaler2.streamNode := flowcontrol.outNode2

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

    cut_scaler_y := scaler0.module.scalerY
    cut_scaler_x := scaler0.module.scalerX

    interpolator0.module.scaler  := scaler0.module.scalerX
    interpolator0.module.loadReg := loadRegs && (loadRegs_delayed === false.B)
    interpolator1.module.scaler  := scaler1.module.scalerX
    interpolator1.module.loadReg := loadRegs && (loadRegs_delayed === false.B)
    interpolator2.module.scaler  := scaler2.module.scalerX
    interpolator2.module.loadReg := loadRegs && (loadRegs_delayed === false.B)

    datacounter0.module.start := flowcontrol.module.sync
    datacounter1.module.start := flowcontrol.module.sync
    datacounter2.module.start := flowcontrol.module.sync

    flowcontrol.module.start := start && (start_delayed === false.B)
    sync := flowcontrol.module.sync
  }
}

trait HDMIProcFFTPins extends HDMIProcFFT[FixedPoint] {
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

class HDMIProcFFTParams(fftSize: Int = 1024, x_size: Int = 1536) {
  val params : HDMIProcFFTParameters[FixedPoint] = HDMIProcFFTParameters (
    flowControlParams = FlowControlParams(
      dataSize = fftSize
    ),
    scalerParams = ScalerParams(
      scale   = log2Ceil(128),
      complex = false
    ),
    interpolatorParams = InterpolatorParams(
      proto = FixedPoint(16.W, 10.BP),
      scalerSize = log2Ceil(128),
      zoh = ZeroOrderHoldParams(
        proto = FixedPoint(16.W, 10.BP),
        size  = 3
      )
    ),
    interpolatorParams2 = InterpolatorParams(
      proto = FixedPoint(8.W, 0.BP),
      scalerSize = log2Ceil(128),
      zoh = ZeroOrderHoldParams(
        proto = FixedPoint(8.W, 0.BP),
        size  = 3
      )
    ),
    decimatorParams = DecimatorParams(
      skipSize = 2
    ),
    dataCounterParams = DataCounterParams(
      dataSize = x_size,
      scalerSize = log2Ceil(128),
    ),
  )
}

class HDMIProcFFTAddr(startAddress: BigInt = 0x0000) {
  val addresses : HDMIProcFFTAddresses = HDMIProcFFTAddresses (
    scalerAddress0    = AddressSet(startAddress + 0x000, 0xFF),
    scalerAddress1    = AddressSet(startAddress + 0x100, 0xFF),
    scalerAddress2    = AddressSet(startAddress + 0x200, 0xFF),
    decimatorAddress0 = AddressSet(startAddress + 0x300, 0xFF),
    decimatorAddress1 = AddressSet(startAddress + 0x400, 0xFF),
    decimatorAddress2 = AddressSet(startAddress + 0x500, 0xFF),
  )
}

object HDMIProcFFTApp extends App
{
  val params = (new HDMIProcFFTParams).params
  val address = (new HDMIProcFFTAddr(0x0000)).addresses
  val lazyDut = LazyModule(new HDMIProcFFT(params, address, 4) with HDMIProcFFTPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/HDMIProcFFT"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

