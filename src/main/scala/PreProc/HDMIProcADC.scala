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

// HDMIProcADC parameters
case class HDMIProcADCParameters[T <: Data: Real: BinaryRepresentation] (
  triggerParams      : TriggerParams[T],
  scalerParams       : ScalerParams,
  interpolatorParams : InterpolatorParams[T],
  decimatorParams    : DecimatorParams,
  dataCounterParams  : DataCounterParams,
)

// HDMIProcADC Addresses
case class HDMIProcADCAddresses (
  triggerAddress    : AddressSet,
  scalerAddress     : AddressSet,
  decimatorAddress0 : AddressSet,
  decimatorAddress1 : AddressSet,
)

class HDMIProcADC[T <: Data: Real: BinaryRepresentation](params: HDMIProcADCParameters[T], address: HDMIProcADCAddresses, beatBytes: Int) extends LazyModule()(Parameters.empty) {

  val trigger       = LazyModule(new AXI4TriggerBlock(params.triggerParams, address.triggerAddress, beatBytes = beatBytes))
  val scaler        = LazyModule(new AXI4ScalerBlock(params.scalerParams, address.scalerAddress, beatBytes = beatBytes))
  val datasplitter0 = LazyModule(new DataSplitter(beatBytes))
  val interpolator0 = LazyModule(new AXI4InterpolatorBlock(params.interpolatorParams, beatBytes = beatBytes))
  val interpolator1 = LazyModule(new AXI4InterpolatorBlock(params.interpolatorParams, beatBytes = beatBytes))
  val decimator0    = LazyModule(new AXI4DecimatorBlock(params.decimatorParams, address.decimatorAddress0, beatBytes = beatBytes))
  val decimator1    = LazyModule(new AXI4DecimatorBlock(params.decimatorParams, address.decimatorAddress1, beatBytes = beatBytes))
  val datacounter0  = LazyModule(new AXI4DataCounterBlock(params.dataCounterParams, beatBytes))
  val datacounter1  = LazyModule(new AXI4DataCounterBlock(params.dataCounterParams, beatBytes))

  // define mem
  lazy val blocks = Seq(trigger, scaler, decimator0, decimator1)
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)
  for (b <- blocks) {
    b.mem.foreach { _ := AXI4Buffer() := bus.node }
  }

  // connect nodes
  datasplitter0.inNode  := AXI4StreamBuffer(BufferParams(depth = params.triggerParams.dataSize)) := scaler.streamNode  := trigger.streamNode
  datacounter0.streamNode := decimator0.streamNode := interpolator0.streamNode := datasplitter0.outNode0
  datacounter1.streamNode := decimator1.streamNode := interpolator1.streamNode := datasplitter0.outNode1

  lazy val module = new LazyModuleImp(this) {
    // IOs
    val start     = IO(Input(Bool()))
    val triggered = IO(Input(Bool()))
    val sync      = IO(Output(Bool()))
    val loadRegs  = IO(Input(Bool()))

    val width = log2Ceil(params.scalerParams.scale)
    val scaler_y = IO(Output(UInt((width+1).W)))
    val scaler_x = IO(Output(UInt((width).W)))

    // Registers
    val loadRegs_delayed  = RegNext(loadRegs, false.B)
    val start_delayed     = RegNext(start, false.B)
    val triggered_delayed = RegNext(triggered, false.B)

    scaler_y := scaler.module.scalerY
    scaler_x := scaler.module.scalerX

    interpolator0.module.scaler  := scaler.module.scalerX
    interpolator1.module.scaler  := scaler.module.scalerX
    interpolator0.module.loadReg := loadRegs && (loadRegs_delayed === false.B)
    interpolator1.module.loadReg := loadRegs && (loadRegs_delayed === false.B)

    datacounter0.module.scaler := scaler.module.scalerX
    datacounter1.module.scaler := scaler.module.scalerX

    datacounter0.module.start := trigger.module.sync
    datacounter1.module.start := trigger.module.sync

    trigger.module.triggered := triggered && (triggered_delayed === false.B)
    trigger.module.start     := start && (start_delayed === false.B)
    sync := trigger.module.sync
  }
}

trait HDMIProcADCPins extends HDMIProcADC[FixedPoint] {
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
    implicit val valName = ValName(s"in")
    val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    trigger.streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode
    val in0 = InModuleBody { ioInNode.makeIO() }
  }
}

class HDMIProcADCParams(fftSize: Int = 1024, x_size: Int = 1536) {
  val params : HDMIProcADCParameters[FixedPoint] = HDMIProcADCParameters (
    triggerParams = TriggerParams(
      proto = FixedPoint(16.W, 10.BP),
      dataSize = fftSize,
      edgeType = 0,
    ),
    scalerParams = ScalerParams(
      scale   = log2Ceil(128),
      complex = true
    ),
    interpolatorParams = InterpolatorParams(
      proto = FixedPoint(16.W, 10.BP),
      scalerSize = log2Ceil(128),
      zoh = ZeroOrderHoldParams(
        proto = FixedPoint(16.W, 10.BP),
        size  = 3
      )
    ),
    decimatorParams = DecimatorParams(
      skipSize = 2
    ),
    dataCounterParams = DataCounterParams(
      dataSize = x_size,
      scalerSize = log2Ceil(128)
    )
  )
}

class HDMIProcADCAddr(startAddress: BigInt = 0x0000) {
  val addresses : HDMIProcADCAddresses = HDMIProcADCAddresses (
    triggerAddress    = AddressSet(startAddress, 0xFF),
    scalerAddress     = AddressSet(startAddress + 0x100, 0xFF),
    decimatorAddress0 = AddressSet(startAddress + 0x200, 0xFF),
    decimatorAddress1 = AddressSet(startAddress + 0x300, 0xFF),
  )
}

object HDMIProcADCApp extends App
{
  val params = (new HDMIProcADCParams).params
  val address = (new HDMIProcADCAddr(0x0000)).addresses
  val lazyDut = LazyModule(new HDMIProcADC(params, address, 4) with HDMIProcADCPins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/HDMIProcADC"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

