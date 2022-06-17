package hdmi.scope 

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.{FixedPoint, IO}
import dsptools.numbers._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import hdmi._
import hdmi.frameBuffer._
import hdmi.preproc._

// HDMIScope parameters
case class HDMIScopeParameters[T <: Data: Real: BinaryRepresentation] (
  adcprocParams  : HDMIProcADCNoInterpolationParameters[T],
  fftprocParams  : HDMIProcFFTNoInterpolationParameters[T],
  asyncParams    : AsyncLoggerParams[T],
  frameBufferParams : FrameBufferParameters[T]
)

// HDMIScope Addresses
case class HDMIScopeAddresses (
  adcprocAddress  : HDMIProcADCNoInterpolationAddresses,
  fftprocAddress  : HDMIProcFFTNoInterpolationAddresses
)

// HDMIScope IO
class HDMIScopeIO extends Bundle {
    // tmds output ports
    val clk_p  = Output(Bool())
    val clk_n  = Output(Bool())
    val data_p = Output(UInt(3.W))
    val data_n = Output(UInt(3.W))

    // Reset and clock
    val clk_pixel  = Input(Clock())
    val clk_serdes = Input(Clock())
    val reset_hdmi = Input(Bool())
}

class HDMIScope[T <: Data: Real: BinaryRepresentation](params: HDMIScopeParameters[T], address: HDMIScopeAddresses, beatBytes: Int) extends LazyModule()(Parameters.empty) {

  val adcproc  = LazyModule(new HDMIProcADCNoInterpolation(params.adcprocParams, address.adcprocAddress, beatBytes))
  val fftproc  = LazyModule(new HDMIProcFFTNoInterpolation(params.fftprocParams, address.fftprocAddress, beatBytes))
  val asyncLog = LazyModule(new AsyncLogger(params.asyncParams, log2Ceil(128), false))

  // connect nodes
  asyncLog.inRealNode         := adcproc.datacounter0.streamNode
  asyncLog.inImagNode         := adcproc.datacounter1.streamNode
  asyncLog.inCutNode          := fftproc.datacounter0.streamNode
  asyncLog.inTresholdNode     := fftproc.datacounter1.streamNode
  asyncLog.inPeakNode         := fftproc.datacounter2.streamNode

  // define mem
  val bus = LazyModule(new AXI4Xbar)
  val mem = Some(bus.node)
  adcproc.mem.get := bus.node
  fftproc.mem.get := bus.node

  lazy val module = new LazyModuleImp(this) {
      // IO
      val io = IO(new HDMIScopeIO)

      // Wires
      val vsync   = Wire(Bool())
      val hsync   = Wire(Bool())
      val pixel_x = Wire(UInt(16.W))
      val pixel_y = Wire(UInt(16.W))
      val video_active = Wire(Bool())
      val video_data   = Wire(UInt(24.W))

      // video timing
      val timeGen = Module(new TimingGenerator(params = TimingGeneratorParams()))
      timeGen.clock := io.clk_pixel
      timeGen.reset := io.reset_hdmi
      video_active  := timeGen.io.video_active
      hsync   := timeGen.io.hsync
      vsync   := timeGen.io.vsync
      pixel_x := timeGen.io.pixel_x
      pixel_y := timeGen.io.pixel_y

      // tmds signaling
      val rgb2tmds = Module(new RGB2tmds)
      rgb2tmds.rst          := io.reset_hdmi
      rgb2tmds.pixelclock   := io.clk_pixel
      rgb2tmds.serialclock  := io.clk_serdes
      rgb2tmds.video_data   := video_data
      rgb2tmds.video_active := video_active
      rgb2tmds.hsync        := hsync
      rgb2tmds.vsync        := vsync
      io.clk_p  := rgb2tmds.clk_p
      io.clk_n  := rgb2tmds.clk_n
      io.data_p := rgb2tmds.data_p
      io.data_n := rgb2tmds.data_n

      withClockAndReset(io.clk_pixel, io.reset_hdmi){
        val frameBuffer = Module(new FrameBuffer(params.frameBufferParams, log2Ceil(128)))

        adcproc.module.start     := frameBuffer.io.start
        adcproc.module.triggered := frameBuffer.io.triggered
        adcproc.module.loadRegs  := frameBuffer.io.loadScaler
        fftproc.module.start     := frameBuffer.io.start
        fftproc.module.loadRegs  := frameBuffer.io.loadScaler

        // async
        asyncLog.module.clock2       := io.clk_pixel
        asyncLog.module.reset2       := io.reset_hdmi
        asyncLog.module.start        := adcproc.module.sync
        asyncLog.module.startFFT     := fftproc.module.sync
        asyncLog.module.pixel_x      := frameBuffer.io.x_location
        asyncLog.module.pixel_x_fft  := frameBuffer.io.x_location_fft
        asyncLog.module.scaler_y     := adcproc.module.scaler_y
        asyncLog.module.scaler_x     := adcproc.module.scaler_x
        asyncLog.module.fft_scaler_y := fftproc.module.cut_scaler_y
        asyncLog.module.fft_scaler_x := fftproc.module.cut_scaler_x
        frameBuffer.io.inReal        := asyncLog.module.io.out0
        frameBuffer.io.inImag        := asyncLog.module.io.out1
        frameBuffer.io.inCUT         := asyncLog.module.io.out2
        frameBuffer.io.inTreshold    := asyncLog.module.io.out3
        frameBuffer.io.inPeak        := asyncLog.module.io.out4
        frameBuffer.io.scaler_y      := asyncLog.module.scaler_y_out
        frameBuffer.io.scaler_x      := asyncLog.module.scaler_x_out
        frameBuffer.io.fft_scaler_y  := asyncLog.module.fft_scaler_y_out
        frameBuffer.io.fft_scaler_x  := asyncLog.module.fft_scaler_x_out

        // frameBuffer
        frameBuffer.clock           := io.clk_pixel
        frameBuffer.reset           := io.reset_hdmi
        frameBuffer.io.pixel_x      := pixel_x
        frameBuffer.io.pixel_y      := pixel_y
        frameBuffer.io.video_active := video_active
        video_data := frameBuffer.io.rgb
      }
    }
}

trait HDMIScopePins extends HDMIScope[FixedPoint] {
  def beatBytes: Int = 4

  // Generate AXI4 slave output
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  val ioInNode0 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 8)))
  val ioInNode1 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 4)))
  fftproc.flowcontrol.inNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 8)) := ioInNode0
  adcproc.trigger.streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 4)) := ioInNode1
  val in0 = InModuleBody { ioInNode0.makeIO() }
  val in1 = InModuleBody { ioInNode1.makeIO() }
}


class HDMIScopeParams(fftSize: Int = 1024) {
  val params : HDMIScopeParameters[FixedPoint] = HDMIScopeParameters (
    adcprocParams = (new HDMIProcADCNoInterpolationParams).params,
    fftprocParams = (new HDMIProcFFTNoInterpolationParams).params,
    asyncParams = AsyncLoggerParams(
      proto0    = FixedPoint(16.W, 10.BP),
      proto1    = FixedPoint(16.W, 10.BP),
      proto2    = FixedPoint( 8.W,  0.BP),
      dataSize = 1024
    ),
    frameBufferParams = FrameBufferParameters(
      proto0 = FixedPoint(16.W, 10.BP),
      proto1 = FixedPoint(16.W, 10.BP),
      proto2 = FixedPoint(8.W, 0.BP),
      imageParams = (new HD1080pNoInterpolation).params,
      beatBytes = 4
    )
  )
}

class HDMIScopeAddr(startAddress: BigInt = 0x0000) {
  val addresses = HDMIScopeAddresses (
    adcprocAddress = (new HDMIProcADCNoInterpolationAddr(startAddress + 0xD000)).addresses,
    fftprocAddress = (new HDMIProcFFTNoInterpolationAddr(startAddress + 0xE000)).addresses
  )
}

object HDMIScopeApp extends App
{
  val params = (new HDMIScopeParams).params
  val address = (new HDMIScopeAddr(0x0000)).addresses
  val lazyDut = LazyModule(new HDMIScope(params, address, 4) with HDMIScopePins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/HDMIScope"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

