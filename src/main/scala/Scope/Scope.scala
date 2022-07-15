package hdmi.scope 

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.{FixedPoint}
import dsptools.numbers._
import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

import hdmi._
import hdmi.frameBuffer._
import hdmi.preproc._

/* 1D processing parameters and addresses */
case class Proc1DParamsAndAddresses[T <: Data: Real: BinaryRepresentation] (
  procParams  : ProcFFT1DParameters[T]
)
/* 1D processing scaler parameters and addresses */
case class Scaler1DParamsAndAddresses (
  scalerParams  : Scaler1DParams,
  scalerAddress : AddressSet
)

/* 2D processing parameters and addresses */
case class Proc2DParamsAndAddresses (
  procParams  : ProcFFT2DParams
)

/* 2D processing scaler parameters and addresses */
case class Scaler2DParamsAndAddresses (
  scalerAddress : AddressSet
)

/* AsyncQueue parameters and addresses */
case class AsyncParamsAndAddresses (
  asyncParams : AsyncLoggerParams
)

// Scope parameters
case class ScopeParameters[T <: Data: Real: BinaryRepresentation] (
  proc1DParams   : Proc1DParamsAndAddresses[T],
  scaler1DParams : Scaler1DParamsAndAddresses,
  proc2DParams   : Proc2DParamsAndAddresses,
  scaler2DParams : Scaler2DParamsAndAddresses,
  asyncParams    : AsyncParamsAndAddresses,
  frameParams    : FrameBufferParameters[T]
)

// Scope IO
class ScopeIO extends Bundle {
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

class AXI4Scope[T <: Data : Real: BinaryRepresentation](params: ScopeParameters[T], beatBytes: Int)(implicit p: Parameters) extends Scope[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock {
  /* Optional memory mapped port */
  val bus = Some(LazyModule(new AXI4Xbar))
  override val mem = Some(bus.get.node)
  scaler1D.mem.get := bus.get.node
  scaler2D.mem.get := bus.get.node
}

abstract class Scope[T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: ScopeParameters[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {

  val proc1D   = LazyModule(new AXI4ProcFFT1D(params.proc1DParams.procParams, beatBytes))
  val scaler1D = LazyModule(new AXI4Scaler1DBlock(params.scaler1DParams.scalerParams, params.scaler1DParams.scalerAddress, beatBytes))
  val proc2D   = LazyModule(new AXI4ProcFFT2D(params.proc2DParams.procParams, 2))
  val scaler2D = LazyModule(new AXI4Scaler2DBlock(params.scaler2DParams.scalerAddress, beatBytes))
  val asyncQ   = LazyModule(new AsyncLogger(params.asyncParams.asyncParams) with AsyncLoggerOutputPins)

  val streamNode  = scaler1D.streamNode
  val streamNode2 = asyncQ.node_doppler.get
  // 1D connect nodes
  proc1D.flowcontrol.inNode := streamNode
  asyncQ.node_cut.get   := proc1D.datacounter0.streamNode
  asyncQ.node_tresh.get := proc1D.datacounter1.streamNode
  asyncQ.node_peak.get  := proc1D.datacounter2.streamNode

  // 2D connect nodes
  proc2D.streamNode := streamNode2

  lazy val module = new LazyModuleImp(this) {
      // IO
      val io = IO(new ScopeIO)

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
        val frameBuffer = Module(new FrameBuffer(params.frameParams, log2Ceil(128)))

        proc1D.module.start     := frameBuffer.io.start
        proc1D.module.loadRegs  := frameBuffer.io.loadScaler
        proc1D.module.i_scaler_x := scaler1D.module.scalerX

        // async
        asyncQ.module.io.clock2 := io.clk_pixel
        asyncQ.module.io.reset2 := io.reset_hdmi
        asyncQ.module.io.i_addr_x_1D.get   := frameBuffer.io.i_addr_x_1D
        asyncQ.module.io.i_scaler_y_1D.get := scaler1D.module.scalerY
        asyncQ.module.io.i_scaler_x_1D.get := scaler1D.module.scalerX
        frameBuffer.io.i_CUT      := asyncQ.o_cut.get.bits.data.asTypeOf(frameBuffer.io.i_CUT)
        frameBuffer.io.i_Treshold := asyncQ.o_tresh.get.bits.data.asTypeOf(frameBuffer.io.i_Treshold)
        frameBuffer.io.i_Peak     := asyncQ.o_peak.get.bits.data.asTypeOf(frameBuffer.io.i_Peak)
        // 2D
        if (params.proc2DParams.procParams.memParams.divideMem) {
          proc2D.module.io.i_addr_x.get := frameBuffer.io.o_addr_x_2D
          proc2D.module.io.i_addr_y.get := frameBuffer.io.o_addr_y_2D
        }
        else {
          proc2D.module.io.i_addr_x.get := Cat(frameBuffer.io.o_addr_y_2D, frameBuffer.io.o_addr_x_2D)
        }
        proc2D.module.io.i_scaler_data := scaler2D.module.io.o_scalerData
        frameBuffer.io.i_scaler := scaler2D.module.io.o_scalerAxis
        frameBuffer.io.i_FFT_2D := proc2D.module.io.o_data
        
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

trait AXI4ScopePins extends AXI4Scope[FixedPoint] {
  def beatBytes: Int = 4

  // Generate AXI4 slave output
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8, dataBits = beatBytes*8, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  val ioInNode0 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 6)))
  val ioInNode1 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
  streamNode  := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 6)) := ioInNode0
  streamNode2 := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := ioInNode1
  val in0 = InModuleBody { ioInNode0.makeIO() }
  val in1 = InModuleBody { ioInNode1.makeIO() }
}


class ScopeParams(rangeSize: Int = 512, dopplerSize: Int = 256, startAddress: BigInt = 0x0000) {
  val params : ScopeParameters[FixedPoint] = ScopeParameters(
    proc1DParams = Proc1DParamsAndAddresses(
      procParams = (new ProcFFT1DParams(rangeSize)).params
    ),
    scaler1DParams = Scaler1DParamsAndAddresses(
      scalerParams = Scaler1DParams(
        scale = log2Ceil(128),
      ),
      scalerAddress = AddressSet(startAddress + 0x0000, 0xFF)
    ),
    proc2DParams = Proc2DParamsAndAddresses(
      procParams = ProcFFT2DParams(
        memParams = Mem2DBlockParams(
            dim1 = rangeSize,
            dim2 = dopplerSize,
            blockRamDim = 2048,
            divideMem = false,
        ),
        pipe = true,
      )
    ),
    scaler2DParams = Scaler2DParamsAndAddresses(
      scalerAddress = AddressSet(startAddress + 0x0100, 0xFF)
    ),
    asyncParams = AsyncParamsAndAddresses(
      asyncParams = AsyncLoggerParams(
        fft_1D = true,
        fft_2D = true,
        s_width_1D = log2Ceil(128),
        s_width_2D = log2Ceil(128),
        dataSize = rangeSize
      )
    ),
    frameParams = FrameBufferParameters(
      proto0 = FixedPoint(16.W, 10.BP),
      proto1 = FixedPoint(16.W, 10.BP),
      proto2 = FixedPoint( 8.W,  0.BP),
      imageParams = (new HD1080pNoInterpolation).params,
      beatBytes = 4
    )
  )
}

object ScopeApp extends App
{
  implicit val p: Parameters = Parameters.empty
  val params = (new ScopeParams).params
  val lazyDut = LazyModule(new AXI4Scope(params, 4) with AXI4ScopePins)

  (new ChiselStage).execute(Array("--target-dir", "verilog/Scope"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}

