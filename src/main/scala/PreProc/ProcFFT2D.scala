package hdmi.preproc

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// ProcFFT2D parameters
case class ProcFFT2DParams(
    memParams : Mem2DBlockParams,
    pipe : Boolean,
)

// ProcFFT1D Addresses
case class ProcFFT2DAddresses (
  scalerAddress : AddressSet
)

// ProcFFT2D Bundle
class ProcFFT2DIO(val params: Mem2DBlockParams) extends Bundle {
    val i_scaler_data = Input(UInt(5.W))
    val o_data   = Output(UInt(24.W))
    val i_addr_x = if (params.divideMem) Some(Input(UInt(log2Ceil(params.blockRamDim).W))) else Some(Input(UInt(log2Ceil(params.dim1 * params.dim2).W)))
    val i_addr_y = if (params.divideMem) Some(Input(UInt(log2Ceil(params.dim1 * params.dim2 / params.blockRamDim).W))) else Some(Input(UInt(0.W)))
}
object ProcFFT2DIO {
  def apply(params: Mem2DBlockParams): ProcFFT2DIO = new ProcFFT2DIO(params)
}

class AXI4ProcFFT2D(params: ProcFFT2DParams, beatBytes: Int)(implicit p: Parameters) extends ProcFFT2D[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock {
  /* Optional memory mapped port */
  override val mem = None
}

abstract class ProcFFT2D[D, U, E, O, B <: Data] (params: ProcFFT2DParams, beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] {
    val streamNode = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())

    // Modules
    val mem2d = LazyModule(new Mem2DBlock(params.memParams, beatBytes) with Mem2DBlockStandaloneBlock)
    val s2rgb = LazyModule(new Scaler2RGBBlock(beatBytes) with Scaler2RGBBlockPins)

    lazy val module = new LazyModuleImp(this) {
        // IOs
        val (in, _) = streamNode.in(0)

        // IO
        val io = IO(new ProcFFT2DIO(params.memParams))

        in.ready := true.B
        mem2d.ioBlock.i_en   := in.valid
        mem2d.ioBlock.i_data := in.bits.data
        mem2d.ioBlock.i_addr_x.get := io.i_addr_x.get
        mem2d.ioBlock.i_addr_y.get := io.i_addr_y.get

        if (params.pipe) s2rgb.ioBlock.i_data := RegNext(mem2d.ioBlock.o_data, 0.U)
        else s2rgb.ioBlock.i_data := mem2d.ioBlock.o_data

        
        if (params.pipe) io.o_data := RegNext(s2rgb.ioBlock.o_data, 0.U)
        else io.o_data := s2rgb.ioBlock.o_data
        
        // Data scaler
        s2rgb.ioBlock.i_max := io.i_scaler_data
    }
}

trait AXI4ProcFFT2DPins extends AXI4ProcFFT2D {
    def beatBytes: Int = 2

    // Node
    val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode
    val in = InModuleBody { ioInNode.makeIO() }
}

object ProcFFT2DApp extends App
{
  implicit val p: Parameters = Parameters.empty
  val params = ProcFFT2DParams (
    memParams = Mem2DBlockParams (
        dim1 = 512,
        dim2 = 256,
        blockRamDim = 2048,
        divideMem = false,
    ),
    pipe = true,
  )

  val lazyDut = LazyModule(new AXI4ProcFFT2D(params, 2) with AXI4ProcFFT2DPins)
  (new ChiselStage).execute(Array("--target-dir", "verilog/ProcFFT2D"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}