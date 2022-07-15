package hdmi.preproc

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.IO

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// Mem2D parameters
case class Mem2DParams(
    dim1 : Int,
    dim2 : Int,
    blockRamDim : Int,
    divideMem   : Boolean,
)

// Mem2D Bundle
class Mem2DIO(params: Mem2DParams) extends Bundle {
    val i_addr_x = if (params.divideMem) Some(Input(UInt(log2Ceil(params.blockRamDim).W))) else Some(Input(UInt(log2Ceil(params.dim1 * params.dim2).W)))
    val i_addr_y = if (params.divideMem) Some(Input(UInt(log2Ceil(params.dim1 * params.dim2 / params.blockRamDim).W))) else Some(Input(UInt(0.W)))

    override def cloneType: this.type = Mem2DIO(params).asInstanceOf[this.type]
}
object Mem2DIO {
  def apply(params: Mem2DParams): Mem2DIO = new Mem2DIO(params)
}

class Mem2D(params: Mem2DParams) extends LazyModule()(Parameters.empty){
    // Stream node
    val streamNode = AXI4StreamIdentityNode()
    // IO
    lazy val io = Wire(new Mem2DIO(params))

    lazy val module = new LazyModuleImp(this) {
        // IOs
        val (in, _)  = streamNode.in(0)
        val (out, _) = streamNode.out(0)

        if (params.divideMem) {
            // registers
            val r_counter = RegInit(0.U(log2Ceil(params.blockRamDim).W))
            val r_select  = RegInit(0.U(log2Ceil(params.dim1 * params.dim2 / params.blockRamDim).W))
            dontTouch(r_counter)
            dontTouch(r_select)
            r_counter.suggestName("r_counter")
            r_select.suggestName("r_select")

            // Memory
            val mem2d = Array.fill(params.dim1 * params.dim2 / params.blockRamDim) {SyncReadMem(params.blockRamDim, in.bits.data.cloneType)}
            // Condition for MuxCase
            val cond = Seq.tabulate(mem2d.size)(n => (RegNext(io.i_addr_y.get,0.U) === n.U, mem2d(n)(io.i_addr_x.get)))

            // Increment r_counter
            when (in.fire()) { r_select := r_select + 1.U }

            // increment r_select
            when (in.fire() && (r_select === ((params.dim1 * params.dim2 / params.blockRamDim) - 1).U)) { r_counter := r_counter + 1.U }

            for (i <- 0 until mem2d.size) {
                // input
                when (in.fire() && (r_select === i.U)) {
                    mem2d(i)(r_counter) := in.bits.data
                }
            }
            out.bits.data := MuxCase(0.U, cond)
        }
        else {
            // registers
            val r_counter = RegInit(0.U(log2Ceil(params.dim1).W))
            val r_select  = RegInit(0.U(log2Ceil(params.dim2).W))

            // Memory
            val mem2d = SyncReadMem(params.dim1 * params.dim2, in.bits.data.cloneType)

            // Increment r_select
            when (in.fire()) { r_select := r_select + 1.U }
            // increment r_counter
            when ((in.fire()) && (r_select === (params.dim2 - 1).U)) { r_counter := r_counter + 1.U }
            
            when (in.fire()) {
                mem2d(r_select*params.dim1.U + r_counter) := in.bits.data
            }
            out.bits.data := mem2d(io.i_addr_x.get)
        }

        // in.ready and out.valid are always true
        in.ready := true.B
        out.valid := RegNext(out.ready, false.B)
    }
}

trait Mem2DStandaloneBlock extends Mem2D{
    def beatBytes: Int = 2

    val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
    val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

    ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode

    val in = InModuleBody { ioInNode.makeIO() }
    val out = InModuleBody { ioOutNode.makeIO() }

    // IO
    def makeCustomIO(): Mem2DIO = {
        val io2: Mem2DIO = IO(io.cloneType)
        io2.suggestName("io")
        io2 <> io
        io2
    }
    val ioBlock = InModuleBody { makeCustomIO() }
}

object Mem2DApp extends App
{
  implicit val p: Parameters = Parameters.empty
  val params = Mem2DParams (
      dim1 = 1024,
      dim2 = 256,
      blockRamDim = 2048,
      divideMem = true,
  )

  val lazyDut = LazyModule(new Mem2D(params) with Mem2DStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/Mem2D"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}