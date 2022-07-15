package hdmi.preproc

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.IO

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// Mem2DBlock parameters
case class Mem2DBlockParams(
    dim1 : Int,
    dim2 : Int,
    blockRamDim : Int,
    divideMem   : Boolean,
)

// Mem2DBlock Bundle
class Mem2DBlockIO(params: Mem2DBlockParams, beatBytes: Int) extends Bundle {
    val i_data = Input(UInt((8*beatBytes).W))
    val i_en   = Input(Bool())
    val o_data = Output(UInt((8*beatBytes).W))
    val i_addr_x = if (params.divideMem) Some(Input(UInt(log2Ceil(params.blockRamDim).W))) else Some(Input(UInt(log2Ceil(params.dim1 * params.dim2).W)))
    val i_addr_y = if (params.divideMem) Some(Input(UInt(log2Ceil(params.dim1 * params.dim2 / params.blockRamDim).W))) else Some(Input(UInt(0.W)))

    override def cloneType: this.type = Mem2DBlockIO(params, beatBytes).asInstanceOf[this.type]
}
object Mem2DBlockIO {
  def apply(params: Mem2DBlockParams, beatBytes: Int): Mem2DBlockIO = new Mem2DBlockIO(params, beatBytes)
}

class Mem2DBlock(params: Mem2DBlockParams, beatBytes: Int) extends LazyModule()(Parameters.empty){
    // IO
    lazy val io = Wire(new Mem2DBlockIO(params, beatBytes))

    lazy val module = new LazyModuleImp(this) {
        if (params.divideMem) {
            // registers
            val r_counter = RegInit(0.U(log2Ceil(params.blockRamDim).W))
            val r_select  = RegInit(0.U(log2Ceil(params.dim1 * params.dim2 / params.blockRamDim).W))
            dontTouch(r_counter)
            dontTouch(r_select)
            r_counter.suggestName("r_counter")
            r_select.suggestName("r_select")

            // Memory
            val mem2d = List.fill(params.dim1 * params.dim2 / params.blockRamDim) {SyncReadMem(params.blockRamDim, io.i_data.cloneType)}
            // Condition for MuxCase
            val cond = Seq.tabulate(mem2d.size)(n => (RegNext(io.i_addr_y.get,0.U) === n.U, mem2d(n)(io.i_addr_x.get)))

            // Increment r_select
            when (io.i_en === true.B) { r_select := r_select + 1.U }

            // increment r_counter
            when ((io.i_en === true.B) && (r_select === ((params.dim1 * params.dim2 / params.blockRamDim) - 1).U)) { r_counter := r_counter + 1.U }

            for (i <- 0 until mem2d.size) {
                // input
                when ((io.i_en === true.B) && (r_select === i.U)) {
                    mem2d(i)(r_counter) := io.i_data
                }
            }
            io.o_data := MuxCase(0.U, cond)
        }
        else {
            // registers
            val r_counter = RegInit(0.U(log2Ceil(params.dim1).W))
            val r_select  = RegInit((params.dim2/2).U(log2Ceil(params.dim2).W))

            // Memory
            val mem2d = SyncReadMem(params.dim1 * params.dim2, io.i_data.cloneType)

            // Increment r_select
            when (io.i_en === true.B) { r_select := r_select + 1.U }
            // increment r_counter
            when ((io.i_en === true.B) && (r_select === (params.dim2/2 - 1).U)) { r_counter := r_counter + 1.U }
            
            when (io.i_en === true.B) {
                mem2d(r_select*params.dim1.U + r_counter) := io.i_data
            }
            io.o_data := mem2d(io.i_addr_x.get)
        }
    }
}

trait Mem2DBlockStandaloneBlock extends Mem2DBlock{
    // IO
    def makeCustomIO(): Mem2DBlockIO = {
        val io2: Mem2DBlockIO = IO(io.cloneType)
        io2.suggestName("io")
        io2 <> io
        io2
    }
    val ioBlock = InModuleBody { makeCustomIO() }
}

object Mem2DBlockApp extends App
{
  implicit val p: Parameters = Parameters.empty
  val params = Mem2DBlockParams (
      dim1 = 512,
      dim2 = 256,
      blockRamDim = 2048,
      divideMem = false,
  )

  val lazyDut = LazyModule(new Mem2DBlock(params, 2) with Mem2DBlockStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/Mem2DBlock"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}