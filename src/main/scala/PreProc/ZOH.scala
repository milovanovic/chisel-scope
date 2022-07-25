package hdmi.preproc

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.ChiselEnum

// ZOH parameters
case class ZOHParams(
  width: Int, // input/output data width
  size : Int  // Zero-order hold size
)

// ZOH IO
class ZOHIO (val dataWidth: Int, val scalerSize: Int, val beatBytes: Int) extends Bundle {
  val i_data  = Input(UInt(dataWidth.W))
  val i_ready = Output(Bool())
  val i_valid = Input(Bool())
  val o_data  = Output(UInt(dataWidth.W))
  val scaler  = Input(UInt(log2Ceil(scalerSize).W))
  val loadReg = Input(Bool())
}

object ZOHIO {
  def apply(dataWidth: Int, scalerSize: Int, beatBytes: Int): ZOHIO = new ZOHIO(dataWidth, scalerSize, beatBytes)
}

class ZOH (val params: ZOHParams, scalerSize: Int, beatBytes: Int) extends Module {
 val io =  IO(ZOHIO(params.width, scalerSize, beatBytes))

  // Signal definitions
  val hold      = RegInit(0.U((params.width).W))
  val counter   = RegInit(0.U((log2Ceil(params.size) + scalerSize + 1).W))
  val scalerReg = RegInit(0.U(log2Ceil(scalerSize).W))
  val read      = io.i_ready && io.i_valid
  val readyReg  = RegInit(false.B)

  // FSM
  object State extends ChiselEnum {
    val sIdle, sRead, sWrite = Value
  }
  val state = RegInit(State.sIdle)
  io.o_data  := hold
  io.i_ready := readyReg

  // Read scaler register
  when (state === State.sIdle) {
    scalerReg := io.scaler
    readyReg  := false.B
    counter   := 0.U
    state     := State.sRead
  }
  // Read input data
  .elsewhen (state === State.sRead) {
    readyReg    := true.B
    when(read) {
      io.o_data := io.i_data
      hold     := io.i_data
      state    := State.sWrite
      readyReg := false.B
      counter := counter + 1.U
    }
  }
  .elsewhen(state === State.sWrite){
    // counter increment
    when(counter === (params.size.U << scalerReg) - 1.U) {
      readyReg := true.B
      counter  := 0.U
      state := State.sRead
    }
    .otherwise {
      counter := counter + 1.U
    }
  }
  .otherwise {
    readyReg := false.B
    state    := State.sIdle
    counter  := 0.U
  }
}

object ZOHApp extends App
{
  val params = ZOHParams(
    width = 16,
    size  = 4
  )
  (new ChiselStage).execute(Array("--target-dir", "verilog/ZOH"), Seq(ChiselGeneratorAnnotation(() => new ZOH(params, 7, 4))))
}