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
class ZOHIO (val dataWidth: Int, val scalerSize: Int) extends Bundle {
  val start  = Input(Bool())
  val i_data = Input(UInt(dataWidth.W))
  val o_data = Output(UInt(dataWidth.W))
  val scaler = Input(UInt(scalerSize.W))
}

object ZOHIO {
  def apply(dataWidth: Int, scalerSize: Int): ZOHIO = new ZOHIO(dataWidth, scalerSize)
}

class ZOH (val params: ZOHParams, scalerSize: Int) extends Module {
 val io =  IO(ZOHIO(params.width, scalerSize))

  // Signal definitions
  val hold      = RegInit(0.U((params.width).W))
  val counter   = RegInit(0.U((log2Ceil(params.size) + scalerSize + 1).W))
  val scalerReg = RegInit(0.U(scalerSize.W))
  val r_start   = RegNext(RegNext(io.start, false.B), false.B)

  // FSM
  object State extends ChiselEnum {
    val sIdle, sRead, sWrite = Value
  }
  val state = RegInit(State.sIdle)
  scalerReg := io.scaler

  // FSM
  when (state === State.sIdle) {
    io.o_data := 0.U
    counter   := 0.U
    when(io.start) {state := State.sRead}
  }
  .elsewhen (state === State.sRead) {
    when(r_start) {
      io.o_data := io.i_data
      hold      := io.i_data
      state     := State.sWrite
      counter   := counter + 1.U
    }
    .otherwise {
      io.o_data := io.i_data
      hold      := io.i_data
      counter   := 0.U
      state     := State.sIdle
    }
    
  }
  .elsewhen (state === State.sWrite){
    io.o_data := hold
    // counter increment
    when (scalerReg === 0.U && params.size.U === 1.U) {
      when(counter === (params.size.U << scalerReg)) {
        counter := 0.U
        when(r_start) { state := State.sRead }
        .otherwise    { state := State.sIdle }
      }
      .otherwise {
        counter := counter + 1.U
      }
    }
    .otherwise {
      when(counter === (params.size.U << scalerReg) - 1.U) {
        counter := 0.U
        when(r_start) { state := State.sRead }
        .otherwise    { state := State.sIdle }
      }
      .otherwise {
        counter := counter + 1.U
      }
    }
  }
  .otherwise {
    io.o_data := io.i_data
    state     := State.sIdle
    counter   := 0.U
  }
}

object ZOHApp extends App
{
  val params = ZOHParams(
    width = 16,
    size  = 4
  )
  (new ChiselStage).execute(Array("--target-dir", "verilog/ZOH"), Seq(ChiselGeneratorAnnotation(() => new ZOH(params, 7))))
}