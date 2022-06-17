package hdmi.preproc

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.FixedPoint
import chisel3.internal.requireIsChiselType
import dsptools.numbers._

// ZeroOrderHold parameters
case class ZeroOrderHoldParams[T <: Data: Real](
  proto: T,  // input/output data type
  size : Int // Zero-order hold size
) {
  requireIsChiselType(proto,  s"($proto) must be chisel type")
}

// ZeroOrderHold IO
class ZeroOrderHoldIO[T <: Data: Real] (params: ZeroOrderHoldParams[T], scalerSize: Int, beatBytes: Int) extends Bundle {
  val in      = Flipped(Decoupled(params.proto))
  val out     = Decoupled(params.proto)
  val scaler  = Input(UInt(log2Ceil(scalerSize).W))
  val loadReg = Input(Bool())

  override def cloneType: this.type = ZeroOrderHoldIO(params, scalerSize, beatBytes).asInstanceOf[this.type]
}

object ZeroOrderHoldIO {
  def apply[T <: Data : Real](params: ZeroOrderHoldParams[T], scalerSize: Int, beatBytes: Int): ZeroOrderHoldIO[T] = new ZeroOrderHoldIO(params, scalerSize, beatBytes)
}

class ZeroOrderHold [T <: Data: Real: BinaryRepresentation] (val params: ZeroOrderHoldParams[T], scalerSize: Int, beatBytes: Int) extends Module {
    val io =  IO(ZeroOrderHoldIO(params, scalerSize, beatBytes))

    // Signal definitions
    val hold      = RegInit(0.U.asTypeOf(params.proto))
    val counter   = RegInit(0.U((log2Ceil(params.size) + scalerSize + 1).W))
    val scalerReg = RegInit(0.U(log2Ceil(scalerSize).W))
    val read      = io.in.fire()
    val write     = io.out.fire()
    val validReg  = RegInit(false.B)
    val readyReg  = RegInit(false.B)

    // FSM
    object State extends ChiselEnum {
        val sIdle, sRead, sWrite = Value
    }
    val state = RegInit(State.sIdle)
    io.out.valid := validReg
    io.out.bits  := hold

    // FSM
    when(io.loadReg) {
      state     := State.sIdle
      scalerReg := io.scaler
      counter   := 0.U
      readyReg  := false.B
      validReg  := false.B
      io.in.ready := false.B
    }
    .otherwise{
      when (state === State.sIdle) {
          io.in.ready := false.B
          readyReg := false.B
          validReg := false.B
          counter  := 0.U
          state    := State.sRead
      }
      .elsewhen (state === State.sRead) {
          io.in.ready := readyReg
          readyReg    := true.B
          when(read) {
            hold     := io.in.bits
            state    := State.sWrite
            validReg := true.B
            readyReg := false.B
          }
      }
      .elsewhen(state === State.sWrite){
          // counter increment
          when(write) {
            when(counter === (params.size.U << scalerReg) - 1.U) {
              io.in.ready := true.B
              counter := 0.U
              when(read) {
                state := State.sWrite
                hold  := io.in.bits
              }
              .otherwise {
                state := State.sRead
                validReg := false.B
              }
            }
            .otherwise {
              counter := counter + 1.U
              io.in.ready := false.B
            }
          }
          .otherwise {
            io.in.ready := false.B
          }
      }
      .otherwise {
        io.in.ready := false.B
        readyReg := false.B
        validReg := false.B
        state    := State.sIdle
        counter  := 0.U
      }
    }
}

object ZeroOrderHoldApp extends App
{
  val params: ZeroOrderHoldParams[FixedPoint] = ZeroOrderHoldParams(
    proto = FixedPoint(16.W, 14.BP),
    size  = 4
  )
  (new ChiselStage).execute(Array("--target-dir", "verilog/ZeroOrderHold"), Seq(ChiselGeneratorAnnotation(() => new ZeroOrderHold(params, 7, 4))))
}