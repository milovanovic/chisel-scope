package hdmi.preproc

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.IO

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// Scaler2RGBBlock Bundle
class Scaler2RGBBlockIO(beatBytes: Int) extends Bundle {
    val i_data = Input(UInt((beatBytes*8).W))
    val o_data = Output(UInt(24.W))
    val i_max  = Input(UInt(log2Ceil(beatBytes*8 + 1).W))

    override def cloneType: this.type = Scaler2RGBBlockIO(beatBytes).asInstanceOf[this.type]
}
object Scaler2RGBBlockIO {
  def apply(beatBytes: Int): Scaler2RGBBlockIO = new Scaler2RGBBlockIO(beatBytes)
}

class Scaler2RGBBlock(beatBytes: Int) extends LazyModule()(Parameters.empty){
    // IOs
    lazy val io = Wire(new Scaler2RGBBlockIO(beatBytes))

    lazy val module = new LazyModuleImp(this) {

        // Maximum value (number of bits)
        val r_max = RegNext(RegNext(io.i_max, 1.U), 1.U)
        val r_maxValue = RegNext(1.U << io.i_max, 0.U)

        val scaler = RegInit(0.U((beatBytes*8 - 8 + 1).W))
        when(r_max >= 8.U) {
            scaler := r_max - 8.U
        }
        .otherwise {
            scaler := 8.U - r_max
        }
        

        // Temporary data
        val data = Wire(UInt((io.i_data.getWidth + 2).W))
        // val dataOut = Wire(UInt(8.W))
        val dataOut0 = Wire(UInt(8.W))
        val dataOut1 = Wire(UInt(8.W))
        val dataOut2 = Wire(UInt(8.W))
        val dataOut3 = Wire(UInt(8.W))

        val cond0 = r_maxValue
        val cond1 = r_maxValue << 1
        val cond2 = cond0 + cond1
        val cond3 = r_maxValue << 2

        // Saturate if data > r_max
        when (r_maxValue > io.i_data) { data := (r_maxValue - 1.U - io.i_data) << 2 }
        .otherwise { data := 0.U }

        // Re-calculate data
        when(r_max >= 8.U) {
            dataOut0 := data >> scaler
            dataOut1 := (data - (r_maxValue)) >> scaler
            dataOut2 := (data - (r_maxValue << 1)) >> scaler
            dataOut3 := (data - ((r_maxValue << 1) + r_maxValue)) >> scaler
        }
        .otherwise {
            dataOut0 := data << scaler
            dataOut1 := (data - (r_maxValue)) << scaler
            dataOut2 := (data - (r_maxValue << 1)) << scaler
            dataOut3 := (data - ((r_maxValue << 1) + r_maxValue)) << scaler
        }

        // Generate output
        when (data < cond0) {
            io.o_data := Cat(255.U(8.W), dataOut0, 0.U(8.W))
        }
        .elsewhen (data < cond1) {
            io.o_data := Cat(255.U(8.W) - dataOut1, 255.U(8.W), 0.U(8.W))
        }
        .elsewhen (data < cond2) {
            io.o_data := Cat(0.U(8.W), 255.U(8.W), dataOut2)
        }
        .elsewhen (data < cond3) {
            io.o_data := Cat(0.U(8.W), 255.U(8.W) - dataOut3, 255.U(8.W))
        }
        .otherwise {
            io.o_data := Cat(0.U(8.W), 0.U(8.W), 255.U(8.W))
        }
    }
}

trait Scaler2RGBBlockPins extends Scaler2RGBBlock {
    // IO
    def makeCustomIO(): Scaler2RGBBlockIO = {
        val io2: Scaler2RGBBlockIO = IO(io.cloneType)
        io2.suggestName("io")
        io2 <> io
        io2
    }
    val ioBlock = InModuleBody { makeCustomIO() }
}

object Scaler2RGBBlockApp extends App
{
  implicit val p: Parameters = Parameters.empty

  val lazyDut = LazyModule(new Scaler2RGBBlock(2) with Scaler2RGBBlockPins)
  (new ChiselStage).execute(Array("--target-dir", "verilog/Scaler2RGB"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}