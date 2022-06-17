package hdmi.preproc

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.FixedPoint

import dsptools.numbers._

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// Scaler2RGB parameters
case class Scaler2RGBParams[T <: Data: Real : BinaryRepresentation](
  proto : T, // Input/output data type
)

class Scaler2RGB[T <: Data: Real : BinaryRepresentation] (params: Scaler2RGBParams[T]) extends LazyModule()(Parameters.empty){

    val inNode     = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
    val outNode    = AXI4StreamMasterNode(AXI4StreamMasterParameters(n = 3, u = 0, numMasters = 1))
    val streamNode = NodeHandle(inNode, outNode)

    lazy val module = new LazyModuleImp(this) {
        // IOs
        val (in, _)  = inNode.in(0)
        val (out, _) = outNode.out(0)

        // Connect ready, valid and last signals
        in.ready := out.ready
        out.valid := in.valid
        out.bits.last := in.bits.last

        // parameters
        val maxValue = (1 << params.proto.getWidth).toLong
        val scaler = log2Ceil(1 << (params.proto.getWidth - 8))

        // Temporary data
        val data = Wire(UInt((params.proto.getWidth + 2).W))
        val dataOut = Wire(UInt(8.W))

        data := (maxValue.U - in.bits.data) << 2

        // Generate output
        when (data < (1*maxValue).U) {
            dataOut := data >> scaler
            out.bits.data := Cat(255.U(8.W), dataOut, 0.U(8.W))
        }
        .elsewhen (data < (2*maxValue).U) {
            dataOut := (data - (1*maxValue).U) >> scaler
            out.bits.data := Cat(255.U(8.W) - dataOut, 255.U(8.W), 0.U(8.W))
        }
        .elsewhen (data < (3*maxValue).U) {
            dataOut := (data-(2*maxValue).U) >> scaler
            out.bits.data := Cat(0.U(8.W), 255.U(8.W), dataOut)
        }
        .elsewhen (data < (4*maxValue).U) {
            dataOut := (data-(3*maxValue).U) >> scaler
            out.bits.data := Cat(0.U(8.W), 255.U(8.W) - dataOut, 255.U(8.W))
        }
        .otherwise {
            dataOut := 0.U
            out.bits.data := Cat(0.U(8.W), 0.U(8.W), 255.U(8.W))
        }
    }
}

trait Scaler2RGBStandaloneBlock[T <: Data]{
  self: Scaler2RGB[T] =>

  def beatBytes: Int = 2

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

object Scaler2RGBApp extends App
{
  implicit val p: Parameters = Parameters.empty
  val params = Scaler2RGBParams[FixedPoint] (
      proto = FixedPoint(16.W, 14.BP)
  )

  val lazyDut = LazyModule(new Scaler2RGB(params) with Scaler2RGBStandaloneBlock[FixedPoint])
  (new ChiselStage).execute(Array("--target-dir", "verilog/Scaler2RGB"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}