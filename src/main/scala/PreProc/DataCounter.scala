package hdmi.preproc

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.ChiselEnum

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// AsyncLogger parameters
case class DataCounterParams(
  scalerSize : Int,
  dataSize : Int,   // Data logger size
)

abstract class DataCounter [D, U, E, O, B <: Data](params: DataCounterParams, beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B]{

    val streamNode = AXI4StreamIdentityNode()

    lazy val module = new LazyModuleImp(this) {
        // IOs
        val (in, _)  = streamNode.in(0)
        val (out, _) = streamNode.out(0)
        
        // Additional IOs
        val start  = IO(Input(Bool()))
        val scaler = IO(Input(UInt(log2Ceil(params.scalerSize).W)))

        // Additional interpolation factor register
        val r_scaler = RegInit(0.U(log2Ceil(params.scalerSize).W))

        // Counter
        val counter = RegInit(0.U(log2Ceil(params.dataSize).W))

        // Signal definitions
        val read  = in.fire()
        val write = out.fire()

        // Data
        out.bits := in.bits

        // FSM
        object State extends ChiselEnum {
            val sInit, sValid, sInvalid = Value
        }
        val state = RegInit(State.sInit)

        when (state === State.sInit) {
            // not ready
            out.valid := false.B
            in.ready  := false.B
            counter   := 0.U

            // State select 
            when (start === 1.U) {
                state := State.sValid
                r_scaler := (1.U << scaler) - 1.U
            }
            .otherwise {
                state := State.sInit
            }
        }
        .elsewhen(state === State.sValid) {
            when (counter >= r_scaler) {
                out.valid := in.valid
                in.ready := out.ready
            }

            // State select 
            when (read && counter === (params.dataSize.U + r_scaler - 1.U)) {
                state := State.sInvalid
                counter := 0.U
            }
            .otherwise {
                // increment data counter
                when(read) {
                    counter  := counter + 1.U
                }
            }
        }
        .elsewhen(state === State.sInvalid) {
            counter := 0.U
            
            // output data is invalid
            out.valid := false.B
            in.ready  := true.B
            
            // State select 
            when (start === 1.U) {
                state := State.sValid
            }
            .otherwise {
                state := State.sInvalid
            }
        }
        .otherwise {
            out.valid := false.B
            in.ready  := false.B
            state     := State.sInit
        }
    }
}

class AXI4DataCounterBlock(params: DataCounterParams, beatBytes : Int)(implicit p: Parameters) extends DataCounter[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock {
  override val mem = None
}


trait AXI4DataCounterStandaloneBlock extends AXI4DspBlock {

  val beatBytes = 4

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

object DataCounterApp extends App
{
  implicit val p: Parameters = Parameters.empty
  val params = DataCounterParams (
      dataSize = 512,
      scalerSize = 7,
  )
  
  val lazyDut = LazyModule(new AXI4DataCounterBlock(params, beatBytes=4) with AXI4DataCounterStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/DataCounter"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}