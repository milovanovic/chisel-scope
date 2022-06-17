package hdmi.preproc

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.ChiselEnum

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// AsyncLogger parameters
case class FlowControlParams(
  dataSize : Int,   // Data logger size
)

class FlowControl(params: FlowControlParams, beatBytes: Int) extends LazyModule()(Parameters.empty) {
    val inNode   = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
    val outNode0 = AXI4StreamMasterNode(AXI4StreamMasterParameters(name = "cut", n = 2))
    val outNode1 = AXI4StreamMasterNode(AXI4StreamMasterParameters(name = "treshold", n = 2))
    val outNode2 = AXI4StreamMasterNode(AXI4StreamMasterParameters(name = "peak", n = 1))

    lazy val module = new LazyModuleImp(this) {
        val (in, _)  = inNode.in(0)
        val (out0, _) = outNode0.out(0)
        val (out1, _) = outNode1.out(0)
        val (out2, _) = outNode2.out(0)

        val start = IO(Input(Bool()))
        val sync  = IO(Output(Bool()))

        // Signal definitions
        val read  = in.fire()
        
        // Registers
        val r_trig = RegInit(false.B)
        val r_cnt_stop = RegInit(0.U(32.W))
        val r_counter  = RegInit(0.U(log2Ceil(params.dataSize).W))

        // Signals
        val cut      = in.bits.data(26,11)
        val treshold = in.bits.data(42,27)
        val peak     = Cat(0.U(7.W), in.bits.data(0))

        // FSM
        object State extends ChiselEnum {
            val sInit, sActive = Value
        }
        val state = RegInit(State.sInit)

        sync := start

        // Input side must be always ready to avoid stopping fft
        in.ready  := true.B

        // Data
        out0.bits.data := cut
        out0.bits.last := in.bits.last
        out1.bits.data := treshold
        out1.bits.last := in.bits.last
        out2.bits.data := peak
        out2.bits.last := in.bits.last

        // increment data r_cnt_stop
        when(read === 0.U) {
            r_cnt_stop := r_cnt_stop + 1.U
        }
        .otherwise {
          r_cnt_stop := 0.U
        }

        when (state === State.sInit) {
            // output data is invalid
            out0.valid := false.B
            out1.valid := false.B
            out2.valid := false.B
            when(start) {
              r_trig := true.B
            }
            // State select 
            when (r_trig === 1.U && r_cnt_stop > 20.U) {
                state := State.sActive
                r_trig := false.B
            }
            .otherwise {
                state := State.sInit
            }
        }
        .elsewhen(state === State.sActive) {
            // pass valid to output
            out0.valid := in.valid
            out1.valid := in.valid
            out2.valid := in.valid
            
            // count data
             when(read) {
                r_counter  := r_counter + 1.U
            }

            // State select 
            when (read & r_counter === (params.dataSize-1).U) {
                state := State.sInit
            }
        }
        .otherwise {
            out0.valid := false.B
            out1.valid := false.B
            out2.valid := false.B
            state      := State.sInit
        }
    }
}


trait FlowControlPins extends FlowControl {
  def beatBytes: Int = 8

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
  val ioOutNode0 = BundleBridgeSink[AXI4StreamBundle]()
  val ioOutNode1 = BundleBridgeSink[AXI4StreamBundle]()
  val ioOutNode2 = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode0 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode0
  ioOutNode1 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode1
  ioOutNode2 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode2
  inNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out0 = InModuleBody { ioOutNode0.makeIO() }
  val out1 = InModuleBody { ioOutNode1.makeIO() }
  val out2 = InModuleBody { ioOutNode2.makeIO() }
}

object FlowControlApp extends App
{
  implicit val p: Parameters = Parameters.empty
  val params = FlowControlParams (
      dataSize = 1024,
  )
  
  val lazyDut = LazyModule(new FlowControl(params, beatBytes=4) with FlowControlPins)
  (new ChiselStage).execute(Array("--target-dir", "verilog/FlowControl"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}