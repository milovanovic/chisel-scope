package hdmi.preproc

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.ChiselEnum

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// AsyncLogger parameters
case class FFTDataSplitterParams(
  dataSize : Int,   // Data logger size
)

class FFTDataSplitter(params: FFTDataSplitterParams) extends LazyModule()(Parameters.empty) {
  val inNode   = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  val outNode0 = AXI4StreamMasterNode(AXI4StreamMasterParameters(name = "cut", n = 2))
  val outNode1 = AXI4StreamMasterNode(AXI4StreamMasterParameters(name = "treshold", n = 2))
  val outNode2 = AXI4StreamMasterNode(AXI4StreamMasterParameters(name = "peak", n = 1))

  lazy val module = new LazyModuleImp(this) {
    val (in, _)   = inNode.in(0)
    val (out0, _) = outNode0.out(0)
    val (out1, _) = outNode1.out(0)
    val (out2, _) = outNode2.out(0)

    val read = IO(Input(Bool()))
    val i_addr_x = IO(Input(UInt(log2Ceil(params.dataSize).W)))

    val r_addr_x = RegNext(i_addr_x, 0.U)

    // Memory
    val mem_data = SyncReadMem(params.dataSize, in.bits.data.cloneType)

    // Registers
    val r_counter = RegInit(0.U(log2Ceil(params.dataSize).W))

    // Input side must be always ready to avoid stopping fft
    in.ready  := ~(reset.asBool)
    // FrameBuffer will send read flag
    val valid = RegNext(RegNext(read, 0.U), 0.U)
    out0.valid := valid
    out1.valid := valid
    out2.valid := valid

    // FSM
    object State extends ChiselEnum { val sIdle, sActive = Value }
    val state = RegInit(State.sIdle)

    val w_cut   = mem_data(r_addr_x)(log2Ceil(params.dataSize) + 16, log2Ceil(params.dataSize) + 1)
    val w_tresh = mem_data(r_addr_x)(log2Ceil(params.dataSize) + 32, log2Ceil(params.dataSize) + 16 + 1)
    val w_peak  = Cat(0.U(7.W), mem_data(r_addr_x)(0))
    // Data
    when (state === State.sIdle) {
      out0.bits.data := 0.U
      out1.bits.data := 0.U
      out2.bits.data := 0.U
      when(in.fire() && (r_counter === (params.dataSize - 1).U)) { state := State.sActive }
    } 
    .otherwise {
      out0.bits.data := w_cut 
      out1.bits.data := w_tresh
      out2.bits.data := w_peak
    }

    // count data
    when(in.fire()) {
      when((r_counter === (params.dataSize - 1).U)) {r_counter := 0.U }// || in.bits.last) {r_counter := 0.U }
      .otherwise {r_counter := r_counter + 1.U}
      // Write to memory
      mem_data(r_counter) := in.bits.data
    }

    // ILA BlackBox for Vivado
      class ILA_SPLIT extends BlackBox {
        val io = IO(new Bundle {
          val clk     = Input(Clock())
          val probe0  = Input(UInt(48.W))
          val probe1  = Input(UInt(8.W))
          val probe2  = Input(UInt(1.W))
          val probe3  = Input(UInt(1.W))
          val probe4  = Input(UInt(16.W))
          val probe5  = Input(UInt(16.W))
          val probe6  = Input(UInt(8.W))
          val probe7  = Input(UInt(8.W))
        })
      }
      val ila = Module(new ILA_SPLIT)
      ila.io.clk := clock
      ila.io.probe0 := in.bits.data
      ila.io.probe1 := r_counter
      ila.io.probe2 := in.fire()
      ila.io.probe3 := in.bits.last
      ila.io.probe4 := out0.bits.data
      ila.io.probe5 := out1.bits.data
      ila.io.probe6 := out2.bits.data
      ila.io.probe7 := r_addr_x
  }
}


trait FFTDataSplitterPins extends FFTDataSplitter {
  def beatBytes: Int = 6

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

object FFTDataSplitterApp extends App
{
  implicit val p: Parameters = Parameters.empty
  val params = FFTDataSplitterParams (
      dataSize = 1024,
  )
  
  val lazyDut = LazyModule(new FFTDataSplitter(params) with FFTDataSplitterPins)
  (new ChiselStage).execute(Array("--target-dir", "verilog/FFTDataSplitter"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}