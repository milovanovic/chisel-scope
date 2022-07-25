package hdmi.preproc

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._

import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._

// AsyncLogger parameters
case class FFTDataSplitterParams(
  dataSize : Int,   // Data logger size
)

class FFTDataSplitter(params: FFTDataSplitterParams, beatBytes: Int) extends LazyModule()(Parameters.empty) {
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

    val r_addr_x = RegNext(i_addr_x)

    // Memory
    val mem_data = SyncReadMem(params.dataSize, in.bits.data.cloneType)
    val mem_last = SyncReadMem(params.dataSize, in.bits.last.cloneType)
    // Signals

    // Registers
    val r_counter = RegInit(0.U(log2Ceil(params.dataSize).W))

    // Input side must be always ready to avoid stopping fft
    in.ready  := true.B
    // FrameBuffer will send read flag
    out0.valid := read
    out1.valid := read
    out2.valid := read
    // Data
    out0.bits.data := mem_data(r_addr_x)(26,11)
    out0.bits.last := mem_last(r_addr_x)
    out1.bits.data := mem_data(r_addr_x)(42,27)
    out1.bits.last := mem_last(r_addr_x)
    out2.bits.data := mem_data(r_addr_x)(7,0)
    out2.bits.last := mem_last(r_addr_x)

    // count data
    when(in.fire()) {
      when((r_counter === (params.dataSize - 1).U) || in.bits.last) {r_counter := 0.U }
      .otherwise {r_counter := r_counter + 1.U}
      // Write to memory
      mem_data(r_counter) := in.bits.data
      mem_last(r_counter) := in.bits.last
    }
  }
}


trait FFTDataSplitterPins extends FFTDataSplitter {
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

object FFTDataSplitterApp extends App
{
  implicit val p: Parameters = Parameters.empty
  val params = FFTDataSplitterParams (
      dataSize = 1024,
  )
  
  val lazyDut = LazyModule(new FFTDataSplitter(params, beatBytes=4) with FFTDataSplitterPins)
  (new ChiselStage).execute(Array("--target-dir", "verilog/FFTDataSplitter"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}