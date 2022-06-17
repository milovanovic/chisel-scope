package hdmi.preproc 

import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

class DataSplitter(beatBytes: Int) extends LazyModule()(Parameters.empty) {

  val inNode  = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  val outNode0 = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters( "real", n = beatBytes/2)))))
  val outNode1 = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters( "imag", n = beatBytes/2)))))

  lazy val module = new LazyModuleImp(this) {
    val (in, inP)     = inNode.in(0)
    val (out0, outP0) = outNode0.out(0)
    val (out1, outP1) = outNode1.out(0)

    in.ready       := out0.ready && out1.ready
    out0.valid     := in.valid
    out1.valid     := in.valid
    out0.bits.data := in.bits.data(8*beatBytes-1, 4*beatBytes)
    out1.bits.data := in.bits.data(4*beatBytes-1, 0)
    out0.bits.last := in.bits.last
    out1.bits.last := in.bits.last
  }
}

class DataSplit(beatBytes: Int) extends LazyModule()(Parameters.empty) {

  val inNode  = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
  val outNode0 = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters( "data0", n = beatBytes)))))
  val outNode1 = AXI4StreamMasterNode(Seq(AXI4StreamMasterPortParameters(Seq(AXI4StreamMasterParameters( "data1", n = beatBytes)))))

  lazy val module = new LazyModuleImp(this) {
    val (in, inP)     = inNode.in(0)
    val (out0, outP0) = outNode0.out(0)
    val (out1, outP1) = outNode1.out(0)

    in.ready       := out0.ready && out1.ready
    out0.valid     := in.valid
    out1.valid     := in.valid
    out0.bits.data := in.bits.data
    out1.bits.data := in.bits.data
    out0.bits.last := in.bits.last
    out1.bits.last := in.bits.last
  }
}

trait DataSplitterStandaloneBlock extends DataSplitter {

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 4)))
  val ioOutNode0 = BundleBridgeSink[AXI4StreamBundle]()
  val ioOutNode1 = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode0 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode0
  ioOutNode1 := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := outNode1
  inNode     := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 4)) := ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out0 = InModuleBody { ioOutNode0.makeIO() }
  val out1 = InModuleBody { ioOutNode1.makeIO() }
}

object DataSplitterApp extends App
{
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new DataSplitter(beatBytes = 4) with DataSplitterStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/DataSplitter"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}
