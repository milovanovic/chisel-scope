package hdmi.scope 

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

// AsyncScopeQueue parameters
case class AsyncScopeQueueParams(
    fft_1D: Boolean,
    fft_2D: Boolean,
    s_width_1D : Int,
    s_width_2D : Int,
    dataSize   : Int, // Data logger size, equal to x axis size
)

class AsyncScopeQueueIO(val fft_1D: Boolean, val fft_2D: Boolean, val s_width_1D: Int, val s_width_2D: Int) extends Bundle {
    val clock2 = Input(Clock())
    val reset2 = Input(Bool())

    // 1D processing
    val i_scaler_y_1D = if (fft_1D) Some(Input(UInt((s_width_1D+1).W)))  else None
    val i_scaler_x_1D = if (fft_1D) Some(Input(UInt((s_width_1D).W)))    else None
    val o_scaler_y_1D = if (fft_1D) Some(Output(UInt((s_width_1D+1).W))) else None
    val o_scaler_x_1D = if (fft_1D) Some(Output(UInt((s_width_1D).W)))   else None

     // 2D processing
    val i_scaler_x_2D = if (fft_2D) Some(Input(UInt((s_width_2D).W)))    else None
    val o_scaler_x_2D = if (fft_2D) Some(Output(UInt((s_width_2D).W)))   else None
    val i_scaler_y_2D = if (fft_2D) Some(Input(UInt((s_width_2D+1).W)))    else None
    val o_scaler_y_2D = if (fft_2D) Some(Output(UInt((s_width_2D+1).W)))   else None
}
object AsyncScopeQueueIO {
  def apply(fft_1D: Boolean, fft_2D: Boolean, s_width_1D: Int, s_width_2D: Int): AsyncScopeQueueIO = new AsyncScopeQueueIO(fft_1D, fft_2D, s_width_1D, s_width_2D)
}

class AsyncScopeQueue(params: AsyncScopeQueueParams) extends LazyModule()(Parameters.empty) {
    val node_range   = if (params.fft_1D) Some(AXI4StreamIdentityNode()) else None
    val node_doppler = if (params.fft_2D) Some(AXI4StreamIdentityNode()) else None

    lazy val module = new LazyModuleImp(this) {
        // IOs
        val io = IO(new AsyncScopeQueueIO(params.fft_1D, params.fft_2D, params.s_width_1D, params.s_width_2D))

        val i_range   = if (params.fft_1D) Some(node_range.get.in(0)._1)      else None
        val i_doppler = if (params.fft_2D) Some(node_doppler.get.in(0)._1)  else None
        val o_range   = if (params.fft_1D) Some(node_range.get.out(0)._1)     else None
        val o_doppler = if (params.fft_2D) Some(node_doppler.get.out(0)._1) else None

        // 1D async
        val async_scaler_y_1D = if (params.fft_1D) Some(Module(new AsyncQueue(chiselTypeOf(io.i_scaler_y_1D.get), AsyncQueueParams(depth = 8, sync = 2, safe = true)))) else None
        val async_scaler_x_1D = if (params.fft_1D) Some(Module(new AsyncQueue(chiselTypeOf(io.i_scaler_x_1D.get), AsyncQueueParams(depth = 8, sync = 2, safe = true)))) else None
        val async_range       = if (params.fft_1D) Some(Module(new AsyncQueue(chiselTypeOf(i_range.get.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true))))   else None

        // 2D async
        val async_scaler_y_2D = if (params.fft_2D) Some(Module(new AsyncQueue(chiselTypeOf(io.i_scaler_y_2D.get), AsyncQueueParams(depth = 8, sync = 2, safe = true)))) else None
        val async_scaler_x_2D = if (params.fft_2D) Some(Module(new AsyncQueue(chiselTypeOf(io.i_scaler_x_2D.get), AsyncQueueParams(depth = 8, sync = 2, safe = true)))) else None
        val async_doppler     = if (params.fft_2D) Some(Module(new AsyncQueue(chiselTypeOf(i_doppler.get.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true)))) else None

        // Connect inputs
        if (params.fft_1D) {
            // Connect async_range
            async_range.get.io.enq <> i_range.get
            async_range.get.io.enq_clock := clock
            async_range.get.io.enq_reset := reset.asBool()

            o_range.get <> async_range.get.io.deq
            async_range.get.io.deq_clock := io.clock2
            async_range.get.io.deq_reset := io.reset2.asBool()

            // Connect async_scaler_y_1D
            async_scaler_y_1D.get.io.enq.valid := true.B
            async_scaler_y_1D.get.io.enq.bits  := io.i_scaler_y_1D.get
            async_scaler_y_1D.get.io.enq_clock := clock
            async_scaler_y_1D.get.io.enq_reset := reset.asBool()
            io.o_scaler_y_1D.get := async_scaler_y_1D.get.io.deq.bits
            async_scaler_y_1D.get.io.deq.ready := true.B
            async_scaler_y_1D.get.io.deq_clock := io.clock2
            async_scaler_y_1D.get.io.deq_reset := io.reset2.asBool()

            // Connect async_scaler_x_1D
            async_scaler_x_1D.get.io.enq.valid := true.B
            async_scaler_x_1D.get.io.enq.bits  := io.i_scaler_x_1D.get
            async_scaler_x_1D.get.io.enq_clock := clock
            async_scaler_x_1D.get.io.enq_reset := reset.asBool()
            io.o_scaler_x_1D.get := async_scaler_x_1D.get.io.deq.bits
            async_scaler_x_1D.get.io.deq.ready := true.B
            async_scaler_x_1D.get.io.deq_clock := io.clock2
            async_scaler_x_1D.get.io.deq_reset := io.reset2.asBool()
        }
        if (params.fft_2D) {
            // Connect async_scaler_x_2D
            async_scaler_x_2D.get.io.enq.valid := true.B
            async_scaler_x_2D.get.io.enq.bits  := io.i_scaler_x_2D.get
            async_scaler_x_2D.get.io.enq_clock := clock
            async_scaler_x_2D.get.io.enq_reset := reset.asBool()
            io.o_scaler_x_2D.get := async_scaler_x_2D.get.io.deq.bits
            async_scaler_x_2D.get.io.deq.ready := true.B
            async_scaler_x_2D.get.io.deq_clock := io.clock2
            async_scaler_x_2D.get.io.deq_reset := io.reset2.asBool()

            // Connect async_scaler_y_2D
            async_scaler_y_2D.get.io.enq.valid := true.B
            async_scaler_y_2D.get.io.enq.bits  := io.i_scaler_y_2D.get
            async_scaler_y_2D.get.io.enq_clock := clock
            async_scaler_y_2D.get.io.enq_reset := reset.asBool()
            io.o_scaler_y_2D.get := async_scaler_y_2D.get.io.deq.bits
            async_scaler_y_2D.get.io.deq.ready := true.B
            async_scaler_y_2D.get.io.deq_clock := io.clock2
            async_scaler_y_2D.get.io.deq_reset := io.reset2.asBool()

            async_doppler.get.io.enq <> i_doppler.get
            async_doppler.get.io.enq_clock := clock
            async_doppler.get.io.enq_reset := reset.asBool()

            // Connect output reset and clock
            o_doppler.get <> async_doppler.get.io.deq
            async_doppler.get.io.deq_clock := io.clock2
            async_doppler.get.io.deq_reset := io.reset2.asBool()
        }
    }
}

trait AsyncScopeQueueStandaloneBlock extends AsyncScopeQueue {
    // 1D processing
    val in_node_range  = if (node_range != None) Some(BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))) else None
    val out_node_range = if (node_range != None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None

    if (node_range != None) { out_node_range.get := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := node_range.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := in_node_range.get   }

    val i_range = if (node_range != None) Some(InModuleBody { in_node_range.get.makeIO()  }) else None
    val o_range = if (node_range != None) Some(InModuleBody { out_node_range.get.makeIO() }) else None

    // 2D processing
    val in_node_doppler  = if (node_doppler != None) Some(BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))) else None
    val out_node_doppler = if (node_doppler != None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None

    if (node_doppler != None) { out_node_doppler.get   := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := node_doppler.get   := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := in_node_doppler.get   }

    val i_doppler = if (node_doppler != None) Some(InModuleBody { in_node_doppler.get.makeIO()  }) else None
    val o_doppler = if (node_doppler != None) Some(InModuleBody { out_node_doppler.get.makeIO() }) else None
}

object AsyncScopeQueueApp extends App
{
  val params = AsyncScopeQueueParams(
    fft_1D = true,
    fft_2D = true,
    s_width_1D = 4,
    s_width_2D = 4,
    dataSize = 1024
  )
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AsyncScopeQueue(params) with AsyncScopeQueueStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/AsyncScopeQueue"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}