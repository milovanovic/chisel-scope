package hdmi.scope 

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

// TODO : Set reset2 to asynchronous reset

// AsyncLogger parameters
case class AsyncLoggerParams(
    fft_1D: Boolean,
    fft_2D: Boolean,
    s_width_1D : Int,
    s_width_2D : Int,
    dataSize   : Int, // Data logger size, equal to x axis size
)

class AsyncLoggerIO(val fft_1D: Boolean, val fft_2D: Boolean, val s_width_1D: Int, val s_width_2D: Int) extends Bundle {
    val clock2 = Input(Clock())
    val reset2 = Input(Bool())

    // 1D processing
    val i_addr_x_1D   = if (fft_1D) Some(Input(UInt(16.W))) else None
    val i_scaler_y_1D = if (fft_1D) Some(Input(UInt((s_width_1D+1).W)))  else None
    val i_scaler_x_1D = if (fft_1D) Some(Input(UInt((s_width_1D).W)))    else None
    val o_scaler_y_1D = if (fft_1D) Some(Output(UInt((s_width_1D+1).W))) else None
    val o_scaler_x_1D = if (fft_1D) Some(Output(UInt((s_width_1D).W)))   else None

     // 2D processing
    val i_scaler_x_2D = if (fft_2D) Some(Input(UInt((s_width_2D).W)))    else None
    val o_scaler_x_2D = if (fft_2D) Some(Output(UInt((s_width_2D).W)))   else None
}
object AsyncLoggerIO {
  def apply(fft_1D: Boolean, fft_2D: Boolean, s_width_1D: Int, s_width_2D: Int): AsyncLoggerIO = new AsyncLoggerIO(fft_1D, fft_2D, s_width_1D, s_width_2D)
}

class AsyncLogger(params: AsyncLoggerParams) extends LazyModule()(Parameters.empty) {
    val node_cut     = if (params.fft_1D) Some(AXI4StreamIdentityNode()) else None
    val node_tresh   = if (params.fft_1D) Some(AXI4StreamIdentityNode()) else None
    val node_peak    = if (params.fft_1D) Some(AXI4StreamIdentityNode()) else None
    val node_doppler = if (params.fft_2D) Some(AXI4StreamIdentityNode()) else None

    lazy val module = new LazyModuleImp(this) {
        // IOs
        val io = IO(new AsyncLoggerIO(params.fft_1D, params.fft_2D, params.s_width_1D, params.s_width_2D))

        val i_cut     = if (params.fft_1D) Some(node_cut.get.in(0)._1)      else None
        val i_tresh   = if (params.fft_1D) Some(node_tresh.get.in(0)._1)    else None
        val i_peak    = if (params.fft_1D) Some(node_peak.get.in(0)._1)     else None
        val i_doppler = if (params.fft_2D) Some(node_doppler.get.in(0)._1)  else None
        val o_cut     = if (params.fft_1D) Some(node_cut.get.out(0)._1)     else None
        val o_tresh   = if (params.fft_1D) Some(node_tresh.get.out(0)._1)   else None
        val o_peak    = if (params.fft_1D) Some(node_peak.get.out(0)._1)    else None
        val o_doppler = if (params.fft_2D) Some(node_doppler.get.out(0)._1) else None

        // 1D async
        val async_scaler_y_1D = if (params.fft_1D) Some(Module(new AsyncQueue(chiselTypeOf(io.i_scaler_y_1D.get), AsyncQueueParams(depth = 8, sync = 2, safe = true)))) else None
        val async_scaler_x_1D = if (params.fft_1D) Some(Module(new AsyncQueue(chiselTypeOf(io.i_scaler_x_1D.get), AsyncQueueParams(depth = 8, sync = 2, safe = true)))) else None

        val async_cut     = if (params.fft_1D) Some(Module(new AsyncQueue(chiselTypeOf(i_cut.get.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true))))   else None
        val async_tresh   = if (params.fft_1D) Some(Module(new AsyncQueue(chiselTypeOf(i_tresh.get.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true)))) else None
        val async_peak    = if (params.fft_1D) Some(Module(new AsyncQueue(chiselTypeOf(i_peak.get.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true))))  else None

        // 2D async
        val async_scaler_x_2D = if (params.fft_2D) Some(Module(new AsyncQueue(chiselTypeOf(io.i_scaler_x_2D.get), AsyncQueueParams(depth = 8, sync = 2, safe = true)))) else None

        val async_doppler = if (params.fft_2D) Some(Module(new AsyncQueue(chiselTypeOf(i_doppler.get.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true)))) else None

        // Connect inputs
        if (params.fft_1D) {
            async_cut.get.io.enq   <> i_cut.get
            async_tresh.get.io.enq <> i_tresh.get
            async_peak.get.io.enq  <> i_peak.get
            async_cut.get.io.enq_clock   := clock
            async_tresh.get.io.enq_clock := clock
            async_peak.get.io.enq_clock  := clock
            async_cut.get.io.enq_reset   := reset.asBool()
            async_tresh.get.io.enq_reset := reset.asBool()
            async_peak.get.io.enq_reset  := reset.asBool()

            // Connect output reset and clock
            async_cut.get.io.deq_clock   := io.clock2
            async_tresh.get.io.deq_clock := io.clock2
            async_peak.get.io.deq_clock  := io.clock2
            async_cut.get.io.deq_reset   := io.reset2.asBool()
            async_tresh.get.io.deq_reset := io.reset2.asBool()
            async_peak.get.io.deq_reset  := io.reset2.asBool()

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

            // Memories and counters
            withClockAndReset(io.clock2, io.reset2) {
                val mem_cut   = SyncReadMem(params.dataSize, chiselTypeOf(i_cut.get.bits))
                val mem_tresh = SyncReadMem(params.dataSize, chiselTypeOf(i_tresh.get.bits))
                val mem_peak  = SyncReadMem(params.dataSize, chiselTypeOf(i_peak.get.bits))

                val cnt_1D = RegInit(0.U(log2Ceil(params.dataSize).W))
                cnt_1D.suggestName("cnt_1D")

                async_cut.get.io.deq.ready   := true.B
                async_tresh.get.io.deq.ready := true.B
                async_peak.get.io.deq.ready  := true.B

                // FFT signal
                when(async_cut.get.io.deq.fire()) {
                    mem_cut(cnt_1D)   := async_cut.get.io.deq.bits
                    mem_tresh(cnt_1D) := async_tresh.get.io.deq.bits
                    mem_peak(cnt_1D)  := async_peak.get.io.deq.bits
                    when ((cnt_1D === (params.dataSize-1).U) || async_cut.get.io.deq.bits.last){
                        cnt_1D := 0.U
                    }
                    .otherwise {
                        cnt_1D := cnt_1D + 1.U
                    }
                }

                // Output data (read and write to the same address should never occur, hopefully)
                o_cut.get.bits    := mem_cut(io.i_addr_x_1D.get)
                o_tresh.get.bits  := mem_tresh(io.i_addr_x_1D.get)
                o_peak.get.bits   := mem_peak(io.i_addr_x_1D.get)
                o_cut.get.valid   := true.B
                o_tresh.get.valid := true.B
                o_peak.get.valid  := true.B
            }
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

            async_doppler.get.io.enq  <> i_doppler.get
            async_doppler.get.io.enq_clock := clock
            async_doppler.get.io.enq_reset := reset.asBool()

            // Connect output reset and clock
            o_doppler.get   <> async_doppler.get.io.deq
            async_doppler.get.io.deq_clock := io.clock2
            async_doppler.get.io.deq_reset := io.reset2.asBool()
        }
    }
}

trait AsyncLoggerStandaloneBlock extends AsyncLogger {
    // 1D processing
    val in_node_cut   = if (node_cut   != None) Some(BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))) else None
    val in_node_tresh = if (node_tresh != None) Some(BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))) else None
    val in_node_peak  = if (node_peak  != None) Some(BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 1)))) else None
    val out_node_cut   = if (node_cut   != None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None
    val out_node_tresh = if (node_tresh != None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None
    val out_node_peak  = if (node_peak  != None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None

    if (node_cut   != None) { out_node_cut.get   := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := node_cut.get   := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := in_node_cut.get   }
    if (node_tresh != None) { out_node_tresh.get := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := node_tresh.get := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := in_node_tresh.get }
    if (node_peak  != None) { out_node_peak.get  := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := node_peak.get  := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 1)) := in_node_peak.get  }

    val i_cut   = if (node_cut   != None) Some(InModuleBody { in_node_cut.get.makeIO()    }) else None
    val i_tresh = if (node_tresh != None) Some(InModuleBody { in_node_tresh.get.makeIO()  }) else None
    val i_peak  = if (node_peak  != None) Some(InModuleBody { in_node_peak.get.makeIO()   }) else None
    val o_cut   = if (node_cut   != None) Some(InModuleBody { out_node_cut.get.makeIO()   }) else None
    val o_tresh = if (node_tresh != None) Some(InModuleBody { out_node_tresh.get.makeIO() }) else None
    val o_peak  = if (node_peak  != None) Some(InModuleBody { out_node_peak.get.makeIO()  }) else None

    // 2D processing
    val in_node_doppler  = if (node_doppler != None) Some(BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))) else None
    val out_node_doppler = if (node_doppler != None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None

    if (node_doppler != None) { out_node_doppler.get   := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := node_doppler.get   := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := in_node_doppler.get   }

    val i_doppler   = if (node_doppler != None) Some(InModuleBody { in_node_doppler.get.makeIO()  }) else None
    val o_doppler   = if (node_doppler != None) Some(InModuleBody { out_node_doppler.get.makeIO() }) else None
}

trait AsyncLoggerOutputPins extends AsyncLogger {
    // 1D processing
    val out_node_cut   = if (node_cut   != None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None
    val out_node_tresh = if (node_tresh != None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None
    val out_node_peak  = if (node_peak  != None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None

    if (node_cut   != None) { out_node_cut.get   := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := node_cut.get   }
    if (node_tresh != None) { out_node_tresh.get := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := node_tresh.get }
    if (node_peak  != None) { out_node_peak.get  := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := node_peak.get  }

    val o_cut   = if (node_cut   != None) Some(InModuleBody { out_node_cut.get.makeIO()   }) else None
    val o_tresh = if (node_tresh != None) Some(InModuleBody { out_node_tresh.get.makeIO() }) else None
    val o_peak  = if (node_peak  != None) Some(InModuleBody { out_node_peak.get.makeIO()  }) else None

    // // 2D processing
    // val out_node_doppler = if (node_doppler != None) Some(BundleBridgeSink[AXI4StreamBundle]()) else None
    // if (node_doppler != None) { out_node_doppler.get   := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := node_doppler.get }
    // val o_doppler   = if (node_doppler != None) Some(InModuleBody { out_node_doppler.get.makeIO() }) else None
}


object AsyncLoggerApp extends App
{
  val params = AsyncLoggerParams(
    fft_1D = true,
    fft_2D = true,
    s_width_1D = 4,
    s_width_2D = 4,
    dataSize = 1024
  )
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AsyncLogger(params) with AsyncLoggerStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/AsyncLogger"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}