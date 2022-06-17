package hdmi.scope 

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.FixedPoint
import chisel3.internal.requireIsChiselType
import dsptools.numbers._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

// TODO : Set reset2 to asynchronous reset

// AsyncLogger parameters
case class AsyncLoggerParams[T <: Data: Real: BinaryRepresentation](
  proto0   : T,     // Input/output data type
  proto1   : T,     // Input/output data type
  proto2   : T,     // Input/output data type
  dataSize : Int,   // Data logger size, equal to x axis size
) {
  requireIsChiselType(proto0,  s"($proto0) must be chisel type")
}

// AsyncLogger IO
class AsyncLoggerIO[T <: Data: Real](params: AsyncLoggerParams[T]) extends Bundle {
    val out0 = Output(params.proto0)
    val out1 = Output(params.proto0)
    val out2 = Output(params.proto1)
    val out3 = Output(params.proto1)
    val out4 = Output(params.proto2)

    override def cloneType: this.type = AsyncLoggerIO(params).asInstanceOf[this.type]
}

object AsyncLoggerIO {
  def apply[T <: Data : Real](params: AsyncLoggerParams[T]): AsyncLoggerIO[T] = new AsyncLoggerIO(params)
}

class AsyncLogger[T <: Data: Real](params: AsyncLoggerParams[T], scalerWidth: Int, xilinxAFIFO: Boolean = false) extends LazyModule()(Parameters.empty) {
    val inRealNode      = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
    val inImagNode      = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
    val inCutNode       = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
    val inTresholdNode  = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())
    val inPeakNode      = AXI4StreamSlaveNode(AXI4StreamSlaveParameters())

    lazy val module = new LazyModuleImp(this) {
        val (in0, _ ) = inRealNode.in(0)
        val (in1, _ ) = inImagNode.in(0)
        val (in2, _ ) = inCutNode.in(0)
        val (in3, _ ) = inTresholdNode.in(0)
        val (in4, _ ) = inPeakNode.in(0)

        // Inputs
        val start       = IO(Input(Bool()))
        val startFFT    = IO(Input(Bool()))
        val clock2      = IO(Input(Clock())) 
        val reset2      = IO(Input(Bool()))
        val pixel_x     = IO(Input(UInt(16.W)))
        val pixel_x_fft = IO(Input(UInt(16.W)))

        val scaler_y         = IO(Input(UInt((scalerWidth+1).W)))
        val scaler_x         = IO(Input(UInt((scalerWidth).W)))
        val fft_scaler_y     = IO(Input(UInt((scalerWidth+1).W)))
        val fft_scaler_x     = IO(Input(UInt((scalerWidth).W)))

        val scaler_y_out     = IO(Output(UInt((scalerWidth+1).W)))
        val scaler_x_out     = IO(Output(UInt((scalerWidth).W)))
        val fft_scaler_y_out = IO(Output(UInt((scalerWidth+1).W)))
        val fft_scaler_x_out = IO(Output(UInt((scalerWidth).W)))

        val io = IO(new AsyncLoggerIO(params))

        // Async queue
        val asyncQueue0 = Module(new AsyncQueue(chiselTypeOf(in0.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true)))
        val asyncQueue1 = Module(new AsyncQueue(chiselTypeOf(in1.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true)))
        val asyncQueue2 = Module(new AsyncQueue(chiselTypeOf(in2.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true)))
        val asyncQueue3 = Module(new AsyncQueue(chiselTypeOf(in3.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true)))
        val asyncQueue4 = Module(new AsyncQueue(chiselTypeOf(in4.bits), AsyncQueueParams(depth = 8, sync = 2, safe = true)))

        val asyncStart    = Module(new AsyncQueue(chiselTypeOf(start), AsyncQueueParams(depth = 8, sync = 2, safe = true)))
        val asyncStartFFT = Module(new AsyncQueue(chiselTypeOf(startFFT), AsyncQueueParams(depth = 8, sync = 2, safe = true)))
        val syncStart     = Wire(Bool())
        val syncStartFFT  = Wire(Bool())

        val asyncScaler_y     = Module(new AsyncQueue(chiselTypeOf(scaler_y),     AsyncQueueParams(depth = 8, sync = 2, safe = true)))
        val asyncScaler_x     = Module(new AsyncQueue(chiselTypeOf(scaler_x),     AsyncQueueParams(depth = 8, sync = 2, safe = true)))
        val asyncFFT_scaler_y = Module(new AsyncQueue(chiselTypeOf(fft_scaler_y), AsyncQueueParams(depth = 8, sync = 2, safe = true)))
        val asyncFFT_scaler_x = Module(new AsyncQueue(chiselTypeOf(fft_scaler_x), AsyncQueueParams(depth = 8, sync = 2, safe = true)))

        // Connect inputs
        asyncQueue0.io.enq <> in0
        asyncQueue1.io.enq <> in1
        asyncQueue2.io.enq <> in2
        asyncQueue3.io.enq <> in3
        asyncQueue4.io.enq <> in4
        asyncQueue0.io.enq_clock := clock
        asyncQueue1.io.enq_clock := clock
        asyncQueue2.io.enq_clock := clock
        asyncQueue3.io.enq_clock := clock
        asyncQueue4.io.enq_clock := clock
        asyncQueue0.io.enq_reset := reset.asBool()
        asyncQueue1.io.enq_reset := reset.asBool()
        asyncQueue2.io.enq_reset := reset.asBool()
        asyncQueue3.io.enq_reset := reset.asBool()
        asyncQueue4.io.enq_reset := reset.asBool()

        // Connect output reset and clock
        asyncQueue0.io.deq_clock := clock2
        asyncQueue1.io.deq_clock := clock2
        asyncQueue2.io.deq_clock := clock2
        asyncQueue3.io.deq_clock := clock2
        asyncQueue4.io.deq_clock := clock2
        asyncQueue0.io.deq_reset := reset2.asBool()
        asyncQueue1.io.deq_reset := reset2.asBool()
        asyncQueue2.io.deq_reset := reset2.asBool()
        asyncQueue3.io.deq_reset := reset2.asBool()
        asyncQueue4.io.deq_reset := reset2.asBool()

        // Connect asyncStart
        asyncStart.io.enq.valid := true.B
        asyncStart.io.enq.bits  := start
        asyncStart.io.enq_clock := clock
        asyncStart.io.enq_reset := reset.asBool()
        syncStart := asyncStart.io.deq.bits
        asyncStart.io.deq.ready := true.B
        asyncStart.io.deq_clock := clock2
        asyncStart.io.deq_reset := reset2.asBool()

        // Connect asyncStartFFT
        asyncStartFFT.io.enq.valid := true.B
        asyncStartFFT.io.enq.bits  := startFFT
        asyncStartFFT.io.enq_clock := clock
        asyncStartFFT.io.enq_reset := reset.asBool()
        syncStartFFT := asyncStartFFT.io.deq.bits
        asyncStartFFT.io.deq.ready := true.B
        asyncStartFFT.io.deq_clock := clock2
        asyncStartFFT.io.deq_reset := reset2.asBool()

        // Connect asyncScaler_y
        asyncScaler_y.io.enq.valid := true.B
        asyncScaler_y.io.enq.bits  := scaler_y
        asyncScaler_y.io.enq_clock := clock
        asyncScaler_y.io.enq_reset := reset.asBool()
        scaler_y_out := asyncScaler_y.io.deq.bits
        asyncScaler_y.io.deq.ready := true.B
        asyncScaler_y.io.deq_clock := clock2
        asyncScaler_y.io.deq_reset := reset2.asBool()

        // Connect asyncScaler_x
        asyncScaler_x.io.enq.valid := true.B
        asyncScaler_x.io.enq.bits  := scaler_x
        asyncScaler_x.io.enq_clock := clock
        asyncScaler_x.io.enq_reset := reset.asBool()
        scaler_x_out := asyncScaler_x.io.deq.bits
        asyncScaler_x.io.deq.ready := true.B
        asyncScaler_x.io.deq_clock := clock2
        asyncScaler_x.io.deq_reset := reset2.asBool()

        // Connect asyncFFT_scaler_y
        asyncFFT_scaler_y.io.enq.valid := true.B
        asyncFFT_scaler_y.io.enq.bits  := fft_scaler_y
        asyncFFT_scaler_y.io.enq_clock := clock
        asyncFFT_scaler_y.io.enq_reset := reset.asBool()
        fft_scaler_y_out := asyncFFT_scaler_y.io.deq.bits
        asyncFFT_scaler_y.io.deq.ready := true.B
        asyncFFT_scaler_y.io.deq_clock := clock2
        asyncFFT_scaler_y.io.deq_reset := reset2.asBool()

        // Connect asyncFFT_scaler_x
        asyncFFT_scaler_x.io.enq.valid := true.B
        asyncFFT_scaler_x.io.enq.bits  := fft_scaler_x
        asyncFFT_scaler_x.io.enq_clock := clock
        asyncFFT_scaler_x.io.enq_reset := reset.asBool()
        fft_scaler_x_out := asyncFFT_scaler_x.io.deq.bits
        asyncFFT_scaler_x.io.deq.ready := true.B
        asyncFFT_scaler_x.io.deq_clock := clock2
        asyncFFT_scaler_x.io.deq_reset := reset2.asBool()

        // Memories and counters
        withClockAndReset(clock2, reset2) {
            val mem0 = SyncReadMem(params.dataSize, params.proto0)
            val mem1 = SyncReadMem(params.dataSize, params.proto0)
            val mem2 = SyncReadMem(params.dataSize, params.proto1)
            val mem3 = SyncReadMem(params.dataSize, params.proto1)
            val mem4 = SyncReadMem(params.dataSize, params.proto2)

            val counter0 = RegInit(0.U(log2Ceil(params.dataSize).W))
            val counter2 = RegInit(0.U(log2Ceil(params.dataSize).W))
            counter0.suggestName("counter0")
            counter2.suggestName("counter2")

            // FSM
            object State extends ChiselEnum {
                val sIdle, sWrite = Value
            }
            val state0 = RegInit(State.sIdle)
            val state2 = RegInit(State.sIdle)
            state0.suggestName("state0")
            state2.suggestName("state2")

            asyncQueue0.io.deq.ready := true.B
            asyncQueue1.io.deq.ready := true.B
            asyncQueue2.io.deq.ready := true.B
            asyncQueue3.io.deq.ready := true.B
            asyncQueue4.io.deq.ready := true.B

            val syncStart_delay = RegNext(syncStart, 0.U)

            when(syncStart && (syncStart_delay === 0.U)) {
                counter0 := 0.U
            }
            .elsewhen(asyncQueue0.io.deq.fire()){
                mem0(counter0) := asyncQueue0.io.deq.bits.data.asTypeOf(params.proto0)
                mem1(counter0) := asyncQueue1.io.deq.bits.data.asTypeOf(params.proto0)
                counter0 := counter0 + 1.U
            }
            when (counter0 === (params.dataSize-1).U){
                counter0 := 0.U
            }

            // FFT signal
            when(syncStartFFT) {
                counter2 := 0.U
            }
            .elsewhen(asyncQueue2.io.deq.fire()){
                mem2(counter2) := asyncQueue2.io.deq.bits.data.asTypeOf(params.proto1)
                mem3(counter2) := asyncQueue3.io.deq.bits.data.asTypeOf(params.proto1)
                mem4(counter2) := asyncQueue4.io.deq.bits.data.asTypeOf(params.proto2)
                counter2 := counter2 + 1.U
            }
            when (counter2 === (params.dataSize-1).U){
                counter2 := 0.U
            }

            // Output data (read and write to the same address should never occur)
            io.out0 := mem0(pixel_x)
            io.out1 := mem1(pixel_x)
            io.out2 := mem2(pixel_x_fft)
            io.out3 := mem3(pixel_x_fft)
            io.out4 := mem4(pixel_x_fft)
        }
    }
}

trait AsyncLoggerStandaloneBlock extends AsyncLogger[FixedPoint] {
    val ioInNode0 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
    val ioInNode1 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 2)))
    val ioInNode2 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 3)))
    val ioInNode3 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 3)))
    val ioInNode4 = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 1)))

    inRealNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := ioInNode0
    inImagNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 2)) := ioInNode1
    inCutNode  := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 3)) := ioInNode2
    inTresholdNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 3)) := ioInNode3
    inPeakNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 1)) := ioInNode4

    val in0 = InModuleBody { ioInNode0.makeIO() }
    val in1 = InModuleBody { ioInNode1.makeIO() }
    val in2 = InModuleBody { ioInNode2.makeIO() }
    val in3 = InModuleBody { ioInNode3.makeIO() }
    val in4 = InModuleBody { ioInNode4.makeIO() }
}


object AsyncLoggerApp extends App
{
  val params: AsyncLoggerParams[FixedPoint] = AsyncLoggerParams(
    proto0    = FixedPoint(16.W, 10.BP),
    proto1    = FixedPoint(16.W, 10.BP),
    proto2    = FixedPoint( 8.W,  0.BP),
    dataSize = 1024
  )
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AsyncLogger(params, 4) with AsyncLoggerStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/AsyncLogger"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}