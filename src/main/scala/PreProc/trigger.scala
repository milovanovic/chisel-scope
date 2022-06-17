package hdmi.preproc

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.FixedPoint
import chisel3.internal.requireIsChiselType
import dsptools.numbers._
import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

// Trigger parameters
case class TriggerParams[T <: Data](
  proto    : T,     // Input/output data type
  dataSize : Int,   // Data size
  edgeType : Int    // Rising or falling edge
) {
  requireIsChiselType(proto,  s"($proto) must be chisel type")
}

// Trigger IO
class TriggerIO[T <: Data: Real] (params: TriggerParams[T], beatBytes: Int) extends Bundle {
  val in  = Flipped(Decoupled(UInt((8*beatBytes).W)))
  val out = Decoupled(UInt((8*beatBytes).W))

  val start     = Input(Bool())
  val triggered = Input(Bool())
  val edgeType  = Input(Bool())
  val treshold  = Input(params.proto)
  val dataSize  = Input(UInt((log2Ceil(params.dataSize)+1).W))

  val sync = Output(Bool())

  override def cloneType: this.type = TriggerIO(params, beatBytes).asInstanceOf[this.type]
}

object TriggerIO {
  def apply[T <: Data : Real](params: TriggerParams[T], beatBytes: Int): TriggerIO[T] = new TriggerIO(params, beatBytes)
}

class TriggerModule [T <: Data: Real: BinaryRepresentation] (val params: TriggerParams[T], beatBytes: Int) extends Module {
    val io =  IO(TriggerIO(params, beatBytes))

    // Signal definitions
    val read  = io.in.fire()
    val write = io.out.fire()

    val realPart = io.in.bits(8*beatBytes-1, 4*beatBytes).asTypeOf(params.proto)
    val imagPart = io.in.bits(4*beatBytes-1, 0*beatBytes).asTypeOf(params.proto)

    val in_delayed = RegInit(0.U.asTypeOf(params.proto))
    val counter    = RegInit(0.U((4*beatBytes).W))
    val edgeType   = io.edgeType
    val treshold   = io.treshold
    val dataSize   = io.dataSize

    // Register used for synchronisation
    val sync = RegInit(0.U(1.W))
    io.sync := sync

    io.out.bits  := io.in.bits

    // FSM
    object State extends ChiselEnum {
        val sIdle, sWait, sTriggered = Value
    }
    val state = RegInit(State.sIdle)

    io.in.ready  := true.B

    when (state === State.sIdle) {
        
        io.out.valid := false.B
        sync         := false.B
        counter      := 0.U
        when (io.start) {
            state      := State.sWait
            in_delayed := realPart
        }
    }
    .elsewhen (state === State.sWait) {
        io.out.valid := false.B
        when (read) {
            counter    := counter + 1.U
            in_delayed := realPart
            when(edgeType === 0.U) {
                when ((in_delayed < treshold) && realPart >= treshold) {
                    state   := State.sTriggered
                    counter := 0.U
                    sync    := true.B
                }
                .elsewhen(io.triggered) {
                    state   := State.sTriggered
                    counter := 0.U
                    sync    := true.B
                }
            }
            .otherwise{
                when ((in_delayed > treshold) && realPart <= treshold) {
                    state   := State.sTriggered
                    counter := 0.U
                    sync    := true.B
                }
                .elsewhen(io.triggered) {
                    state   := State.sTriggered
                    counter := 0.U
                    sync    := true.B
                }
            }
        }
    }
    .elsewhen(state === State.sTriggered) {
        io.out.valid := io.in.valid
        sync         := false.B
        when(write && (counter < dataSize - 1.U)) {
            counter  := counter + 1.U
        }
        .elsewhen (write && (counter === dataSize - 1.U)) {
            state := State.sIdle
            counter := 0.U
        }
    }
    .otherwise {
        io.out.valid := false.B
        sync         := false.B
        state        := State.sIdle
    }
}


abstract class Trigger [T <: Data : Real: BinaryRepresentation, D, U, E, O, B <: Data] (params: TriggerParams[T], beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

    val streamNode = AXI4StreamIdentityNode()

    lazy val module = new LazyModuleImp(this) {
        val (in, _)  = streamNode.in(0)
        val (out, _) = streamNode.out(0)

        val start     = IO(Input(Bool()))
        val triggered = IO(Input(Bool()))
        val sync      = IO(Output(Bool()))
        val counter   = IO(Output(UInt((4*beatBytes).W)))

        val treshold = RegInit(0.U((4*beatBytes).W))
        val edgeType = RegInit(0.U(1.W))
        val dataSize = RegInit(params.dataSize.U((log2Ceil(params.dataSize)+1).W))

        // Define register fields
        val fields = Seq(RegField(4*beatBytes, treshold, RegFieldDesc(name = "scale", desc = "Register used to set the triger treshold")),
                         RegField(log2Ceil(params.dataSize)+1, dataSize, RegFieldDesc(name = "scale", desc = "Register used to set data size")),
                         RegField(1, edgeType, RegFieldDesc(name = "scale", desc = "Register used to set trigger edge")))
                         
        // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)

        val triggerMod = Module(new TriggerModule(params, beatBytes))
        triggerMod.io.in.bits  := in.bits.data
        triggerMod.io.in.valid := in.valid
        in.ready := triggerMod.io.in.ready
        out.bits.data := triggerMod.io.out.bits
        out.valid     := triggerMod.io.out.valid
        triggerMod.io.out.ready := out.ready

        triggerMod.io.start     := start
        triggerMod.io.triggered := triggered
        triggerMod.io.edgeType  := edgeType
        triggerMod.io.treshold  := treshold.asTypeOf(params.proto)
        triggerMod.io.dataSize  := dataSize
        sync    := triggerMod.io.sync
    }
}

class AXI4TriggerBlock[T <: Data : Real: BinaryRepresentation](params: TriggerParams[T], address: AddressSet, beatBytes: Int = 4)(implicit p: Parameters) extends Trigger[T, AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
}


trait AXI4TriggerStandaloneBlock extends AXI4DspBlock {
  def standaloneParams = AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))

    m :=
      BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) :=
      ioMemNode

    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}
  
  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = 4)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode :=
    AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) :=
    streamNode :=
    BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 4)) :=
    ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}


object TriggerApp extends App
{
  val params: TriggerParams[FixedPoint] = TriggerParams(
    proto = FixedPoint(16.W, 14.BP),
    dataSize = 512,
    edgeType = 0,
  )
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AXI4TriggerBlock(params, AddressSet(0x00, 0xF), beatBytes = 4) with AXI4TriggerStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/Trigger"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}