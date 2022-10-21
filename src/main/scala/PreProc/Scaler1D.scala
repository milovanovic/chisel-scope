package hdmi.preproc  

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._

import dsptools.numbers._

import dspblocks._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.axi4stream._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

import scala.math._

// Scaler1D parameters
case class Scaler1DParams(
  dataSize: Int,  // Range FFT size
  scale   : Int,  // Scaler1D
)

abstract class Scaler1D [D, U, E, O, B <: Data] (params: Scaler1DParams, beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

    val streamNode = AXI4StreamIdentityNode()
    
    lazy val module = new LazyModuleImp(this) {
        val (in, _)  = streamNode.in(0)
        val (out, _) = streamNode.out(0)

        val width = params.scale
        val scaleYReg = RegInit((1 << width).U((width+1).W))
        val scaleXReg = RegInit(0.U((width).W))

        // Outputs
        val scalerY = IO(Output(UInt((width+1).W)))
        val scalerX = IO(Output(UInt((width).W)))
        scalerY := scaleYReg
        scalerX := scaleXReg

        val fields = Seq(RegField(width+1, scaleYReg, RegFieldDesc(name = "scaleYReg", desc = "Register used to scale y axis of the signal")),
                         RegField(width  , scaleXReg, RegFieldDesc(name = "scaleXReg", desc = "Register used to scale x axis of the signal")))

        // Define abstract register map so it can be AXI4, Tilelink, APB, AHB
        regmap(fields.zipWithIndex.map({ case (f, i) => i * beatBytes -> Seq(f)}): _*)

        // Signals
        val cut     = in.bits.data(log2Ceil(params.dataSize) + 16, log2Ceil(params.dataSize) + 1)
        val tresh   = in.bits.data(log2Ceil(params.dataSize) + 32, log2Ceil(params.dataSize) + 16 + 1)
        val w_cut   = Wire(SInt(16.W)) 
        val w_tresh = Wire(UInt(16.W)) 

        // define type in case of overflow
        val type_cut:   SInt = (w_cut * (1 << params.scale)).cloneType
        val type_tresh: UInt = (w_tresh * (1 << params.scale)).cloneType
        val w_cut_temp = Wire(type_cut)
        val w_tresh_temp = Wire(type_tresh)

        w_cut_temp := cut.asSInt << scaleYReg(width-1,0)
        w_tresh_temp := tresh << scaleYReg(width-1,0)

        when (scaleYReg(width) === 0.U) {
            w_cut := cut.asSInt >> scaleYReg(width-1,0)
            w_tresh := tresh >> scaleYReg(width-1,0)
        }
        .otherwise{
          // check w_cut for overflow
          when (w_cut_temp > BigInt((1 << 15) - 1).S) {
            w_cut := BigInt((1 << 15) - 1).S
          }
          .elsewhen(w_cut_temp < BigInt(-1 << 15).S) {
            w_cut := BigInt(1 << 15).S
          }
          .otherwise {
            w_cut := cut.asSInt << scaleYReg(width-1,0)
          }
          // check w_tresh for overflow
          when (w_tresh_temp > BigInt((1 << 16) - 1).U) {
            w_tresh := BigInt((1 << 16) - 1).U
          }
          .otherwise {
            w_tresh := tresh << scaleYReg(width-1,0)
          }
        }
        out.bits.data := Cat(in.bits.data(in.bits.data.getWidth - 1, log2Ceil(params.dataSize) + 32 + 1), w_tresh, w_cut, in.bits.data(log2Ceil(params.dataSize), 0))
        in.ready  := out.ready
        out.valid := in.valid
    }  
}

class AXI4Scaler1DBlock(params: Scaler1DParams, address: AddressSet, beatBytes: Int = 4)(implicit p: Parameters) extends Scaler1D[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
}

trait AXI4Scaler1DStandaloneBlock extends AXI4DspBlock {

  val beatBytes = 4

  // Generate AXI4 slave output
  def standaloneParams = AXI4BundleParameters(addrBits = beatBytes*8*2, dataBits = beatBytes*8*2, idBits = 1)
  val ioMem = mem.map { m => {
    val ioMemNode = BundleBridgeSource(() => AXI4Bundle(standaloneParams))
    m := BundleBridgeToAXI4(AXI4MasterPortParameters(Seq(AXI4MasterParameters("bundleBridgeToAXI4")))) := ioMemNode
    val ioMem = InModuleBody { ioMemNode.makeIO() }
    ioMem
  }}

  val ioInNode = BundleBridgeSource(() => new AXI4StreamBundle(AXI4StreamBundleParameters(n = beatBytes)))
  val ioOutNode = BundleBridgeSink[AXI4StreamBundle]()

  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = 6)) := ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

object Scaler1DApp extends App
{
  val params = Scaler1DParams(
    dataSize = 1024,
    scale = log2Ceil(256),
  )
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AXI4Scaler1DBlock(params, AddressSet(0x00, 0xF), beatBytes = 4) with AXI4Scaler1DStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/Scaler1D"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}