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

// Scaler parameters
case class ScalerParams(
  scale   : Int,  // Scaler
  complex : Boolean = true, // Complex or real signal
)

abstract class Scaler [D, U, E, O, B <: Data] (params: ScalerParams, beatBytes: Int) extends LazyModule()(Parameters.empty) with DspBlock[D, U, E, O, B] with HasCSR {

    val streamNode = AXI4StreamIdentityNode()
    
    lazy val module = new LazyModuleImp(this) {
        val (in, _)  = streamNode.in(0)
        val (out, _) = streamNode.out(0)

        val width = log2Ceil(params.scale)
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

        // If signals are complex
        if (params.complex) {
          // Real and imag signals
          val real = Wire(SInt((4*beatBytes).W)) 
          val imag = Wire(SInt((4*beatBytes).W)) 

          // define type in case of overflow
          val typeT: SInt = (real * (1 << params.scale)).cloneType
          val realWire = Wire(typeT)
          val imagWire = Wire(typeT)

          realWire := (in.bits.data(8*beatBytes-1,4*beatBytes)).asSInt << scaleYReg(width-1,0)
          imagWire := (in.bits.data(4*beatBytes-1,0)).asSInt << scaleYReg(width-1,0)

          when (scaleYReg(width) === 0.U) {
              real := (in.bits.data(8*beatBytes-1,4*beatBytes)).asSInt >> scaleYReg(width-1,0)
              imag := (in.bits.data(4*beatBytes-1,0)).asSInt >> scaleYReg(width-1,0)
          }
          .otherwise{
            // check real for overflow
            when (realWire > BigInt((1 << (4*beatBytes - 1)) - 1).S) {
              real := BigInt((1 << (4*beatBytes - 1)) - 1).S
            }
            .elsewhen(realWire < BigInt(-1 << (4*beatBytes - 1)).S) {
              real := BigInt(1 << (4*beatBytes - 1)).S
            }
            .otherwise {
              real := (in.bits.data(8*beatBytes-1,4*beatBytes)).asSInt << scaleYReg(width-1,0)
            }
            // check imag for overflow
            when (imagWire > BigInt((1 << (4*beatBytes - 1)) - 1).S) {
              imag := BigInt((1 << (4*beatBytes - 1)) - 1).S
            }
            .elsewhen(imagWire < BigInt(-1 << (4*beatBytes - 1)).S) {
              imag := BigInt(1 << (4*beatBytes - 1)).S
            }
            .otherwise {
              imag := (in.bits.data(4*beatBytes-1,0)).asSInt << scaleYReg(width-1,0)
            }
          }
          out.bits.data := Cat(real,imag)
        }
        // Signals only have real value
        else {
          // define type in case of overflow
          val typeT: SInt = (in.bits.data.asSInt * (1 << params.scale)).cloneType
          val dataWire = Wire(typeT)

          dataWire := in.bits.data.asSInt << scaleYReg(width-1,0)

          when (scaleYReg(width) === 0.U) {
              out.bits.data := in.bits.data >> scaleYReg(width-1,0)
          }
          .otherwise{
            // Check for overflow
            when (dataWire > BigInt((1 << (8*beatBytes - 1)) - 1).S) {
              out.bits.data := BigInt((1 << (8*beatBytes - 1)) - 1).S.asUInt
            }
            .elsewhen(dataWire < BigInt(-1 << (8*beatBytes - 1)).S) {
              out.bits.data := BigInt(1 << (8*beatBytes - 1)).S.asUInt
            }
            .otherwise {
              out.bits.data := in.bits.data << scaleYReg(width-1,0)
            }
          }
        }
        in.ready  := out.ready
        out.valid := in.valid
    }  
}

class AXI4ScalerBlock(params: ScalerParams, address: AddressSet, beatBytes: Int = 4)(implicit p: Parameters) extends Scaler[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4EdgeParameters, AXI4Bundle](params, beatBytes) with AXI4DspBlock with AXI4HasCSR {
  override val mem = Some(AXI4RegisterNode(address = address, beatBytes = beatBytes))
}

trait AXI4ScalerStandaloneBlock extends AXI4DspBlock {

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

  ioOutNode := AXI4StreamToBundleBridge(AXI4StreamSlaveParameters()) := streamNode := BundleBridgeToAXI4Stream(AXI4StreamMasterParameters(n = beatBytes)) := ioInNode

  val in = InModuleBody { ioInNode.makeIO() }
  val out = InModuleBody { ioOutNode.makeIO() }
}

object ScalerApp extends App
{
  val params = ScalerParams(
    scale   = log2Ceil(256),
    complex = true
  )
  implicit val p: Parameters = Parameters.empty
  
  val lazyDut = LazyModule(new AXI4ScalerBlock(params, AddressSet(0x00, 0xF), beatBytes = 4) with AXI4ScalerStandaloneBlock)
  (new ChiselStage).execute(Array("--target-dir", "verilog/Scaler"), Seq(ChiselGeneratorAnnotation(() => lazyDut.module)))
}