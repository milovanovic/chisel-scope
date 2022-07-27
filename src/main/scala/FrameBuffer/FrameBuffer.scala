package hdmi.frameBuffer

import dsptools.numbers._

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.experimental._
import chisel3.util._

import javax.imageio.ImageIO
import java.io.File

class ImageRom (image: String) {
    val photo = ImageIO.read(new File(image))
    val w = photo.getWidth
    val h = photo.getHeight
    val imageROM = VecInit(Seq.tabulate(h) { index_y => VecInit(Seq.tabulate(w) { index_x => {if ((photo.getRGB(index_x, index_y) & 0xffffff).toInt < 0x808080) false.B else true.B}})})
}

case class ImageParameters (
    // image parameters
    h_video       : Int,
    h_fp          : Int,
    h_sync        : Int,
    h_bp          : Int,
    h_total       : Int,
    v_video       : Int,
    v_fp          : Int,
    v_sync        : Int,
    v_bp          : Int,
    v_total       : Int,
    h_pol         : Int,
    v_pol         : Int,
    active        : Int,
    // offset parameters
    offset_top    : Int,
    offset_bottom : Int,
    offset_left   : Int,
    offset_right  : Int,
    // graph parameters
    div_size_x    : Int,
    div_x         : Int,
    div_size_y_1D : Int,
    div_y_1D      : Int,
    div_size_y_2D : Int,
    div_y_2D      : Int,
    axis_size     : Int
)

case class FrameBufferParameters[T <: Data: Real: BinaryRepresentation] (
  val proto0 : T,
  val proto1 : T,
  val proto2 : T,
  val imageParams : ImageParameters,
  val beatBytes   : Int
)

// FrameBuffer IO
class FrameBufferIO[T <: Data: Real](val params: FrameBufferParameters[T], val scalerWidth: Int) extends Bundle {
    // Timing signals
    val video_active = Input(Bool())
    val pixel_x      = Input(UInt(16.W))
    val pixel_y      = Input(UInt(16.W))
    // RGB data
    val rgb  = Output(UInt(24.W))
    // Start signal
    val start   = Output(Bool())
    // Load scaler
    val loadScaler = Output(Bool())
    // X_location
    val o_addr_x_1D = Output(UInt(9.W))
    // Input FFT 1D data
    val i_CUT       = Input(params.proto1)
    val i_Treshold  = Input(params.proto1)
    val i_Peak      = Input(params.proto2)
    // Input FFT 2D data
    val i_FFT_2D = Input(UInt(24.W))
    // X_location
    val o_addr_x_2D = Output(UInt(9.W))
    val o_addr_y_2D = Output(UInt(8.W))
    // Scaler signals
    val i_scaler = Input(UInt(10.W))
}

object FrameBufferIO {
  def apply[T <: Data : Real](params: FrameBufferParameters[T], scalerWidth: Int): FrameBufferIO[T] = new FrameBufferIO(params, scalerWidth)
}

class FrameBuffer[T <: Data: Real](params: FrameBufferParameters[T], scalerWidth: Int) extends Module {

    // FrameBuffer IO
    val io = IO(new FrameBufferIO(params, scalerWidth))

    // RGB
    val rgb = RegInit(0.U(24.W))
    io.rgb := rgb

    // Color values
    val grey_px      = Cat( 63.U(8.W),  63.U(8.W),  63.U(8.W))
    val black_px     = Cat(  0.U(8.W),   0.U(8.W),   0.U(8.W))
    val white_px     = Cat(255.U(8.W), 255.U(8.W), 255.U(8.W))
    val green_px     = Cat(  0.U(8.W), 255.U(8.W),   0.U(8.W))
    val yellow_px    = Cat(255.U(8.W), 255.U(8.W),  51.U(8.W))
    val red_px       = Cat(237.U(8.W),  26.U(8.W),  59.U(8.W))
    val blue_px      = Cat(  0.U(8.W),   0.U(8.W), 255.U(8.W))
    val pink_px      = Cat(255.U(8.W),   0.U(8.W), 127.U(8.W))
    val orange_px    = Cat(255.U(8.W), 127.U(8.W),   0.U(8.W))
    val purple_px    = Cat(127.U(8.W),   0.U(8.W), 255.U(8.W))
    val turquoise_px = Cat(  0.U(8.W), 204.U(8.W), 204.U(8.W))


    // Axis parameters
    val axis_size     = params.imageParams.axis_size
    val offset_left   = params.imageParams.offset_left
    val offset_top    = params.imageParams.offset_top
    val offset_right  = params.imageParams.offset_right
    val offset_bottom = params.imageParams.offset_bottom
    val div_x         = params.imageParams.div_x
    val div_size_x    = params.imageParams.div_size_x
    val div_y_1D      = params.imageParams.div_y_1D
    val div_size_y_1D = params.imageParams.div_size_y_1D
    val div_y_2D      = params.imageParams.div_y_2D
    val div_size_y_2D = params.imageParams.div_size_y_2D
    
    val x_start_sig  = offset_left
    val x_start_addr = x_start_sig - 2.U
    val x_end_sig    = offset_left + div_x*div_size_x
    val x_end_addr   = x_end_sig - 2.U
    val x_start      = offset_left - axis_size/2
    val x_end        = offset_left + div_x*div_size_x + axis_size/2

    val y_start_1D   = offset_top - axis_size/2
    val y_end_1D     = offset_top + axis_size/2 + div_size_y_1D*div_y_1D

    val y_start_2D     = div_y_1D*div_size_y_1D + 2*offset_top - axis_size
    val y_end_2D       = div_y_1D*div_size_y_1D + 2*offset_top + div_y_2D*div_size_y_2D + axis_size
    val y_start_sig_2D = div_y_1D*div_size_y_1D + 2*offset_top
    val y_end_sig_2D   = div_y_1D*div_size_y_1D + 2*offset_top + div_y_2D*div_size_y_2D

    // Connect inputs video_active, pixel_x and pixel_y to regs
    val video_active = RegInit(false.B)
    val pixel_x      = RegInit(0.U(16.W))
    val pixel_y      = RegInit(0.U(16.W))
    video_active := io.video_active
    pixel_x      := io.pixel_x
    pixel_y      := io.pixel_y

    // Delayed 1D inputs
    val inTreshold_delayed = RegInit(0.U.asTypeOf(params.proto1))
    val i_CUT      = RegInit(0.U.asTypeOf(params.proto1))
    val i_Treshold = RegInit(0.U.asTypeOf(params.proto1))
    val i_Peak     = RegInit(0.U.asTypeOf(params.proto2))
    i_CUT  := io.i_CUT
    i_Treshold  := io.i_Treshold
    i_Peak  := io.i_Peak
    // Delayed 2D inputs
    val i_FFT_2D = RegInit(0.U(24.W))
    i_FFT_2D := io.i_FFT_2D

    when (io.o_addr_x_1D < (x_end - x_start).U) {
        inTreshold_delayed := i_Treshold
    }

    // relative location of coordinate X
    val w_temp_addr_x_1D = pixel_x - x_start_addr
    when ((pixel_x >= x_start_addr) && (pixel_x < x_end_addr)) {
        io.o_addr_x_1D := w_temp_addr_x_1D(9,1)
    }
    .otherwise {
        io.o_addr_x_1D := 0.U
    }

    // Scaler registers
    val i_scaler = RegNext(RegNext(io.i_scaler, 0.U), 0.U)

    // relative location of coordinate X
    val w_temp_addrx = pixel_x - x_start_sig
    when ((pixel_x >= x_start_sig) && (pixel_x < x_end_sig)) {
        io.o_addr_x_2D := w_temp_addrx(9,1) >> i_scaler
    }
    .otherwise {
        io.o_addr_x_2D := 0.U
    }
    // relative location of coordinate Y
    val w_temp_addry = pixel_y - y_start_sig_2D
    when ((pixel_y >= y_start_sig_2D) && (pixel_y < y_end_sig_2D)) {
        io.o_addr_y_2D := w_temp_addry(8,1)
    }
    .otherwise {
        io.o_addr_y_2D := 0.U
    }

    // X line condition
    var seq_x = Seq[Bool]()
    for (i <- 1 until div_x) {
        seq_x = seq_x :+ ((pixel_x > (x_start + i*div_size_x)) && (pixel_x <= x_start + i*div_size_x + axis_size))
    }
    val x_cond = seq_x.reduce(_ || _)

    // Y line 1D condition
    var seq_y_1D = Seq[Bool]()
    for (i <- 1 until div_y_1D) {
        seq_y_1D = seq_y_1D :+ ((pixel_y > (y_start_1D + i*div_size_y_1D)) && (pixel_y <= y_start_1D + i*div_size_y_1D + axis_size))
    }
    val y_cond_1D = seq_y_1D.reduce(_ || _)

    // FFT signal cond
    val signal_fft_cond   = RegInit(0.U.asTypeOf(params.proto1))
    val signal_fft_cond_1 = RegInit(0.U.asTypeOf(params.proto1))
    val signal_fft_cond_2 = RegInit(0.U.asTypeOf(params.proto1))
    signal_fft_cond   := ((y_end_1D - axis_size/2).U - pixel_y).asTypeOf(params.proto1)
    signal_fft_cond_1 := ((y_end_1D - 1 - axis_size/2).U - pixel_y).asTypeOf(params.proto1)
    signal_fft_cond_2 := ((y_end_1D - 2 - axis_size/2).U - pixel_y).asTypeOf(params.proto1)

    // Peak signal cond
    val signal_peak_cond   = RegInit(0.U.asTypeOf(params.proto1))
    val signal_peak_cond_2 = RegInit(0.U.asTypeOf(params.proto1))
    signal_peak_cond   := ((y_end_1D + axis_size).U - pixel_y).asTypeOf(params.proto1)
    signal_peak_cond_2 := ((y_end_1D).U - pixel_y).asTypeOf(params.proto1)

    // FFT 1D signal condition
    val cut_cond = RegInit(false.B)
    cut_cond := ((pixel_x > x_start_sig) && (pixel_x <= x_end_sig)) && ((pixel_y > y_start_1D + axis_size/2) && (pixel_y <= y_end_1D - axis_size/2)) && (i_CUT >= signal_fft_cond)
    // Treshold condition
    val tresh_cond = RegInit(false.B)
    tresh_cond := ((pixel_x > x_start_sig) && (pixel_x <= x_end_sig)) && ((pixel_y > y_start_1D + axis_size/2) && (pixel_y <= y_end_1D - axis_size/2)) && 
                  ((i_Treshold === signal_fft_cond) || (i_Treshold === signal_fft_cond_1) || (i_Treshold === signal_fft_cond_2) || (i_Treshold < signal_fft_cond && inTreshold_delayed > signal_fft_cond) || (i_Treshold > signal_fft_cond && inTreshold_delayed < signal_fft_cond))
    // Peak condition
    val peak_cond = RegInit(false.B)
    peak_cond := ((pixel_x > x_start) && (pixel_x <= x_end)) && ((pixel_y > y_end_1D) && (pixel_y <= y_end_1D + 3*axis_size)) && (i_Peak.asUInt >= 1.U)
    // FFT border line condition
    val fft_1D_border_cond = RegInit(false.B)
    fft_1D_border_cond := (((pixel_x > x_start) && (pixel_x <= x_end)) && (((pixel_y > y_start_1D) && (pixel_y <= y_start_1D + axis_size)) || ((pixel_y > y_end_1D - axis_size) && (pixel_y <= y_end_1D)))) ||
                          (((pixel_y > y_start_1D + axis_size) && (pixel_y <= y_end_1D - axis_size)) && (((pixel_x > x_start) && (pixel_x <= x_start + axis_size)) || ((pixel_x > x_end - axis_size) && (pixel_x <= x_end))))
    // FFT grid line condition
    val fft_1D_grid_cond = RegInit(false.B)
    fft_1D_grid_cond := (((pixel_x > x_start + axis_size) && (pixel_x < x_end - axis_size)) && y_cond_1D) || (((pixel_y > y_start_1D + axis_size) && (pixel_y <= y_end_1D - axis_size)) && x_cond)


    // FFT 2D signal condition
    val fft_2D_cond = RegInit(false.B)
    fft_2D_cond := ((pixel_x >= x_start_sig) && (pixel_x < x_end_sig)) && ((pixel_y >= y_start_sig_2D) && (pixel_y < y_end_sig_2D))
    // Border line condition
    val fft_2D_border_cond = RegInit(false.B)
    fft_2D_border_cond := (((pixel_x >= x_start) && (pixel_x < x_end)) && (((pixel_y >= y_start_2D) && (pixel_y < y_start_2D + axis_size)) || ((pixel_y >= y_end_2D - axis_size) && (pixel_y < y_end_2D)))) ||
                          (((pixel_y >= y_start_2D + axis_size) && (pixel_y < y_end_2D - axis_size)) && (((pixel_x >= x_start) && (pixel_x < x_start + axis_size)) || ((pixel_x >= x_end - axis_size) && (pixel_x < x_end)))) 



    // Draw
    when (video_active) {
    // FFT 1D
        // Draw Treshold signal
        when (tresh_cond){
            rgb := yellow_px // send yellow pixels
        }
        // Draw FFT signal
        .elsewhen (cut_cond){
            // when(i_Peak.asUInt >= 1.U) {
            //     rgb := red_px // send red pixels
            // }
            when(i_CUT >= i_Treshold) {
                rgb := red_px // send red pixels
            }
            .otherwise {
                rgb := turquoise_px // send yellow pixels
            }
        }
        // Draw peak signals
        .elsewhen (peak_cond){
            rgb := green_px // send green pixels
        }
        // Draw border lines
        .elsewhen (fft_1D_border_cond || fft_2D_border_cond) {
            rgb := white_px // send white pixels
        }
        // Draw grid lines
        .elsewhen (fft_1D_grid_cond) {
            rgb := grey_px // send grey pixels
        }
        .elsewhen (fft_2D_cond){
            rgb := i_FFT_2D // send calculated pixels
        }
        // Otherwise send black pixels
        .otherwise {
            rgb := black_px // send black pixels
        }
    }
    .otherwise {
        rgb := black_px // send black pixels
    }
    
    // Generate start signal
    when (video_active) {
        // Start
        when((pixel_x >= x_start_addr) && (pixel_x < x_end_addr)) {
            io.start := true.B
        }
        .otherwise {
            io.start := false.B
        }
        // Load scaler
        when (pixel_y === (params.imageParams.v_video-1).U) {
            io.loadScaler := true.B
        }
        .otherwise {
            io.loadScaler := false.B
        }
    }
    .otherwise{
        io.start      := false.B
        io.loadScaler := false.B
    }
}

class HD1080p {
    val params = ImageParameters (
        // image parameters
        h_video       = 1920,
        h_fp          = 88,
        h_sync        = 44,
        h_bp          = 148,
        h_total       = 2200,
        v_video       = 1080,
        v_fp          = 4,
        v_sync        = 5,
        v_bp          = 36,
        v_total       = 1125,
        h_pol         = 1,
        v_pol         = 1,
        active        = 1,
        // offset parameters
        offset_top    = 64,
        offset_bottom = 64,
        offset_left   = 64,
        offset_right  = 64,
        // graph parameters
        div_size_x    = 192,
        div_x         = 8,
        div_size_y_1D = 32,
        div_y_1D      = 12,
        div_size_y_2D = 128,
        div_y_2D      = 4,
        axis_size     = 2,
    )
}

class HD1080pNoInterpolation {
    val params = ImageParameters (
        // image parameters
        h_video       = 1920,
        h_fp          = 88,
        h_sync        = 44,
        h_bp          = 148,
        h_total       = 2200,
        v_video       = 1080,
        v_fp          = 4,
        v_sync        = 5,
        v_bp          = 36,
        v_total       = 1125,
        h_pol         = 1,
        v_pol         = 1,
        active        = 1,
        // offset parameters
        offset_top    = 64,
        offset_bottom = 64,
        offset_left   = 64 + 320,
        offset_right  = 64,
        // graph parameters
        div_size_x    = 128,
        div_x         = 8,
        div_size_y_1D = 32,
        div_y_1D      = 12,
        div_size_y_2D = 128,
        div_y_2D      = 4,
        axis_size     = 2,
    )
}
object FrameBufferApp extends App
{
  val HD1080p = ImageParameters (
    // image parameters
    h_video       = 1920,
    h_fp          = 88,
    h_sync        = 44,
    h_bp          = 148,
    h_total       = 2200,
    v_video       = 1080,
    v_fp          = 4,
    v_sync        = 5,
    v_bp          = 36,
    v_total       = 1125,
    h_pol         = 1,
    v_pol         = 1,
    active        = 1,
    // offset parameters
    offset_top    = 64,
    offset_bottom = 64,
    offset_left   = 64,
    offset_right  = 64,
    // graph parameters
    div_size_x    = 192,
    div_x         = 8,
    div_size_y_1D = 32,
    div_y_1D      = 12,
    div_size_y_2D = 128,
    div_y_2D      = 4,
    axis_size     = 2,
)
  // here just define parameters
  val params : FrameBufferParameters[FixedPoint] = FrameBufferParameters(
      proto0 = FixedPoint(16.W, 10.BP),
      proto1 = FixedPoint(16.W, 10.BP),
      proto2 = FixedPoint( 8.W,  0.BP),
      imageParams = HD1080p,
      beatBytes = 4
  )

  (new ChiselStage).execute(Array("--target-dir", "verilog/FrameBuffer"), Seq(ChiselGeneratorAnnotation(() => new FrameBuffer(params, 3))))
}