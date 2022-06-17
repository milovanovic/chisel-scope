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
    div_size_y    : Int,
    div_y         : Int,
    div_size_y_2  : Int,
    div_y_2       : Int,
    axis_size     : Int
)

case class FrameBufferParameters[T <: Data: Real: BinaryRepresentation] (
  proto0 : T,
  proto1 : T,
  proto2 : T,
  imageParams : ImageParameters,
  beatBytes   : Int
){
  requireIsChiselType(proto0,  s"($proto0) must be chisel type")
}

// FrameBuffer IO
class FrameBufferIO[T <: Data: Real](params: FrameBufferParameters[T], scalerWidth: Int) extends Bundle {
    // Timing signals
    val video_active = Input(Bool())
    val pixel_x      = Input(UInt(16.W))
    val pixel_y      = Input(UInt(16.W))
    // RGB data
    val rgb          = Output(UInt(24.W))
    // Start and triggered signals
    val start     = Output(Bool())
    val triggered = Output(Bool())
    // Load scaler
    val loadScaler = Output(Bool())
    // X_location
    val x_location     = Output(UInt(16.W))
    val x_location_fft = Output(UInt(16.W))
    // Input signal data
    val inReal = Input(params.proto0)
    val inImag = Input(params.proto0)
    val inCUT = Input(params.proto1)
    val inTreshold  = Input(params.proto1)
    val inPeak  = Input(params.proto2)

    val scaler_y     = Input(UInt((scalerWidth+1).W))
    val scaler_x     = Input(UInt((scalerWidth).W))
    val fft_scaler_y = Input(UInt((scalerWidth+1).W))
    val fft_scaler_x = Input(UInt((scalerWidth).W))

    override def cloneType: this.type = FrameBufferIO(params,scalerWidth).asInstanceOf[this.type]
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
    val div_y         = params.imageParams.div_y
    val div_size_y    = params.imageParams.div_size_y
    val div_y_2       = params.imageParams.div_y_2
    val div_size_y_2  = params.imageParams.div_size_y_2
    
    val x_start_sig  = offset_left
    val x_end_sig    = offset_left + div_x*div_size_x
    
    val x_start   = offset_left - axis_size/2
    val x_end     = offset_left + div_x*div_size_x + axis_size/2

    val y_start   = offset_top - axis_size/2
    val y_mid     = offset_top + div_size_y*div_y/2
    val y_end     = offset_top + axis_size/2 + div_size_y*div_y

    val y_start_2 = div_y*div_size_y + 2*offset_top - axis_size/2
    val y_mid_2   = y_start_2 + axis_size + div_size_y*div_y/2
    val y_end_2   = div_y*div_size_y + 2*offset_top + div_y_2*div_size_y_2 + axis_size/2

    // Connect inputs video_active, pixel_x and pixel_y to regs
    val video_active = RegInit(false.B)
    val pixel_x      = RegInit(0.U(16.W))
    val pixel_y      = RegInit(0.U(16.W))
    video_active := io.video_active
    pixel_x      := io.pixel_x
    pixel_y      := io.pixel_y

    // Delayed inputs
    val inReal_delayed = RegInit(0.U.asTypeOf(params.proto0))
    val inImag_delayed = RegInit(0.U.asTypeOf(params.proto0))
    val inTreshold_delayed = RegInit(0.U.asTypeOf(params.proto1))

    val inReal     = RegInit(0.U.asTypeOf(params.proto0))
    val inImag     = RegInit(0.U.asTypeOf(params.proto0))
    val inCUT      = RegInit(0.U.asTypeOf(params.proto1))
    val inTreshold = RegInit(0.U.asTypeOf(params.proto1))
    val inPeak     = RegInit(0.U.asTypeOf(params.proto2))
    inReal := io.inReal
    inImag := io.inImag
    inCUT  := io.inCUT
    inTreshold  := io.inTreshold
    inPeak  := io.inPeak

    when (io.x_location < (x_end - x_start).U) {
        inReal_delayed := inReal
        inImag_delayed := inImag
        inTreshold_delayed := inTreshold
    }

    // Y line condition
    var seq_y = Seq[Bool]()
    for (i <- 1 until div_y) {
        seq_y = seq_y :+ ((pixel_y > (y_start + i*div_size_y)) && (pixel_y <= y_start + i*div_size_y + axis_size))
    }
    val y_cond = seq_y.reduce(_ || _)

    // X line condition
    var seq_x = Seq[Bool]()
    for (i <- 1 until div_x) {
        seq_x = seq_x :+ ((pixel_x > (x_start + i*div_size_x)) && (pixel_x <= x_start + i*div_size_x + axis_size))
    }
    val x_cond = seq_x.reduce(_ || _)

    // Y line 2 condition
    var seq_y_2 = Seq[Bool]()
    for (i <- 1 until div_y_2) {
        seq_y_2 = seq_y_2 :+ ((pixel_y > (y_start_2 + i*div_size_y_2)) && (pixel_y <= y_start_2 + i*div_size_y_2 + axis_size))
    }
    val y_cond_2 = seq_y_2.reduce(_ || _)

    // Time signal cond
    val signal_cond   = RegInit(0.U.asTypeOf(params.proto0))
    val signal_cond_2 = RegInit(0.U.asTypeOf(params.proto0))
    signal_cond   := (y_mid.U - pixel_y).asTypeOf(params.proto0)
    signal_cond_2 := ((y_mid-1).U-pixel_y).asTypeOf(params.proto0)

    // FFT signal cond
    val signal_fft_cond   = RegInit(0.U.asTypeOf(params.proto1))
    val signal_fft_cond_1 = RegInit(0.U.asTypeOf(params.proto1))
    val signal_fft_cond_2 = RegInit(0.U.asTypeOf(params.proto1))
    signal_fft_cond   := ((y_end_2 - axis_size/2).U - pixel_y).asTypeOf(params.proto1)
    signal_fft_cond_1 := ((y_end_2 - 1 - axis_size/2).U - pixel_y).asTypeOf(params.proto1)
    signal_fft_cond_2 := ((y_end_2 - 2 - axis_size/2).U - pixel_y).asTypeOf(params.proto1)

    // Peak signal cond
    val signal_peak_cond   = RegInit(0.U.asTypeOf(params.proto1))
    val signal_peak_cond_2 = RegInit(0.U.asTypeOf(params.proto1))
    signal_peak_cond   := ((y_end_2 + axis_size).U - pixel_y).asTypeOf(params.proto1)
    signal_peak_cond_2 := ((y_end_2).U - pixel_y).asTypeOf(params.proto1)

    // Scaler registers
    val scaler_x     = RegInit(0.U(scalerWidth.W))
    val fft_scaler_x = RegInit(0.U(scalerWidth.W))
    val scaler_y     = RegInit(0.U((scalerWidth + 1).W))
    val fft_scaler_y = RegInit(0.U((scalerWidth + 1).W))

    // relative location of coordinate X
    when ((pixel_x > x_start_sig) && (pixel_x < x_end_sig)) {
        io.x_location     := (pixel_x - x_start_sig)
        io.x_location_fft := (pixel_x - x_start_sig)
    }
    .otherwise {
        io.x_location     := 0.U
        io.x_location_fft := 0.U
    }

    // load images
    val img_div_x = new ImageRom("./src/main/resources/div_x.jpg")
    val img_div_y = new ImageRom("./src/main/resources/div_y.jpg")
    val img_0     = new ImageRom("./src/main/resources/0.jpg")
    val img_1     = new ImageRom("./src/main/resources/1.jpg")
    val img_2     = new ImageRom("./src/main/resources/2.jpg")
    val img_3     = new ImageRom("./src/main/resources/3.jpg")
    val img_4     = new ImageRom("./src/main/resources/4.jpg")
    val img_5     = new ImageRom("./src/main/resources/5.jpg")
    val img_6     = new ImageRom("./src/main/resources/6.jpg")
    val img_7     = new ImageRom("./src/main/resources/7.jpg")
    val img_8     = new ImageRom("./src/main/resources/8.jpg")
    val img_9     = new ImageRom("./src/main/resources/9.jpg")
    val img_novel = new ImageRom("./src/main/resources/novel.jpg")

    // Logo conditions
    val logo_cond = RegInit(false.B)
    logo_cond := (((pixel_x > x_end + offset_right) && (pixel_x <= x_end + offset_right + img_novel.w)) && ((pixel_y > y_start) && (pixel_y <= y_start + img_novel.h)))
    val logo_img = Wire(Bool())
    when (logo_cond) {
        logo_img := img_novel.imageROM(pixel_y - y_start.U)(pixel_x - (x_end + offset_right).U)
    }
    .otherwise {
        logo_img := false.B
    }

    // divs conditions
    val div_x_cond = RegInit(false.B)
    div_x_cond := (((pixel_x > x_start) && (pixel_x <= x_start + img_div_x.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val div_y_cond = RegInit(false.B)
    div_y_cond := (((pixel_x > x_start + img_div_x.w + 5*img_0.w) && (pixel_x <= x_start + img_div_x.w + 5*img_0.w + img_div_y.w)) && ((pixel_y > y_end + (offset_top - img_div_y.h)/2) && (pixel_y <= y_end + (offset_top - img_div_y.h)/2 + img_div_y.h)))
    
    // divs conditions for FFT
    val div_x_cond_2 = RegInit(false.B)
    div_x_cond_2 := (((pixel_x > x_start) && (pixel_x <= x_start + img_div_x.w)) && ((pixel_y > y_end_2 + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end_2 + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val div_y_cond_2 = RegInit(false.B)
    div_y_cond_2 := (((pixel_x > x_start + img_div_x.w + 5*img_0.w) && (pixel_x <= x_start + img_div_x.w + 5*img_0.w + img_div_y.w)) && ((pixel_y > y_end_2 + (offset_top - img_div_y.h)/2) && (pixel_y <= y_end_2 + (offset_top - img_div_y.h)/2 + img_div_y.h)))
    
    val div_x_img = Wire(Bool())
    val div_y_img = Wire(Bool())
    when (div_x_cond) {
        div_x_img := img_div_x.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - x_start.U)
    }
    .elsewhen (div_x_cond_2) {
        div_x_img := img_div_x.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - x_start.U)
    }
    .otherwise {
        div_x_img := false.B
    }
    when (div_y_cond) {
        div_y_img := img_div_y.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 5*img_0.w).U)
    }
    .elsewhen (div_y_cond_2) {
        div_y_img := img_div_y.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 5*img_0.w).U)
    }
    .otherwise {
        div_y_img := false.B
    }

    // Number conditions for div_x
    val num_x_0_cond = RegInit(false.B)
    num_x_0_cond := (((pixel_x > x_start + img_div_x.w + 0*img_0.w) && (pixel_x <= x_start + img_div_x.w + 1*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val num_x_1_cond = RegInit(false.B)
    num_x_1_cond := (((pixel_x > x_start + img_div_x.w + 1*img_0.w) && (pixel_x <= x_start + img_div_x.w + 2*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val num_x_2_cond = RegInit(false.B)
    num_x_2_cond := (((pixel_x > x_start + img_div_x.w + 2*img_0.w) && (pixel_x <= x_start + img_div_x.w + 3*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val num_x_3_cond = RegInit(false.B)
    num_x_3_cond := (((pixel_x > x_start + img_div_x.w + 3*img_0.w) && (pixel_x <= x_start + img_div_x.w + 4*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))

    val num_x_0 = Wire(Bool())
    val num_x_1 = Wire(Bool())
    val num_x_2 = Wire(Bool())
    val num_x_3 = Wire(Bool())

    when(scaler_x === 0.U) {
        when (num_x_0_cond) {
            num_x_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            num_x_0 := true.B
        }
        when (num_x_1_cond) {
            num_x_1 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
        }
        .otherwise {
            num_x_1 := true.B
        }
        when (num_x_2_cond) {
            num_x_2 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 2*img_0.w).U)
        }
        .otherwise {
            num_x_2 := true.B
        }
        num_x_3 := true.B
    }
    .elsewhen (scaler_x === 1.U) {
        when (num_x_0_cond) {
            num_x_0 := img_6.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            num_x_0 := true.B
        }
        when (num_x_1_cond) {
            num_x_1 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
        }
        .otherwise {
            num_x_1 := true.B
        }
        num_x_2 := true.B
        num_x_3 := true.B
    }
    .elsewhen (scaler_x === 2.U) {
        when (num_x_0_cond) {
            num_x_0 := img_3.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            num_x_0 := true.B
        }
        when (num_x_1_cond) {
            num_x_1 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
        }
        .otherwise {
            num_x_1 := true.B
        }
        num_x_2 := true.B
        num_x_3 := true.B
    }
    .elsewhen (scaler_x === 3.U) {
        when (num_x_0_cond) {
            num_x_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            num_x_0 := true.B
        }
        when (num_x_1_cond) {
            num_x_1 := img_6.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
        }
        .otherwise {
            num_x_1 := true.B
        }
        num_x_2 := true.B
        num_x_3 := true.B
    }
    .elsewhen (scaler_x === 4.U) {
        when (num_x_0_cond) {
            num_x_0 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            num_x_0 := true.B
        }
        num_x_1 := true.B
        num_x_2 := true.B
        num_x_3 := true.B
    }
    .elsewhen (scaler_x === 5.U) {
        when (num_x_0_cond) {
            num_x_0 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            num_x_0 := true.B
        }
        num_x_1 := true.B
        num_x_2 := true.B
        num_x_3 := true.B
    }
    .elsewhen (scaler_x === 6.U) {
        when (num_x_0_cond) {
            num_x_0 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            num_x_0 := true.B
        }
        num_x_1 := true.B
        num_x_2 := true.B
        num_x_3 := true.B
    }
    .elsewhen (scaler_x === 7.U) {
        when (num_x_0_cond) {
            num_x_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            num_x_0 := true.B
        }
        num_x_1 := true.B
        num_x_2 := true.B
        num_x_3 := true.B
    }
    .otherwise {
        when (num_x_0_cond) {
            num_x_0 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            num_x_0 := true.B
        }
        when (num_x_1_cond) {
            num_x_1 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
        }
        .otherwise {
            num_x_1 := true.B
        }
        when (num_x_2_cond) {
            num_x_2 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 2*img_0.w).U)
        }
        .otherwise {
            num_x_2 := true.B
        }
        when (num_x_3_cond) {
            num_x_3 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 3*img_0.w).U)
        }
        .otherwise {
            num_x_3 := true.B
        }
    }

    // Number conditions for fft div_x
    val fft_num_x_0_cond = RegInit(false.B)
    fft_num_x_0_cond := (((pixel_x > x_start + img_div_x.w + 0*img_0.w) && (pixel_x <= x_start + img_div_x.w + 1*img_0.w)) && ((pixel_y > y_end_2 + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end_2 + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val fft_num_x_1_cond = RegInit(false.B)
    fft_num_x_1_cond := (((pixel_x > x_start + img_div_x.w + 1*img_0.w) && (pixel_x <= x_start + img_div_x.w + 2*img_0.w)) && ((pixel_y > y_end_2 + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end_2 + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val fft_num_x_2_cond = RegInit(false.B)
    fft_num_x_2_cond := (((pixel_x > x_start + img_div_x.w + 2*img_0.w) && (pixel_x <= x_start + img_div_x.w + 3*img_0.w)) && ((pixel_y > y_end_2 + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end_2 + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val fft_num_x_3_cond = RegInit(false.B)
    fft_num_x_3_cond := (((pixel_x > x_start + img_div_x.w + 3*img_0.w) && (pixel_x <= x_start + img_div_x.w + 4*img_0.w)) && ((pixel_y > y_end_2 + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end_2 + (offset_top - img_div_x.h)/2 + img_div_x.h)))

    val fft_num_x_0 = Wire(Bool())
    val fft_num_x_1 = Wire(Bool())
    val fft_num_x_2 = Wire(Bool())
    val fft_num_x_3 = Wire(Bool())

    when(fft_scaler_x === 0.U) {
        when (fft_num_x_0_cond) {
            fft_num_x_0 := img_1.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            fft_num_x_0 := true.B
        }
        when (fft_num_x_1_cond) {
            fft_num_x_1 := img_2.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
        }
        .otherwise {
            fft_num_x_1 := true.B
        }
        when (fft_num_x_2_cond) {
            fft_num_x_2 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 2*img_0.w).U)
        }
        .otherwise {
            fft_num_x_2 := true.B
        }
        fft_num_x_3 := true.B
    }
    .elsewhen (fft_scaler_x === 1.U) {
        when (fft_num_x_0_cond) {
            fft_num_x_0 := img_6.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            fft_num_x_0 := true.B
        }
        when (fft_num_x_1_cond) {
            fft_num_x_1 := img_4.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
        }
        .otherwise {
            fft_num_x_1 := true.B
        }
        fft_num_x_2 := true.B
        fft_num_x_3 := true.B
    }
    .elsewhen (fft_scaler_x === 2.U) {
        when (fft_num_x_0_cond) {
            fft_num_x_0 := img_3.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            fft_num_x_0 := true.B
        }
        when (fft_num_x_1_cond) {
            fft_num_x_1 := img_2.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
        }
        .otherwise {
            fft_num_x_1 := true.B
        }
        fft_num_x_2 := true.B
        fft_num_x_3 := true.B
    }
    .elsewhen (fft_scaler_x === 3.U) {
        when (fft_num_x_0_cond) {
            fft_num_x_0 := img_1.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            fft_num_x_0 := true.B
        }
        when (fft_num_x_1_cond) {
            fft_num_x_1 := img_6.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
        }
        .otherwise {
            fft_num_x_1 := true.B
        }
        fft_num_x_2 := true.B
        fft_num_x_3 := true.B
    }
    .elsewhen (fft_scaler_x === 4.U) {
        when (fft_num_x_0_cond) {
            fft_num_x_0 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            fft_num_x_0 := true.B
        }
        fft_num_x_1 := true.B
        fft_num_x_2 := true.B
        fft_num_x_3 := true.B
    }
    .elsewhen (fft_scaler_x === 5.U) {
        when (fft_num_x_0_cond) {
            fft_num_x_0 := img_4.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            fft_num_x_0 := true.B
        }
        fft_num_x_1 := true.B
        fft_num_x_2 := true.B
        fft_num_x_3 := true.B
    }
    .elsewhen (fft_scaler_x === 6.U) {
        when (fft_num_x_0_cond) {
            fft_num_x_0 := img_2.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            fft_num_x_0 := true.B
        }
        fft_num_x_1 := true.B
        fft_num_x_2 := true.B
        fft_num_x_3 := true.B
    }
    .elsewhen (fft_scaler_x === 7.U) {
        when (fft_num_x_1_cond) {
            fft_num_x_0 := img_1.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            fft_num_x_0 := true.B
        }
        fft_num_x_1 := true.B
        fft_num_x_2 := true.B
        fft_num_x_3 := true.B
    }
    .otherwise {
        when (fft_num_x_0_cond) {
            fft_num_x_0 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
        }
        .otherwise {
            fft_num_x_0 := true.B
        }
        when (fft_num_x_1_cond) {
            fft_num_x_1 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
        }
        .otherwise {
            fft_num_x_1 := true.B
        }
        when (fft_num_x_2_cond) {
            fft_num_x_2 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 2*img_0.w).U)
        }
        .otherwise {
            fft_num_x_2 := true.B
        }
        when (fft_num_x_3_cond) {
            fft_num_x_3 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 3*img_0.w).U)
        }
        .otherwise {
            fft_num_x_3 := true.B
        }
    }

    // Number conditions for div_y
    val num_y_0_cond = RegInit(false.B)
    num_y_0_cond := (((pixel_x > x_start + img_div_x.w + img_div_y.w + 5*img_0.w) && (pixel_x <= x_start + img_div_x.w + img_div_y.w + 6*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val num_y_1_cond = RegInit(false.B)
    num_y_1_cond := (((pixel_x > x_start + img_div_x.w + img_div_y.w + 6*img_0.w) && (pixel_x <= x_start + img_div_x.w + img_div_y.w + 7*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val num_y_2_cond = RegInit(false.B)
    num_y_2_cond := (((pixel_x > x_start + img_div_x.w + img_div_y.w + 7*img_0.w) && (pixel_x <= x_start + img_div_x.w + img_div_y.w + 8*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val num_y_3_cond = RegInit(false.B)
    num_y_3_cond := (((pixel_x > x_start + img_div_x.w + img_div_y.w + 8*img_0.w) && (pixel_x <= x_start + img_div_x.w + img_div_y.w + 9*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))

    val num_y_0 = Wire(Bool())
    val num_y_1 = Wire(Bool())
    val num_y_2 = Wire(Bool())
    val num_y_3 = Wire(Bool())

    when(scaler_y === 0.U || scaler_y === 8.U) {
        when (num_y_0_cond) {
            num_y_0 := img_6.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        when (num_y_1_cond) {
            num_y_1 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            num_y_1 := true.B
        }
        num_y_2 := true.B
        num_y_3 := true.B
    }
    .elsewhen (scaler_y === 1.U) {
        when (num_y_0_cond) {
            num_y_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        when (num_y_1_cond) {
            num_y_1 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            num_y_1 := true.B
        }
        when (num_y_2_cond) {
            num_y_2 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            num_y_2 := true.B
        }
        num_y_3 := true.B
    }
    .elsewhen (scaler_y === 2.U) {
        when (num_y_0_cond) {
            num_y_0 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        when (num_y_1_cond) {
            num_y_1 := img_5.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            num_y_1 := true.B
        }
        when (num_y_2_cond) {
            num_y_2 := img_6.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            num_y_2 := true.B
        }
        num_y_3 := true.B
    }
    .elsewhen (scaler_y === 3.U) {
        when (num_y_0_cond) {
            num_y_0 := img_5.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        when (num_y_1_cond) {
            num_y_1 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            num_y_1 := true.B
        }
        when (num_y_2_cond) {
            num_y_2 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            num_y_2 := true.B
        }
        num_y_3 := true.B
    }
    .elsewhen (scaler_y === 4.U) {
        when (num_y_0_cond) {
            num_y_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        when (num_y_1_cond) {
            num_y_1 := img_0.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            num_y_1 := true.B
        }
        when (num_y_2_cond) {
            num_y_2 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            num_y_2 := true.B
        }
        when (num_y_3_cond) {
            num_y_3 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
        }
        .otherwise {
            num_y_3 := true.B
        }
    }
    .elsewhen (scaler_y === 5.U) {
        when (num_y_0_cond) {
            num_y_0 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        when (num_y_1_cond) {
            num_y_1 := img_0.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            num_y_1 := true.B
        }
        when (num_y_2_cond) {
            num_y_2 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            num_y_2 := true.B
        }
        when (num_y_3_cond) {
            num_y_3 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
        }
        .otherwise {
            num_y_3 := true.B
        }
    }
    .elsewhen (scaler_y === 6.U) {
        when (num_y_0_cond) {
            num_y_0 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        when (num_y_1_cond) {
            num_y_1 := img_0.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            num_y_1 := true.B
        }
        when (num_y_2_cond) {
            num_y_2 := img_9.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            num_y_2 := true.B
        }
        when (num_y_3_cond) {
            num_y_3 := img_6.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
        }
        .otherwise {
            num_y_3 := true.B
        }
    }
    .elsewhen (scaler_y === 7.U) {
        when (num_y_0_cond) {
            num_y_0 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        when (num_y_1_cond) {
            num_y_1 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            num_y_1 := true.B
        }
        when (num_y_2_cond) {
            num_y_2 := img_9.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            num_y_2 := true.B
        }
        when (num_y_3_cond) {
            num_y_3 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
        }
        .otherwise {
            num_y_3 := true.B
        }
    }
    .elsewhen (scaler_y === 9.U) {
        when (num_y_0_cond) {
            num_y_0 := img_3.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        when (num_y_1_cond) {
            num_y_1 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            num_y_1 := true.B
        }
        num_y_2 := true.B
        num_y_3 := true.B
    }
    .elsewhen (scaler_y === 10.U) {
        when (num_y_0_cond) {
            num_y_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        when (num_y_1_cond) {
            num_y_1 := img_6.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            num_y_1 := true.B
        }
        num_y_2 := true.B
        num_y_3 := true.B
    }
    .elsewhen (scaler_y === 11.U) {
        when (num_y_0_cond) {
            num_y_0 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        num_y_1 := true.B
        num_y_2 := true.B
        num_y_3 := true.B
    }
    .elsewhen (scaler_y === 12.U) {
        when (num_y_0_cond) {
            num_y_0 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        num_y_1 := true.B
        num_y_2 := true.B
        num_y_3 := true.B
    }
    .elsewhen (scaler_y === 13.U) {
        when (num_y_0_cond) {
            num_y_0 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        num_y_1 := true.B
        num_y_2 := true.B
        num_y_3 := true.B
    }
    .elsewhen (scaler_y === 14.U) {
        when (num_y_0_cond) {
            num_y_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        num_y_1 := true.B
        num_y_2 := true.B
        num_y_3 := true.B
    }
    .elsewhen (scaler_y === 15.U) {
        when (num_y_0_cond) {
            num_y_0 := img_0.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        num_y_1 := true.B
        num_y_2 := true.B
        num_y_3 := true.B
    }
    .otherwise {
        when (num_y_0_cond) {
            num_y_0 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            num_y_0 := true.B
        }
        when (num_y_1_cond) {
            num_y_1 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            num_y_1 := true.B
        }
        when (num_y_2_cond) {
            num_y_2 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            num_y_2 := true.B
        }
        when (num_y_3_cond) {
            num_y_3 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
        }
        .otherwise {
            num_y_3 := true.B
        }
    }

    // Number conditions for fft div_y
    val fft_num_y_0_cond = RegInit(false.B)
    fft_num_y_0_cond := (((pixel_x > x_start + img_div_x.w + img_div_y.w + 5*img_0.w) && (pixel_x <= x_start + img_div_x.w + img_div_y.w + 6*img_0.w)) && ((pixel_y > y_end_2 + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end_2 + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val fft_num_y_1_cond = RegInit(false.B)
    fft_num_y_1_cond := (((pixel_x > x_start + img_div_x.w + img_div_y.w + 6*img_0.w) && (pixel_x <= x_start + img_div_x.w + img_div_y.w + 7*img_0.w)) && ((pixel_y > y_end_2 + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end_2 + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val fft_num_y_2_cond = RegInit(false.B)
    fft_num_y_2_cond := (((pixel_x > x_start + img_div_x.w + img_div_y.w + 7*img_0.w) && (pixel_x <= x_start + img_div_x.w + img_div_y.w + 8*img_0.w)) && ((pixel_y > y_end_2 + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end_2 + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    val fft_num_y_3_cond = RegInit(false.B)
    fft_num_y_3_cond := (((pixel_x > x_start + img_div_x.w + img_div_y.w + 8*img_0.w) && (pixel_x <= x_start + img_div_x.w + img_div_y.w + 9*img_0.w)) && ((pixel_y > y_end_2 + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end_2 + (offset_top - img_div_x.h)/2 + img_div_x.h)))

    val fft_num_y_0 = Wire(Bool())
    val fft_num_y_1 = Wire(Bool())
    val fft_num_y_2 = Wire(Bool())
    val fft_num_y_3 = Wire(Bool())

    when(fft_scaler_y === 0.U || fft_scaler_y === 8.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_3.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        when (fft_num_y_1_cond) {
            fft_num_y_1 := img_2.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            fft_num_y_1 := true.B
        }
        fft_num_y_2 := true.B
        fft_num_y_3 := true.B
    }
    .elsewhen (fft_scaler_y === 1.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_6.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        when (fft_num_y_1_cond) {
            fft_num_y_1 := img_4.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            fft_num_y_1 := true.B
        }
        fft_num_y_2 := true.B
        fft_num_y_3 := true.B
    }
    .elsewhen (fft_scaler_y === 2.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_1.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        when (fft_num_y_1_cond) {
            fft_num_y_1 := img_2.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            fft_num_y_1 := true.B
        }
        when (fft_num_y_2_cond) {
            fft_num_y_2 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            fft_num_y_2 := true.B
        }
        fft_num_y_3 := true.B
    }
    .elsewhen (fft_scaler_y === 3.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_2.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        when (fft_num_y_1_cond) {
            fft_num_y_1 := img_5.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            fft_num_y_1 := true.B
        }
        when (fft_num_y_2_cond) {
            fft_num_y_2 := img_6.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            fft_num_y_2 := true.B
        }
        fft_num_y_3 := true.B
    }
    .elsewhen (fft_scaler_y === 4.U) {
         when (fft_num_y_0_cond) {
            fft_num_y_0 := img_5.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        when (fft_num_y_1_cond) {
            fft_num_y_1 := img_1.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            fft_num_y_1 := true.B
        }
        when (fft_num_y_2_cond) {
            fft_num_y_2 := img_2.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            fft_num_y_2 := true.B
        }
        fft_num_y_3 := true.B
    }
    .elsewhen (fft_scaler_y === 5.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_1.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        when (fft_num_y_1_cond) {
            fft_num_y_1 := img_0.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            fft_num_y_1 := true.B
        }
        when (fft_num_y_2_cond) {
            fft_num_y_2 := img_2.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            fft_num_y_2 := true.B
        }
        when (fft_num_y_3_cond) {
            fft_num_y_3 := img_4.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
        }
        .otherwise {
            fft_num_y_3 := true.B
        }
    }
    .elsewhen (fft_scaler_y === 6.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_2.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        when (fft_num_y_1_cond) {
            fft_num_y_1 := img_0.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            fft_num_y_1 := true.B
        }
        when (fft_num_y_2_cond) {
            fft_num_y_2 := img_4.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            fft_num_y_2 := true.B
        }
        when (fft_num_y_3_cond) {
            fft_num_y_3 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
        }
        .otherwise {
            fft_num_y_3 := true.B
        }
    }
    .elsewhen (fft_scaler_y === 7.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_4.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        when (fft_num_y_1_cond) {
            fft_num_y_1 := img_0.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            fft_num_y_1 := true.B
        }
        when (fft_num_y_2_cond) {
            fft_num_y_2 := img_9.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            fft_num_y_2 := true.B
        }
        when (fft_num_y_3_cond) {
            fft_num_y_3 := img_6.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
        }
        .otherwise {
            fft_num_y_3 := true.B
        }
    }
    .elsewhen (fft_scaler_y === 9.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_1.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        when (fft_num_y_1_cond) {
            fft_num_y_1 := img_6.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            fft_num_y_1 := true.B
        }
        fft_num_y_2 := true.B
        fft_num_y_3 := true.B
    }
    .elsewhen (fft_scaler_y === 10.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        fft_num_y_1 := true.B
        fft_num_y_2 := true.B
        fft_num_y_3 := true.B
    }
    .elsewhen (fft_scaler_y === 11.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_4.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        fft_num_y_1 := true.B
        fft_num_y_2 := true.B
        fft_num_y_3 := true.B
    }
    .elsewhen (fft_scaler_y === 12.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_2.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        fft_num_y_1 := true.B
        fft_num_y_2 := true.B
        fft_num_y_3 := true.B
    }
    .elsewhen (fft_scaler_y === 13.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_1.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        fft_num_y_1 := true.B
        fft_num_y_2 := true.B
        fft_num_y_3 := true.B
    }
    .elsewhen (fft_scaler_y === 14.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_0.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        fft_num_y_1 := true.B
        fft_num_y_2 := true.B
        fft_num_y_3 := true.B
    }
    .elsewhen (fft_scaler_y === 15.U) {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_0.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        fft_num_y_1 := true.B
        fft_num_y_2 := true.B
        fft_num_y_3 := true.B
    }
    .otherwise {
        when (fft_num_y_0_cond) {
            fft_num_y_0 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
        }
        .otherwise {
            fft_num_y_0 := true.B
        }
        when (fft_num_y_1_cond) {
            fft_num_y_1 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
        }
        .otherwise {
            fft_num_y_1 := true.B
        }
        when (fft_num_y_2_cond) {
            fft_num_y_2 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
        }
        .otherwise {
            fft_num_y_2 := true.B
        }
        when (fft_num_y_3_cond) {
            fft_num_y_3 := img_8.imageROM(pixel_y - (y_end_2 + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
        }
        .otherwise {
            fft_num_y_3 := true.B
        }
    }

    // Real time signal condition
    val real_cond = RegInit(false.B)
    real_cond := ((pixel_x > x_start_sig) && (pixel_x <= x_end_sig)) && ((pixel_y > y_start + axis_size/2) && (pixel_y <= y_end - axis_size/2)) && 
                 ((inReal === signal_cond) || (inReal === signal_cond_2) || (inReal < signal_cond && inReal_delayed > signal_cond) || (inReal > signal_cond && inReal_delayed < signal_cond))
    // Imag time signal condition
    val imag_cond = RegInit(false.B)
    imag_cond := ((pixel_x > x_start_sig) && (pixel_x <= x_end_sig)) && ((pixel_y > y_start + axis_size/2) && (pixel_y <= y_end - axis_size/2)) && 
                 (((inImag === signal_cond) || (inImag === signal_cond_2) || (inImag < signal_cond && inImag_delayed > signal_cond) || (inImag > signal_cond && inImag_delayed < signal_cond)))
    // Border line condition
    val border_cond = RegInit(false.B)
    border_cond := (((pixel_x > x_start) && (pixel_x <= x_end)) && (((pixel_y > y_start) && (pixel_y <= y_start + axis_size)) || ((pixel_y > y_end - axis_size) && (pixel_y <= y_end)))) ||
                   (((pixel_y > y_start + axis_size) && (pixel_y <= y_end - axis_size)) && (((pixel_x > x_start) && (pixel_x <= x_start + axis_size)) || ((pixel_x > x_end - axis_size) && (pixel_x <= x_end)))) 

    // Grid line condition
    val grid_cond = RegInit(false.B)
    grid_cond := (((pixel_x > x_start + axis_size) && (pixel_x < x_end - axis_size)) && y_cond) || (((pixel_y > y_start + axis_size) && (pixel_y <= y_end - axis_size)) && x_cond)

    // FFT signal condition
    val cut_cond = RegInit(false.B)
    cut_cond := ((pixel_x > x_start_sig) && (pixel_x <= x_end_sig)) && ((pixel_y > y_start_2 + axis_size/2) && (pixel_y <= y_end_2 - axis_size/2)) && (inCUT >= signal_fft_cond)
    // Treshold condition
    val tresh_cond = RegInit(false.B)
    tresh_cond := ((pixel_x > x_start_sig) && (pixel_x <= x_end_sig)) && ((pixel_y > y_start_2 + axis_size/2) && (pixel_y <= y_end_2 - axis_size/2)) && 
                 ((inTreshold === signal_fft_cond) || (inTreshold === signal_fft_cond_1) || (inTreshold === signal_fft_cond_2) || (inTreshold < signal_fft_cond && inTreshold_delayed > signal_fft_cond) || (inTreshold > signal_fft_cond && inTreshold_delayed < signal_fft_cond))
    // Peak condition
    val peak_cond = RegInit(false.B)
    peak_cond := ((pixel_x > x_start) && (pixel_x <= x_end)) && ((pixel_y > y_end_2) && (pixel_y <= y_end_2 + 3*axis_size)) && (inPeak.asUInt >= 1.U)
    // FFT border line condition
    val fft_border_cond = RegInit(false.B)
    fft_border_cond := (((pixel_x > x_start) && (pixel_x <= x_end)) && (((pixel_y > y_start_2) && (pixel_y <= y_start_2 + axis_size)) || ((pixel_y > y_end_2 - axis_size) && (pixel_y <= y_end_2)))) ||
                       (((pixel_y > y_start_2 + axis_size) && (pixel_y <= y_end_2 - axis_size)) && (((pixel_x > x_start) && (pixel_x <= x_start + axis_size)) || ((pixel_x > x_end - axis_size) && (pixel_x <= x_end))))

    // FFT grid line condition
    val fft_grid_cond = RegInit(false.B)
    fft_grid_cond := (((pixel_x > x_start + axis_size) && (pixel_x < x_end - axis_size)) && y_cond_2) || (((pixel_y > y_start_2 + axis_size) && (pixel_y <= y_end_2 - axis_size)) && x_cond)

    // Draw
    when (video_active) {
    // FrameBuffer
        // Draw real signal
        when (real_cond){
            rgb := green_px // send green pixels
        }
        // Draw imag and FFT signal
        .elsewhen (imag_cond){
            rgb := yellow_px // send yellow pixels
        }
        // Draw boarder lines
        .elsewhen (border_cond) {
            rgb := white_px // send white pixels
        }
        // Draw grid lines
        .elsewhen (grid_cond) {
            rgb := grey_px // send grey pixels
        }
    // Spectrometer
        // Draw Treshold signal
        .elsewhen (tresh_cond){
            rgb := yellow_px // send yellow pixels
        }
        // Draw FFT signal
        .elsewhen (cut_cond){
            // when(inPeak.asUInt >= 1.U) {
            //     rgb := red_px // send red pixels
            // }
            when(inCUT >= inTreshold) {
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
        .elsewhen (fft_border_cond) {
            rgb := white_px // send white pixels
        }
        // Draw grid lines
        .elsewhen (fft_grid_cond) {
            rgb := grey_px // send grey pixels
        }
        // Draw NovelIC logo
        .elsewhen (logo_cond) {
            when (logo_img) {
                rgb := white_px // send white_px pixels
            }
            .otherwise {
                 rgb := red_px // send red_px pixels
            }
        }
        // Draw div_x and div_y
        .elsewhen ((div_x_cond && (div_x_img === false.B)) || (div_y_cond && (div_y_img === false.B)) || 
                   (div_x_cond_2 && (div_x_img === false.B)) || (div_y_cond_2 && (div_y_img === false.B))) {
            rgb := white_px // send white_px pixels
        }
        // Draw numbers
        .elsewhen ((num_x_0_cond && (num_x_0 === false.B)) || (num_x_1_cond && (num_x_1 === false.B)) || (num_x_2_cond && (num_x_2 === false.B)) || (num_x_3_cond && (num_x_3 === false.B)) ||
                   (fft_num_x_0_cond && (fft_num_x_0 === false.B)) || (fft_num_x_1_cond && (fft_num_x_1 === false.B)) || (fft_num_x_2_cond && (fft_num_x_2 === false.B)) || (fft_num_x_3_cond && (fft_num_x_3 === false.B))) {
            rgb := white_px // send white_px pixels
        }
        // Draw numbers
        .elsewhen ((num_y_0_cond && (num_y_0 === false.B)) || (num_y_1_cond && (num_y_1 === false.B)) || (num_y_2_cond && (num_y_2 === false.B)) || (num_y_3_cond && (num_y_3 === false.B)) ||
                   (fft_num_y_0_cond && (fft_num_y_0 === false.B)) || (fft_num_y_1_cond && (fft_num_y_1 === false.B)) || (fft_num_y_2_cond && (fft_num_y_2 === false.B)) || (fft_num_y_3_cond && (fft_num_y_3 === false.B))) {
            rgb := white_px // send white_px pixels
        }
        // Otherwise send black pixels
        .otherwise {
            rgb := black_px // send black pixels
        }
    }
    .otherwise {
        rgb := black_px // send black pixels
    }
    
    // Generate start and triggered signals
    when (video_active) {
        // Start
        when((pixel_y === 0.U) && (pixel_x < x_start)) {
            io.start     := true.B
            scaler_x     := io.scaler_x
            scaler_y     := io.scaler_y
            fft_scaler_x := io.fft_scaler_x
            fft_scaler_y := io.fft_scaler_y
        }
        .otherwise {
            io.start := false.B
        }
        // Trigger
        when(pixel_y === (y_start/2).U) {
            io.triggered := true.B
        }
        .otherwise {
            io.triggered := false.B
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
        io.triggered  := false.B
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
        div_size_y    = 64,
        div_y         = 8,
        div_size_y_2  = 32,
        div_y_2       = 12,
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
        div_size_y    = 64,
        div_y         = 8,
        div_size_y_2  = 32,
        div_y_2       = 12,
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
    div_size_y    = 64,
    div_y         = 8,
    div_size_y_2  = 32,
    div_y_2       = 12,
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