// SPDX-License-Identifier: Apache-2.0

package hdmi.preproc

import breeze.math.Complex
import breeze.signal.fourierTr
import breeze.linalg._
import breeze.plot._

import chisel3.util.log2Up

object Utils {
  // Scaler 2 RGB function
  def scaler2rgb(in: Int, width: Int): Int = {
    val maxValue = 1 << width
    val scaler = if (width >= 8) {width - 8}
    else {8 - width}

    var a = (maxValue - 1 - in) << 2
    var X = a
    var Y: Int = 0
    if (X < 1*maxValue) {
      Y = if (width >= 8) {a >> scaler}
      else {a << scaler}
      
      (255 << 16) + (Y << 8) + 0
    }
    else if (X < 2*maxValue) {
      Y = if (width >= 8) {(a-1*maxValue) >> scaler}
      else {(a-1*maxValue) << scaler}
      
      ((255-Y) << 16) + (255 << 8) + 0
    }
    else if (X < 3*maxValue) {
      Y = if (width >= 8) {(a-2*maxValue) >> scaler}
      else {(a-2*maxValue) << scaler}

      (0 << 16) + (255 << 8) + Y
    }
    else if (X < 4*maxValue) {
      Y = if (width >= 8) {(a-3*maxValue) >> scaler}
      else {(a-3*maxValue) << scaler}

      (0 << 16) + ((255-Y) << 8) + 255
    }
    else if (X == 4*maxValue) 255
    else (0 << 16) + (0 << 8) + 255
  }

  // Signed integer to unsigned
  def toUint(c: Int, beatBytes: Int): BigInt = {
    var out: BigInt = if (c < 0) (BigInt(c) + BigInt(1L << (8*beatBytes))) & BigInt((1L << (8*beatBytes))-1L) else BigInt(c)
    out
  }

  // Scaler model
  def scalerSeq(c: Seq[Int], scaler: Int, beatBytes: Int) : Seq[Int] = {
    var out = Seq[Int]()
    for (i <- 0 until c.size) {
      if (scaler < 8) {
        out = out :+ (c(i) >>> scaler)
      }
      else {
        if (c(i) >= 0) {
          if (BigInt(c(i)) * BigInt(1 << (scaler & 0x7)) > BigInt((1 << (8*beatBytes - 1)) - 1)) {
            out = out :+ BigInt((1 << (8*beatBytes - 1)) - 1).toInt
          }
          else {
            out = out :+ ((BigInt(c(i)) * BigInt(1 << (scaler & 0x7))) & BigInt((1L << (8*beatBytes))-1L)).toInt
          }
        }
        else {
          if (BigInt(c(i)) * BigInt(1 << (scaler & 0x7)) < BigInt(-1 << (8*beatBytes - 1))) {
            out = out :+ BigInt(1 << (8*beatBytes - 1)).toInt
          }
          else {
            out = out :+ ((BigInt(c(i)) * BigInt(1 << (scaler & 0x7))) & BigInt((1L << (8*beatBytes))-1L)).toInt
          }
        }
        
      }
    }
    out
  }

  // Decimator model
  def decimatorSeq(c: Seq[Int], skipSize: Int) : Seq[Int] = {
    var out = Seq[Int]()
    for (i <- 0 until c.size) {
      if (skipSize == 0) {
        out = out :+ c(i)
      }
      else {
        if (i % skipSize == 0) {
          out = out :+ c(i)
        }
      }
    }
    out
  }

  // Zero-Order Hold model
  def zohSeq(c: Seq[Double], zeroOrderHold: Int) : Seq[Double] = {
    var out = Seq[Double]()
    for (i <- 0 until c.size) {
      for (j <- 0 until zeroOrderHold) {
        out = out :+ c(i)
      }
    }
    out
  }

  // interpolation model
  def interpolatedSeq(c: Seq[Int], zeroOrderHold: Int): Seq[Int] = {
    var u = Seq[Int]()
    for (i <- 0 until c.size){
      for (j <- 0 until zeroOrderHold) {
        if (i == 0) { 
          u = u :+ c(0)
        }
        else {
          u = u :+ (c(i)-c(i-1))
        }
      }
    }
    var y = Seq[Int]()
    for (i <- 0 until u.size) {
      if (i == 0) { 
        y = y :+ u(0)
      }
      else { 
        y = y :+ (u(i)+y(i-1))
      }
    }
    var out = Seq[Int]()
    for (i <- 0 until y.size) {
      out = out :+ y(i)/zeroOrderHold
    }
    out
  }
    
  /**
  * Convert int data to binary string
  */
  def asNdigitBinary (source: Int, digits: Int): String = {
    val lstring = source.toBinaryString
    //val sign = if (source > 0) "%0" else "%1"
    if (source >= 0) {
      //val l: java.lang.Long = lstring.toLong
      val l: java.lang.Long = lstring.toLong
      String.format ("%0" + digits + "d", l)
    }
    else
      lstring.takeRight(digits)
  }
  
  
  /**
  * Generate real sinusoid. Assumption is that dataWidth of the fft input is always equal to 16
  * Scale parameter is useful when square magnitude is calculated to prevent overflow
  */

  def getTone(numSamples: Int, f1r: Double, scale: Int = 1): Seq[Int] = {
    (0 until numSamples).map(i => (math.sin(2 * math.Pi * f1r * i) * scala.math.pow(2, 14)/scale).toInt)
  }
  
  /**
  * Generate complex sinusoid. Assumption is that dataWidth of the fft input is always equal to 16
  * Scale parameter is useful when square magnitude is calculated to prevent overflow
  */
  
  def getComplexTone(numSamples: Int, f1r: Double, scale: Int = 1): Seq[Complex] = {
    (0 until numSamples).map(i => Complex((math.cos(2 * math.Pi * f1r * i) * scala.math.pow(2, 13)/scale).toInt, (math.sin(2 * math.Pi * f1r * i)*scala.math.pow(2, 13)/scale).toInt))
  }
  
  /**
  * Generate random signal. Assumption is that dataWidth of the fft input is always equal to 16
  * Scale parameter is useful when square magnitude is calculated to prevent overflow
  */
  def genRandSignal(numSamples: Int, scale: Int = 1, binPoint: Int = 14): Seq[Int] = {
    import scala.util.Random
    
    Random.setSeed(11110L)
    (0 until numSamples).map(x => (Random.nextDouble()*scala.math.pow(2, binPoint)/scale).toInt)
  }
  
  /**
  * Generate complex random signal. Assumption is that dataWidth of the fft input is always equal to 16
  * Scale parameter is useful when square magnitude is calculated to prevent overflow
  */
  def genComplexRandSignal(numSamples: Int, scale: Int = 1, binPoint: Int = 13): Seq[Complex] = {
    import scala.util.Random
    
    Random.setSeed(11110L)
    (0 until numSamples).map(x => Complex((Random.nextDouble()*scala.math.pow(2, binPoint)/scale).toInt, Random.nextDouble()*scala.math.pow(2, binPoint)/scale.toInt))
  }
  
  /**
  * Format inData so that it is compatible with 32 AXI4Stream data
  */
  def formAXI4StreamRealData(inData: Seq[Int], dataWidth: Int): Seq[Int] = {
    inData.map(data => java.lang.Long.parseLong(
                                  asNdigitBinary(data, dataWidth) ++ 
                                  asNdigitBinary(0, dataWidth), 2).toInt)
  }
  
  /**
  * Format complex inData so that it is compatible with 32 AXI4Stream data
  */
  def formAXI4StreamComplexData(inData : Seq[Complex], dataWidth: Int): Seq[Int] = {
    inData.map(data => java.lang.Long.parseLong(
                                  asNdigitBinary(data.real.toInt, dataWidth) ++ 
                                  asNdigitBinary(data.imag.toInt, dataWidth)).toInt)
  }
  
  
  /*****************************************************
  * Does bit reversal of the input data
  */
 
  def bit_reverse(in: Int, width: Int): Int = {
    import scala.math.pow
    var test = in
    var out = 0
    for (i <- 0 until width) {
      if (test / pow(2, width-i-1) >= 1) {
        out += pow(2,i).toInt
        test -= pow(2,width-i-1).toInt
      }
    }
    out
  }
  
  def bitrevorder_data(testSignal: Seq[Int]): Seq[Int] = {
    val seqLength = testSignal.size
    val new_indices = (0 until seqLength).map(x => bit_reverse(x, log2Up(seqLength)))
    new_indices.map(x => testSignal(x))
  }

 /*******************************************************/
  
  /**
  * Calculate log2
  */
  def log2(x: Double): Double =  scala.math.log(x)/scala.math.log(2)
  
  /**
  * Calculate jpl magnitude of the complex input data
  */
  def jplMag(in: Complex): Int = {
    import scala.math._
    
    val u = abs(in.real).max(abs(in.imag))
    val v = abs(in.real).min(abs(in.imag))
    val jpl = (u + v/8).max(7 * u/8 + v/2).toInt
    jpl
  }

  /****************************************************************
  * Plot functions
  */
  def plot_data(inputData: Seq[Int], plotName: String, fileName: String): Unit = {

    val f = Figure()
    val p = f.subplot(0)
    p.legend_=(true)
  
    val data = inputData.map(e => e.toDouble).toSeq
    val xaxis = (0 until data.length).map(e => e.toDouble).toSeq.toArray
    
    p += plot(xaxis, data.toArray, name = plotName)
   
    p.ylim(data.min, data.max)
    p.title_=(plotName + s" ${inputData.length}")

    p.xlabel = "Time Bins"
    p.ylabel = "Amplitude"
    f.saveas(s"test_run_dir/" + fileName)
  }
  
  def plot_fft(inputData: Seq[Long], plotName: String, fileName: String): Unit = {

    val f = Figure()
    val p = f.subplot(0)
    p.legend_=(true)
    
    val data = inputData.map(e => e.toDouble).toSeq
    val xaxis = (0 until data.length).map(e => e.toDouble).toSeq.toArray
    
    p += plot(xaxis, data.toArray, name = plotName)
   
    p.ylim(data.min, data.max)
    p.title_=(plotName + s" ${inputData.length}")

    p.xlabel = "Frequency Bin"
    p.ylabel = "Amplitude"
    f.saveas(s"test_run_dir/" + fileName)
  }
  /****************************************************************/
  
  /**
  * Calculate expected sine and cosine at the output of the nco module
  */
  def calcExpectedNcoOut(fftSize: Int, binWithPeak: Int): Seq[Complex] = {
    require(binWithPeak < fftSize, "Index of expected peak can not be larger than fft size")
    
    val genSinGolden = (1 until (fftSize + 1)).map(i => (math.sin(2 * math.Pi * binWithPeak/fftSize * i) * scala.math.pow(2, 14)).toInt)
    val genCosGolden = (1 until (fftSize + 1)).map(i => (math.cos(2 * math.Pi * binWithPeak/fftSize * i) * scala.math.pow(2, 14)).toInt)
    val genComplex = genSinGolden.zip(genCosGolden).map { case (sin, cos) => Complex(cos, sin) }
    genComplex
  }
  
  /**
  * Calculate expected output of the fft module
  */
  def calcExpectedFFTOut(fftSize: Int, binWithPeak: Int, scale: Int = 1): Seq[Complex] = {
    require(binWithPeak < fftSize, "Index of expected peak can not be larger than fft size")

    val ncoOut = calcExpectedNcoOut(fftSize, binWithPeak)
    val fftOut = fourierTr(DenseVector(ncoOut.map( c => Complex(c.real.toDouble/scale, c.imag.toDouble/scale)).toArray)).toScalaVector
    fftOut
  }
  
  /**
  * Calculate jpl magnitude, square magnitude or log2 magnitude
  */
  def calcExpectedMagOut(fftSize: Int, binWithPeak: Int, scale: Int = 1, magType: String = "jplMag"): Seq[Int] = {
    require(binWithPeak < fftSize, "Index of expected peak can not be larger than fftSize")
    val fft = calcExpectedFFTOut(fftSize, binWithPeak, scale)
    val mag = magType match {
      case "jplMag" => {
        val jpl = fft.map(c => jplMag(c))
        jpl
      }
      case "sqrMag" => {
        val magSqr = fft.map(c => (c.real * c.real + c.imag * c.imag).toInt)
        magSqr
      }
      case "log2Mag" => {
        val log2Mag = fft.map(c => log2(jplMag(c).toDouble).toInt)
        log2Mag
      }
      case _ => fft.map(c => c.abs.toInt).toSeq
    }
    mag
  }
  
  /**
  * Check FFT error
  */
  def checkFFTError(expected: Seq[Complex], received: Seq[Complex], tolerance: Int = 2) {
    expected.zip(received).foreach {
      case (in, out) => 
        require(math.abs(in.real - out.real) <= tolerance & math.abs(in.imag - out.imag) <= tolerance, "Tolerance is not satisfied")
    }
  }
  
  /**
  * Check magnitude error, nco error, used also for checking output of the accumulator
  */
  def checkDataError(expected: Seq[Int], received: Seq[Int], tolerance: Int = 2) {
    expected.zip(received).foreach {
      case (in, out) => {
        print("\n\nexpected: ")
        print(in)
        print("\nreceived: ")
        print(out)
        require(math.abs(in - out) <= tolerance, "Tolerance is not satisfied")
      }
    }
  }
}
