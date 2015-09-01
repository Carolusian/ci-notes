import Jama._
import java.util.Random

object NMF {
	def main(args:Array[String]) {
		val wa = Array(Array(1.,2.,3.),Array(4.,5.,6.))
		val wb = Array(Array(2.,4.),Array(2.,5.),Array(4.,7.))
		val data = (new Matrix(wa)).times(new Matrix(wb))
		val result = factorize(data)
		display(result._1.getArray)
		println()
		display(result._2.getArray)
	}
	
	implicit def array2SuperArray(arg:Array[Array[Double]]) = new SuperArray(arg)
	
	def factorize(data:Matrix,
				fc:Int=2, 
				maxItr:Int = 500):(Matrix,Matrix) = {
		val rc = data.getRowDimension
		val cc = data.getColumnDimension
		
		var w = randomMatrix(rc,fc)
		var h = randomMatrix(fc,cc)
		
		for ( i <- 0 until maxItr ) {
			print( "iteration:"+i+"  ")
			println(distance(w.times(h).getArray,data.getArray))
			if(distance(w.times(h).getArray,data.getArray)<0.001)
				return (w,h)
			val hn = w.transpose.times(data)
			val hd = w.transpose.times(w).times(h)
			//[a(0)(0)*b(0)(0),a(0)(1)*b(0)(1)....] , [a(0)(0)/b(0)(0),a(0)(1)/b(0)(1)....]
			h =  new Matrix(h.getArray * (hn.getArray / hd.getArray));
			
			val wn = (data).times(h.transpose)
			val wd = w.times(h).times(h.transpose)
			w = new Matrix(w.getArray * (wn.getArray / wd.getArray));
		}
		(w,h)
	}
	
	def distance(wh:Array[Array[Double]],original:Array[Array[Double]]) = {
		var distance = 0.
		for ( i <- 0 until wh.length ) {
			for ( j <- 0 until wh(i).length) {
				distance += Math.pow(original(i)(j)-wh(i)(j),2)
			}
		}
		Math.sqrt(distance)
	}
	
	def randomMatrix(rowCount:Int, colCount:Int) = {
		val rand:Random = new Random(System.currentTimeMillis())
		val matrix = for ( i <- 0 until rowCount ) yield {
			val tmp = for ( j <-0 until colCount) yield {
				rand.nextDouble()
			}
			tmp.toArray
		}
		new Matrix(matrix.toArray)
	}
	
	def display(arg:Array[Array[Double]]) {
		arg.foreach {
			x=> {
				x.foreach(y=>print(y+" "))
				println()
			}
		}
	}
}

class SuperArray( val arr: Array[Array[Double]] ) {
	def *(that:Array[Array[Double]]) = {
		val result = for ( i <- 0 until arr.length ) yield {
			val tmp = for ( j <- 0 until arr(0).length) yield {
				arr(i)(j) * that(i)(j)
			}
			tmp.toArray
		}
		result.toArray
	}
	
	def / (that:Array[Array[Double]]) = {
		val result = for ( i <- 0 until arr.length ) yield {
			val tmp = for ( j <- 0 until arr(0).length) yield {
				arr(i)(j) / that(i)(j)
			}
			tmp.toArray
		}
		result.toArray
	}
}
