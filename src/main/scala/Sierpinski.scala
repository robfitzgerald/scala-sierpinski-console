import scala.annotation.tailrec

object Sierpinski extends App {

	case class Point(x: Int ,y: Int, mark: String)
	val UNSET = ""
  
	def drawAtDepth(n: Int, height: Int) {
		if (n < 0) throw new ArithmeticException()
		if (height < 0) throw new ArithmeticException()
		val yBounds = height
		val xBounds = (height * 2) - 1	

		val trianglePoints = drawTriangles(0, n, Point(0,0,""), Point(xBounds,yBounds,"")).distinct.toSeq
		val background = drawBackground(xBounds,yBounds).diff(trianglePoints).map(p=>Point(p.x,p.y,"_"))
		val triangles = trianglePoints.map(p=>Point(p.x,p.y,"1"))
		val trianglesOnBackground = triangles.union(background).sortBy(p=>(0 - p.y,p.x))
		val image = trianglesOnBackground.map(paint=>paint.mark).grouped(xBounds)
		image.foreach(line=>println(line.mkString))
	}

	def drawBackground(xBounds: Int, yBounds: Int): Seq[Point] = {
		(for (x <- 0 until xBounds; y <- 0 until yBounds) yield Point(x,y,"")).toSeq
	}

	/* 
	 * draws a triangle from origin to terminal, not-inclusive
	 */

	def drawTriangles(depth: Int, maxDepth: Int, origin: Point, terminal: Point): Seq[Point] = {
  	if (depth == maxDepth) {
			val xRelativeHalfwayPoint = ((terminal.x.toFloat - origin.x.toFloat) / 2.0).ceil.toInt
			val yRelativeHeight = terminal.y - origin.y
			(
				(for (x <- 0 until xRelativeHalfwayPoint; y <- 0 until yRelativeHeight if (y <= x)) yield 
					Point(x+origin.x, y + origin.y, "")) ++
				(for (x <- 0 until (xRelativeHalfwayPoint - 1); y <- 0 until yRelativeHeight if (y < ((yRelativeHeight - 1) - x))) yield 
					Point(x + origin.x + xRelativeHalfwayPoint, y + origin.y, "")))
		} else {
			((drawTriangles(
				depth + 1, 
				maxDepth, 
				origin, 
				bottomLeftEndPoint(origin, terminal))) ++
			(drawTriangles(
				depth + 1,
				maxDepth,
				bottomRightStartPoint(origin, terminal),
				bottomRightEndPoint(origin, terminal))) ++
			(drawTriangles(
				depth + 1,
				maxDepth, 
				topStartPoint(origin, terminal),
				topEndPoint(origin, terminal))))
		}	
	}	

	def bottomLeftEndPoint(origin: Point, terminal: Point): Point = {
		new Point(
				(origin.x + ((terminal.x.toFloat - origin.x.toFloat) / 2.0).ceil.toInt),
				(origin.y + ((terminal.y.toFloat - origin.y.toFloat) / 2.0).ceil.toInt),
				UNSET
			)
	}

	def bottomRightStartPoint(origin: Point, terminal: Point): Point = {
		new Point(
				(origin.x + ((terminal.x.toFloat - origin.x.toFloat) / 2.0).ceil.toInt),
				(origin.y),
				UNSET
			)
	}

	def bottomRightEndPoint(origin: Point, terminal: Point): Point = {
		new Point(
				(terminal.x),
				(origin.y + ((terminal.y.toFloat - origin.y.toFloat) / 2.0).ceil.toInt),
				UNSET
			)
	}

	def topStartPoint(origin: Point, terminal: Point): Point = {
		new Point(
				(origin.x + ((terminal.x.toFloat - origin.x.toFloat) / 4.0).ceil.toInt),
				(origin.y + ((terminal.y.toFloat - origin.y.toFloat) / 2.0).ceil.toInt),
				UNSET
			)
	}

	def topEndPoint(origin: Point, terminal: Point): Point = {
		new Point(
				(origin.x + (((terminal.x.toFloat - origin.x.toFloat) / 4.0).ceil.toInt) * 3),
				(terminal.y),
				UNSET
			)
	}

	try {
		args.length match {
			case 0 => drawAtDepth(1, 32)
			case 1 => drawAtDepth(args(0).toInt, 32)
			case 2 => drawAtDepth(args(0).toInt, args(1).toInt)
		}
    // drawAtDepth(readInt(), 32)
	} catch {
	  case e: Exception => println("usage: run <depth> <height> for e^<depth> triangles printed to <height> lines on the console.");
	}
}