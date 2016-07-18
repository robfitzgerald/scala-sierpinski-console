object Solution {

	case class Point(x: Int ,y: Int, mark: String)
  
	def drawAtDepth(n: Int) {
		// original problem bounds
		val yBounds = 32
		val xBounds = 63
		
		// larger result
		// val yBounds = 256
		// val xBounds = 511	

		val trianglePoints = drawTriangles(0, n, Point(0,0,""), Point(xBounds,yBounds,""))
		val background = drawBackground(xBounds,yBounds).diff(trianglePoints).map(p=>Point(p.x,p.y,"_"))
		val triangles = trianglePoints.map(p=>Point(p.x,p.y,"1"))
		val trianglesOnBackground = triangles.union(background).sortBy(p=>(0 - p.y,p.x))
		// println("should have " + yBounds*xBounds + " elements: " + trianglesOnBackground.length)
		val image = trianglesOnBackground.map(paint=>paint.mark).grouped(xBounds)
		image.foreach(line=>println(line.mkString))
	}

	def drawBackground(xBounds: Int, yBounds: Int): Seq[Point] = {
		(for (x <- 0 until xBounds; y <- 0 until yBounds) yield Point(x,y,"")).toSeq
	}

	/* 
	 * draws a triangle from origin to terminalCoordinate not-inclusive
	 */
	def drawTriangles(depth: Int, maxDepth: Int, origin: Point, terminalCoordinate: Point): Seq[Point] = {
  	if (depth == maxDepth) {
			val xRelativeHalfwayPoint = ((terminalCoordinate.x.toFloat - origin.x.toFloat) / 2.0).ceil.toInt
			val yRelativeHeight = terminalCoordinate.y - origin.y
			// println("dimensions: " + origin.toString + " , " + terminalCoordinate.toString)
			// println("xRelativeHalfwayPoint: " + xRelativeHalfwayPoint + ", yRelativeHeight: " + yRelativeHeight)
			val leftCurve = for (x <- 0 until xRelativeHalfwayPoint; y <- 0 until yRelativeHeight if (y <= x)) yield Point(x+origin.x, y + origin.y, "")
			val rightCurve = for (x <- 0 until (xRelativeHalfwayPoint - 1); y <- 0 until yRelativeHeight if (y < ((yRelativeHeight - 1) - x))) yield Point(x + origin.x + xRelativeHalfwayPoint, y + origin.y, "")
			(leftCurve ++ rightCurve).distinct.toSeq
		} else {
			val halfwayYIncrement = ((terminalCoordinate.y.toFloat - origin.y.toFloat) / 2.0).ceil.toInt
			val quarterwayXPosition = ((terminalCoordinate.x.toFloat - origin.x.toFloat) / 4.0).ceil.toInt
			val bottomLeftStartPoint = origin
			val bottomLeftEndPoint = new Point((origin.x + (quarterwayXPosition * 2)), (origin.y + halfwayYIncrement), "")
			val bottomRightStartPoint = new Point((origin.x + (quarterwayXPosition * 2)), (origin.y), "")
			val bottomRightEndPoint = new Point((terminalCoordinate.x), (origin.y + halfwayYIncrement), "")
			val topStartPoint = new Point((origin.x + quarterwayXPosition), (origin.y + halfwayYIncrement), "")
			val topEndPoint = new Point((origin.x + (quarterwayXPosition * 3)), (terminalCoordinate.y), "")
			// println("new bottom left: " + bottomLeftStartPoint.toString + ", " + bottomLeftEndPoint.toString)
			// println("new bottom right: " + bottomRightStartPoint.toString + ", " + bottomRightEndPoint.toString)
			// println("new top: " + topStartPoint.toString + ", " + topEndPoint.toString)
			val bottomLeft = drawTriangles(
				depth + 1, 
				maxDepth, 
				bottomLeftStartPoint, 
				bottomLeftEndPoint)
			val bottomRight = drawTriangles(
				depth + 1,
				maxDepth,
				bottomRightStartPoint,
				bottomRightEndPoint)
			val top = drawTriangles(
				depth + 1,
				maxDepth, 
				topStartPoint,
				topEndPoint)
			(bottomLeft ++ bottomRight ++ top).distinct.toSeq
		}	
	}	

  def main(args: Array[String]) {
		try{
	    drawAtDepth(readInt())
		} catch {
		  case e: Exception => println("usage: run this application and then enter a number greater than 0, followed by the enter key." + e);
		}
  }
}