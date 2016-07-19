object Sierpinski extends App {

	/**
	 * represents a point on the 2 dimensional coordinate plane
	 * @type {Point}
	 * @param {Int}      x:      x-coordinate
	 * @param {Int}      y:      y-coordinate
	 * @param {String}   mark:   image to use when printing this point to the screen. defaults to an unset value.
	 */
	case class Point(x: Int ,y: Int, mark: String = "")
  
	/**
	 * head of the recursive tree that builds triangles, combines them with the background, and prints it to the console.
	 * @param  {Int} 		depth:      depth to recurse
	 * @param  {Int}    height: height of output, which also calculates the width.
	 */
	def drawAtDepth(depth: Int, height: Int) {
		if (depth < 0 || height < 0) throw new ArithmeticException()
		val yBounds = height
		val xBounds = (height * 2) - 1	

		val trianglePoints = drawTriangles(0, depth, Point(0,0), Point(xBounds,yBounds)).distinct.toSeq
		val backgroundPointsMarked = drawBackground(xBounds,yBounds).diff(trianglePoints).map(p=>Point(p.x,p.y,"_"))
		val trianglesMarked = trianglePoints.map(p=>Point(p.x,p.y,"1"))
		val trianglesOnBackground = trianglesMarked.union(backgroundPointsMarked).sortBy(p=>(0 - p.y,p.x))
		val image = trianglesOnBackground.map(paint=>paint.mark).grouped(xBounds)
		image.foreach(line=>println(line.mkString))
	}

	/**
	 * creates point objects to cover the 2-dimensional coordinate plane on integer steps
	 * @param   {Int}   xBounds:     width of plane, from 0 to xBounds
	 * @param   {Int}   yBounds:     height of plane, from 0 to yBounds
	 * @return  {Seq[Point]}         all values that can exist in this image
	 */
	def drawBackground(xBounds: Int, yBounds: Int): Seq[Point] = {
		(for (x <- 0 until xBounds; y <- 0 until yBounds) yield Point(x,y)).toSeq
	}

	/**
	 * draws a triangle from origin to terminal, not-inclusive
	 * @param {Int}	    depth:      current recursion depth
	 * @param {Int}     maxDepth:   point at which to stop recursing and begin creating triangle points
	 * @param {Point}   origin:     coordinate value for the bottom-left corner of this triangle section
	 * @param {Point}   terminal:   coordinate that represents the x and y bounds of this triangle section
	 * @return {Seq[Point]}         all points on triangles related to the triangle between origin and terminal
	 */
	def drawTriangles(depth: Int, maxDepth: Int, origin: Point, terminal: Point): Seq[Point] = {
  	if (depth == maxDepth) {
			val halfwayX = ((terminal.x.toFloat - origin.x.toFloat) / 2.0).ceil.toInt
			val height = terminal.y - origin.y
			(
				(for (x <- 0 until halfwayX; y <- 0 until height if (y <= x)) yield 
					Point(x+origin.x, y + origin.y)) ++
				(for (x <- 0 until (halfwayX - 1); y <- 0 until height if (y < ((height - 1) - x))) yield 
					Point(x + origin.x + halfwayX, y + origin.y)))
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

	/**
	 * calcuates a terminal point for the bottom-left sub-triangle
	 * @param {Point}   origin:    first coordinate of the sub-section
	 * @param {Point}   terminal:  x and y bounds of the sub-section
	 * @return {Point} terminal point of the sub-section
	 */
	def bottomLeftEndPoint(origin: Point, terminal: Point): Point = {
		new Point(
				(origin.x + ((terminal.x.toFloat - origin.x.toFloat) / 2.0).ceil.toInt),
				(origin.y + ((terminal.y.toFloat - origin.y.toFloat) / 2.0).ceil.toInt)
			)
	}

	/**
	 * calcuates an origin point for the bottom-right sub-triangle
	 * @param {Point}   origin:    first coordinate of the sub-section
	 * @param {Point}   terminal:  x and y bounds of the sub-section
	 * @return {Point} origin point of the sub-section
	 */
	def bottomRightStartPoint(origin: Point, terminal: Point): Point = {
		new Point(
				(origin.x + ((terminal.x.toFloat - origin.x.toFloat) / 2.0).ceil.toInt),
				(origin.y)
			)
	}

	/**
	 * calcuates a terminal point for the bottom-right sub-triangle
	 * @param {Point}   origin:    first coordinate of the sub-section
	 * @param {Point}   terminal:  x and y bounds of the sub-section
	 * @return {Point} terminal point of the sub-section
	 */
	def bottomRightEndPoint(origin: Point, terminal: Point): Point = {
		new Point(
				(terminal.x),
				(origin.y + ((terminal.y.toFloat - origin.y.toFloat) / 2.0).ceil.toInt)
			)
	}

	/**
	 * calcuates an origin point for the top sub-triangle
	 * @param {Point}   origin:    first coordinate of the sub-section
	 * @param {Point}   terminal:  x and y bounds of the sub-section
	 * @return {Point} origin point of the sub-section
	 */
	def topStartPoint(origin: Point, terminal: Point): Point = {
		new Point(
				(origin.x + ((terminal.x.toFloat - origin.x.toFloat) / 4.0).ceil.toInt),
				(origin.y + ((terminal.y.toFloat - origin.y.toFloat) / 2.0).ceil.toInt)
			)
	}

	/**
	 * calcuates a terminal point for the top sub-triangle
	 * @param {Point}   origin:    first coordinate of the sub-section
	 * @param {Point}   terminal:  x and y bounds of the sub-section
	 * @return {Point} terminal point of the sub-section
	 */
	def topEndPoint(origin: Point, terminal: Point): Point = {
		new Point(
				(origin.x + (((terminal.x.toFloat - origin.x.toFloat) / 4.0).ceil.toInt) * 3),
				(terminal.y)
			)
	}

	/**
	 * main program
	 */
	try {
		args.length match {
			case 0 => drawAtDepth(1, 32)
			case 1 => drawAtDepth(args(0).toInt, 32)
			case 2 => drawAtDepth(args(0).toInt, args(1).toInt)
		}
	} catch {
	  case e: Exception => println("usage: run <depth> <height> for e^<depth> triangles printed to <height> lines on the console.   (" + e + ")");
	}
}