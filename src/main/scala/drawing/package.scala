package object drawing {
  // the positions at which the given 'colour' should be drawn
  type ColourLayer = (Seq[Coordinates], Char)

  val emptyLayer: ColourLayer = (Nil, ' ')
}
