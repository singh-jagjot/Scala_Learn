class Polynom(nonZeroTerms: Map[Int, Double]):
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  def terms = nonZeroTerms.withDefaultValue(0.0)

//   def + (other: Polynom): Polynom = Polynom(
//     terms ++ other.terms.map((exp, coeff) => (exp, terms(exp) + coeff))
//   )

  def + (other: Polynom): Polynom = Polynom(other.terms.foldLeft(terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] =
    val (exp, coeff) = term
    terms + (exp -> (terms(exp) + coeff))

  override def toString(): String = 
    val termStrings = 
        for (exp, coeff) <- terms.toList.sorted.reverse
        yield
            val exponent = if exp == 0 then "" else s"x^$exp"
            s"$coeff$exponent"
    if terms.isEmpty then "0"
    else termStrings.mkString(" + ")


val x = Polynom(0 -> 2, 1 -> -3, 2 -> 1)
val z = Polynom()
x + x + z