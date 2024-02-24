trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

def eval(e: Expr): Int = e match
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)

val expr = Sum(Number(1), Number(1))
eval(expr)

def show(e: Expr): String = e match
    case Number(n) => s"$n" //or n.toString
    case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"

show(expr)
 