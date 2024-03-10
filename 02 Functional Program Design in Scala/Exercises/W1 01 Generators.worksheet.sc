trait Generator[+T]:
    def generate(): T

val integers = new Generator[Int]:
    val rand = java.util.Random()
    def generate(): Int = rand.nextInt()

val booleans = new Generator[Boolean]:
    def generate(): Boolean = integers.generate() > 0

val pairs = new Generator[(Int, Int)]:
    def generate(): (Int, Int) = (integers.generate(), integers.generate())

pairs.generate()
