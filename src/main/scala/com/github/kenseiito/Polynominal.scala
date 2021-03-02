package com.github.kenseiito

object Try {

  def main(args: Array[String]): Unit = {
    val p = Polynominal(Vector(2, -1, 0, -3, 1))
    val q = Polynominal(Vector(-2, -1, 3, -1, 0))

    println(p)
    println(-p)
    println(p - p)
    //println(p + q)
    //println(p * q)
  }
}

case class Polynominal(coefficients: Vector[Int]) {
  //多項式は降べき順の係数リストとして、ただし定数0は空リストとして実装、最高次係数はnon zero
  require(coefficients.isEmpty || coefficients.head != 0)

  //定数0の次数は-1として定義してしまう
  val degree = coefficients.length - 1

  override def toString: String =
    if (this.coefficients.isEmpty) "0"
    else {
      val stringVector =
        for {
          i <- 0 to this.degree
          j = this.degree - i
          c = this.coefficients(i)
          if c != 0
          //冪の文字列
          powerString = j match {
            //0次の冪
            case 0 => ""
            //1次の冪
            case 1 => "X"
            case _ => s"X^$j"
          }
          //演算子および係数の文字列
          //係数 1 は基本省略、負の係数の演算子は -
          cString =
            //最高次は演算子なし
            if (j == this.degree)
              if (c == 1) ""
              else if (c == -1) "- "
              else if (c > 0) s"$c"
              else s"- ${c.abs}"
            //定数項は1を略さない
            else if (j == 0)
              if (c > 0) s" + $c"
              else s" - ${c.abs}"
            else if (c == 1) " + "
            else if (c == -1) " - "
            else if (c > 0) s" + $c"
            else s" - ${c.abs}"
        } yield cString + powerString

      stringVector.mkString("")
    }

  //和
  def +(that: Polynominal) = {
    //要素 0 によって長さを合わせ、成分ごとの和をとる
    //リストの先頭が0かも知れない
    val resCoefficients = this.coefficients.zipAll(that.coefficients, 0, 0).map({ case (x, y) => x + y })

    //リストを基本コンストラクタに渡せるように
    def regulate(xs: Vector[Int]): Vector[Int] = xs match {
      case Vector()                => Vector()
      case Vector(x, _*) if x != 0 => xs
      case _                       => regulate(xs.tail)
    }

    Polynominal(regulate(resCoefficients))
  }

  //積
  def *(that: Polynominal) = {
    //和との分配則、交換則が成り立つような積の一意的な定義
    val coefficientsAsIndexedSeq =
      for (k <- 0 to (this.degree + that.degree))
        yield {
          val vector =
            for {
              i <- 0 to this.degree
              j <- 0 to that.degree
              if i + j == k
            } yield this.coefficients(i) * that.coefficients(j)
          //for (i <- 0.max(k - that.degree) to this.degree.min(k))
          //yield this.coefficients(i) * that.coefficients(k - i)

          vector.sum
        }

    Polynominal(coefficientsAsIndexedSeq.toVector)
  }

  //和の逆元
  def unary_- = Polynominal(this.coefficients.map(x => -x))

  //差
  def -(that: Polynominal) = this + -that

  //余り付割り算
  def /%(that: Polynominal) = {}
}
