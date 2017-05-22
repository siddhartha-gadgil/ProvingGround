package provingground.translation

/**
  * @author gadgil
  */
object StringParse {
  trait WriteString[A] {
    def writer(a: A): String
  }

  object WriteString {
    def apply[A](w: A => String): WriteString[A] = new WriteString[A] {
      def writer(a: A) = w(a)
    }

    def simple[A] = apply((a: A) => a.toString)
  }

  trait ReadString[A] {
    def reader(str: String): A
  }

  object ReadString {
    def apply[A](r: String => A): ReadString[A] = new ReadString[A] {
      def reader(s: String) = r(s)
    }
  }

  def write[A: WriteString](a: A) = {
    implicitly[WriteString[A]].writer(a)
  }

  def read[A: ReadString](str: String) = {
    implicitly[ReadString[A]].reader(str)
  }

  implicit def idWrite: WriteString[String] = new WriteString[String] {
    def writer(a: String) = a
  }
}
