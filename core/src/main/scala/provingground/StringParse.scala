package provingground

/**
 * @author gadgil
 */
object StringParse {
  trait WriteString[A]{
    def write(a: A) : String
  }
  
  trait ReadString[A]{
    def read(str: String): A
  }
  
  def write[A : WriteString](a: A) = {
    implicitly[WriteString[A]].write(a)
  }
  
  def read[A : ReadString](str: String)= {
    implicitly[ReadString[A]].read(str)
  }
  
  implicit def idWrite : WriteString[String] = new WriteString[String]{
    def write(a: String) = a
  }
}