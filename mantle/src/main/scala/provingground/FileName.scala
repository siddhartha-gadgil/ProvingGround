package provingground.interface
import provingground._

import ammonite.ops.Path

object FileName {
  def apply(name: String, ext: String) =
    if (name.endsWith("." + ext)) name else s"$name.$ext"

  def unapply(file: Path, ext: String) =
    if (file.ext == ext) Some(file.name.dropRight(ext.length + 1)) else None

  def unapply(file: String, ext: String) =
    if (file.endsWith(ext)) Some(file.dropRight(ext.length + 1)) else None
}
