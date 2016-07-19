import ammonite.ops._
object Docgen extends App {
  val wd = cwd
  val projects = ls(wd) filter ((x) => x.isDir && (ls(x) exists (_.name == "target")))
  def api(p: Path) = ls.rec(p) find (_.name == "api")
  %sbt 'doc
  for (p <- projects; dir <- api(p)) yield {
    rm(cwd / "gh-pages" / p.name)
    cp(dir, cwd / "gh-pages" / p.name)
  }// copying to a clone of the gh-pages branch
  % git ("-C", "gh-pages", "add", ".", "--all")
  % git ("-C", "gh-pages", "commit", "-am", """"api-update"""")
  % git ("-C", "gh-pages", "push")
  %git ("-C", "gh-pages", "log")

}
