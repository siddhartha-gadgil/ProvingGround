import ammonite.ops._
implicit val wd = cwd
lazy val allProjects = ls(wd) filter ((x) => x.isDir && (ls(x) exists (_.name == "target")))
val projectNames= List("core" -> "coreJVM", "mantle" -> "mantle")
def api(p: Path) = ls.rec(p) find (_.name == "api")
projectNames map {case (file, name) => %sbt (s"$name/doc")}
% git ("-C", "gh-pages", "pull")
for ((file, name) <- projectNames; dir <- api(cwd / file)) yield {
    rm(cwd / "gh-pages" / file)
    cp(dir, cwd / "gh-pages" / file)
  }// copying to a clone of the gh-pages branch
% git ("-C", "gh-pages", "add", ".", "--all")
% git ("-C", "gh-pages", "commit", "-am", """"api-update"""")
% git ("-C", "gh-pages", "push")
println("Latest git log and status from gh-pages")
%git ("-C", "gh-pages", "log", "-1")
%git ("-C", "gh-pages", "status")
