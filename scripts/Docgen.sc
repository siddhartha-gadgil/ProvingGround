import ammonite.ops._
implicit val wd = cwd
%sbt("doc")
lazy val allProjects = ls(wd) filter ((x) => x.isDir
  && (ls(x) exists (_.name == "target"))
  && !(x.name.startsWith("."))
  && !(x.name.startsWith("."))
  && !(Set("gh-pages", "docs", "data", "project") contains (x.name))
  )
def apiPair(p: Path) = ls.rec(p) find (_.name == "api") map ((p, _))
val docs = cwd / "docs"
def apiTarg(name: String) = docs / name / "api"
lazy val apiPairs = (allProjects map (apiPair)).flatten
def cpApi = apiPairs.foreach{
  case (dir, api) =>
    rm(apiTarg(dir.name))
    cp(api, apiTarg(dir.name))
  }
cpApi
