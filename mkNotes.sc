#!/usr/bin/env amm
val notesDir = os.pwd / 'notes
val notes = os.list(notesDir).filter(_.ext == "ipynb").sortBy(_.last)
def nb(p: os.Path) = {
    pprint.log(p.last)
    os.proc("jupyter", "nbconvert", "--to", "html", "--output", (os.pwd / 'docs / 'notes / p.last).toString, p.relativeTo(os.pwd).toString).call()
}
notes.foreach(nb)
def item(p: os.Path) = {
    val name = p.last
    s"""<li><a href="$name.html">$name</a></li>"""
}
val allItems = notes.map(item).mkString("\n")
val index =
s"""
<html>
    <head>
        <title>ProvingGround Notebooks</title>
        <link href="../css/bootstrap.min.css" rel="stylesheet">
    </head>
    <body>
        <div class="container">
            <h2> ProvingGround Notebooks </h2>
            <ul>
            $allItems
            </ul>
        </div>
    </body>
</html>
"""
os.write.over(os.pwd / 'docs / 'notes / "index.html", index)