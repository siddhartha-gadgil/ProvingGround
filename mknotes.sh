#!/bin/bash

printf "<html><title>Notebooks</title>\n<body><h2>Notebooks</h2>\n<ul>\n" > docs/notes/index.html

for note in notes/*.ipynb
do
    echo $note
    printf '<li><a href="../' >> docs/notes/index.html
    printf $note >> docs/notes/index.html
    printf '.html">' >> docs/notes/index.html
    printf $note >> docs/notes/index.html
    printf '</a></li>\n' >> docs/notes/index.html
    jupyter nbconvert --to html --output ../docs/$note.html $note
done

printf "\n</ul></body></html>\n" >> docs/notes/index.html