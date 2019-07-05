#!/bin/bash

for note in notes/*.ipynb
do
    echo $note
    jupyter nbconvert --to html --output ../docs/$note.html $note
done