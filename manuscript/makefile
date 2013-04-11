#!/bin/bash
# echo "Type the 1 of the file and press enter:\c"
# read 1
all:
	R CMD Sweave MSBody.Rnw 
	R CMD pdflatex MSBody.tex
	R CMD bibtex MSBody.aux
	R CMD pdflatex MSBody.tex
	R CMD pdflatex MSBody.tex
	R CMD texcount MSBody.tex
	open MSBody.pdf