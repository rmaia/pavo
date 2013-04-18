all:
	R CMD Sweave pavo.Rnw 
	pdflatex pavo.tex
	pdflatex pavo.tex
	bibtex pavo.aux
	pdflatex pavo.tex
	open pavo.pdf