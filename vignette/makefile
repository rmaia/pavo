all:
	R CMD Sweave pavo.Rnw 
	pdflatex pavo.tex
	pdflatex pavo.tex
	bibtex pavo.aux
	pdflatex pavo.tex
	texcount pavo.tex
	open pavo.pdf
	rm pavo.aux
	rm pavo.log
	rm pavo.out