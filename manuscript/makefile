all:
	R CMD Sweave MSBody.Rnw 
	pdflatex MSBody.tex
	pdflatex MSBody.tex
	bibtex MSBody.aux
	pdflatex MSBody.tex
	texcount MSBody.tex
	open MSBody.pdf
	rm MSBody.aux
	rm MSbody.log
	rm MSbody.out