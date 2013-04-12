all:
	R CMD Sweave MSBody.Rnw 
	R CMD pdflatex MSBody.tex
	R CMD bibtex MSBody.aux
	R CMD pdflatex MSBody.tex
	R CMD pdflatex MSBody.tex
	R CMD texcount MSBody.tex
	open MSBody.pdf
	rm MSBody.aux
	rm MSbody.log
	rm MSbody.out