all:
	mkdir -p compiled
	latex -output-directory=compiled base.tex
	latex -output-directory=compiled base.tex
	bibtex compiled/base
	latex -output-directory=compiled base.tex
	pdflatex -output-directory=compiled base.tex
	mv compiled/base.pdf presentacion.pdf

clean:
	rm -f compiled/*
	rm -f *.log *.dvi *.aux *.blg *.ps *.nav *.out *.snm *.toc *.bbl *.vrb

eps:
	convert  img/versioncontrol1.png img/versioncontrol1.eps

