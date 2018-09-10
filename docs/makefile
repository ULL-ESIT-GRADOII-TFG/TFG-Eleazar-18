all:
	latex lang.tex
	latex lang.tex
	bibtex lang
	latex lang.tex
	latex lang.tex
	dvips -o lang.ps memtfg.dvi
	ps2pdf -sPAPERSIZE=a4 -DMaxSubsetPct=100 -dCompatibilityLevel=1.2 -dSubsetFonts=true -dEmbedAllFonts=true lang.ps memtfg.pdf

clean:
	rm -f *.toc *.ps *.log *.lof *.lot *.dvi *.aux *.blg *.bbl *.glg *.glo *.ist *.alg *.acn *.acr *.out *.gls

pdf:
		pdflatex lang.tex
		pdflatex lang.tex
		bibtex lang
		pdflatex lang.tex
		pdflatex lang.tex

spell:
	# ls -l /usr/local/lib/aspell-0.60
	# aspell dump dicts
	# aspell -t check lang.tex
	aspell check lang.tex
