###  This file is in the public domain, copy and modify it freely. ###

## define the command names of your applications:
LATEX=latex
LATEX_SRC_SPECIALS=latex --src-specials
VIEWER=okular
PDFVIEWER=okular
BIBTEX=bibtex
DVIPS=dvips
PS2PDF=ps2pdf

## define the file name you're working on:
FILE=TEMPLATE


dvi:	# runs LaTeX, produces the .dvi [first item here runs on naked "make"]
	$(LATEX) $(FILE).tex

pdf: 	dvi  # makes pdf from latex via dvips and ps2pdf
	$(DVIPS)  $(FILE).dvi
	$(PS2PDF) $(FILE).ps  

view:	pdf clean  # makes and shows the file
	$(PDFVIEWER) $(FILE).pdf &

bib:	# runs BibTex
	$(BIBTEX) $(FILE)

all:	dvi bib dvi pdf  # nomen est omen

log:	# shows complete log-file 
	cat $(FILE).log

clean:  # throws away trash
	rm -f *.aux
	rm -f *.lof
	rm -f *.log
	rm -f *.lol
	rm -f *.toc
	rm -f *.bbl
	rm -f *.blg
	rm -f *.bak
	rm -f *~
	rm -f *.RData
	rm -f *.Rout
	rm -f *.nav
	rm -f *.out
	rm -f *.snm

cleaner: clean  # throws away all that could be trash
	rm -f $(FILE).pdf
	rm -f $(FILE).ps
	rm -f $(FILE).dvi

## add more commands of your choice here: