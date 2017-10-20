
master_thesis.pdf: master_thesis.tex ../literature.bib ../macros.tex Makefile $(wildcard sections/*.tex) $(wildcard img/*.tex)
	   			   latexmk -pdf $<

clean:
		rm -rf p.pdf *.out *.bbl *.log *.blg *.aux *.bcf *.fls *.loa \
    	*.log *.snm *.nav *.toc *.fdb_* *-blx.bib *.run.xml *.lof *.lot