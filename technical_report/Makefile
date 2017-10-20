
technical_report.pdf: technical_report.tex ../literature.bib ../macros.tex Makefile $(wildcard sections/*.tex) $(wildcard ../img/*.eps)
	   			  	  latexmk -pdf $<

clean:
		rm -rf p.pdf *.out *.bbl *.log *.blg *.aux *.bcf *.fls *.loa \
    	*.log *.snm *.nav *.toc *.fdb_* *-blx.bib *.run.xml *.lof *.lot