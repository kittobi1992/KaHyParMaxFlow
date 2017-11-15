
master_thesis.pdf: master_thesis.tex ../literature.bib ../macros.tex Makefile $(wildcard sections/*.tex) $(wildcard img/*.tex)
	   			   latexmk -pdf -f --shell-escape $<

clean:
		rm -rf *.out *.bbl *.log *.blg *.aux *.bcf *.fls *.loa *.gz *.md5 \
    	*.log *.snm *.nav *.toc *.fdb_* *-blx.bib *.run.xml *.lof *.lot *.dpth *.auxlock \
		experiments/*.md5 experiments/*.dpth experiments/*.log experiments/*.pdf