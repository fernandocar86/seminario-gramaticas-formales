docs:
	lgrind -d lgrindef -i -lSICStus ../setup.pl > setup.tex
	lgrind -d lgrindef -i -lSICStus ../parser/mgpx.pl > mgpx.tex
	lgrind -d lgrindef -i -lSICStus ../parser/lpx.pl > lpx.tex
	lgrind -d lgrindef -i -lSICStus ../parser/mghapx.pl > mghapx.tex
	lgrind -d lgrindef -i -lSICStus ../parser/lhapx.pl > lhapx.tex
	lgrind -d lgrindef -i -lSICStus ../grammars/logic1.pl > logic1.tex
	lgrind -d lgrindef -i -lSICStus ../grammars/g0.pl > g0.tex
	lgrind -d lgrindef -i -lSICStus ../grammars/gh6.pl > gh6.tex
	lgrind -d lgrindef -i -lSICStus ../grammars/larsonian1.pl > larsonian1.tex
	latex listing
	latex listing
	dvips listing -o
	ps2pdf listing.ps
	/bin/rm listing.ps
	open listing.pdf

lrn:
	lgrind -d lgrindef -i -lSICStus ../learner/rg.pl > rg.tex
	latex lrnlisting
	latex lrnlisting
	dvips lrnlisting -o
	ps2pdf lrnlisting.ps
	/bin/rm lrnlisting.ps
	open lrnlisting.pdf

clean:
	/bin/rm -f *.dvi *.log *.dlg *.aux *.toc
	/bin/rm -f g*.tex log*.tex lar*.tex lp*.tex lh*.tex m*.tex s*.tex r*.tex
	-gzip -f *.ps
