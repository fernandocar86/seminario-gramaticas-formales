clean:
	-/bin/mv *~ old
	-/bin/mv d*/*~ old
	-/bin/mv g*/*~ old
	-/bin/mv l*/*~ old
	-/bin/mv p*/*~ old
	-/bin/mv s*/*~ old
	-/bin/mv t*/*~ old
	/bin/rm -f *.dvi *.log *.dlg *.ps *.aux *.toc
	/bin/rm -f dotdep.* ltree.* qtree.* wishtemp
	/bin/rm -f docs/*.dvi docs/*.log docs/*.dlg docs/*.ps docs/*.aux docs/*.toc
	/bin/rm -f docs/g*.tex docs/log*.tex docs/lar*.tex docs/lp*.tex
	/bin/rm -f docs/lh*.tex docs/m*.tex docs/s*.tex docs/r*.tex

ltree:
	latex ltree
	dvips ltree
	ps2pdf ltree.ps
	/bin/rm ltree.ps
	open ltree.pdf
