#-----------------------------------------------------------------------------
# LaTeX

LATEX         = latex
LATEX_OPTS    =
PDFLATEX      = pdflatex
PDFLATEX_OPTS =

#-----------------------------------------------------------------------------
# lhs2TeX

LHS2TEX       = lhs2TeX

#-----------------------------------------------------------------------------
# dvips

DVIPS       = dvips
DVIPS_OPTS  =

#-----------------------------------------------------------------------------
# pattern rules

pdf : talk.pdf

%.tex : %.lhs
	$(LHS2TEX) --poly $< > $@

%.dvi : %.tex
	$(LATEX) $(LATEX_OPTS) $<

%.4.ps : %.ps
	psnup -4 -r $< $@

%.ps : %.dvi
	$(DVIPS) $(DVIPS_OPTS) -o $@ $<

%.pdf : %.tex
	$(PDFLATEX) $(PDFLATEX_OPTS) $<

%.ghci: %.lhs
	ghci -fglasgow-exts -pgmL $(LHS2TEX) -optL --pre $<

#-----------------------------------------------------------------------------
# rules

.PHONY : clean

clean :
	rm -f *~ *% *.aux *.bbl *.blg *.log *.toc *.out *.nav *.snm *.rel
	rm -f *.tex *.[1-9]* *.mpx *.ptb *.pdfsync

realclean : clean
	rm -f *.dvi *.ps *.pdf

