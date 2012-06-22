
#-----------------------------------------------------------------------------
# Files

PROJECT       := talk
CODE          := $(PROJECT).hs
FMTS          := $(PROJECT).fmt
STYS          := $(PROJECT).sty

#-----------------------------------------------------------------------------
# LaTeX

LATEX         := latex
LATEX_OPTS    :=
PDFLATEX      := pdflatex
PDFLATEX_OPTS :=

#-----------------------------------------------------------------------------
# lhs2TeX

LHS2TEX := lhs2TeX

#-----------------------------------------------------------------------------
# GHC

GHCI := ghci

#-----------------------------------------------------------------------------
# dvips

DVIPS       := dvips
DVIPS_OPTS  :=

#-----------------------------------------------------------------------------
# rules

pdf : $(PROJECT).pdf

code : $(CODE)

ghci : $(CODE) .setup force
	$(GHCI) $(CODE)

force : .setup
	@:

.setup :
	touch .setup

.PHONY : clean realclean

clean :
	rm -f *~ *% *.aux *.bbl *.blg *.log *.toc *.out *.nav *.snm *.rel
	rm -f *.tex *.[1-9]* *.mpx *.ptb *.pdfsync
	rm -f .setup

realclean : clean
	rm -f *.dvi *.ps *.pdf

#-----------------------------------------------------------------------------
# pattern rules

$(PROJECT).pdf : $(FMTS) $(STYS)

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

%.hs : %.lhs
	$(LHS2TEX) --newcode $< > $@ || rm -f $@

