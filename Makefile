
#-------------------------------------------------------------------------------
# Commands and programs

LHS2TEX         := lhs2TeX
LATEXMK		:= latexmk
GHC             := ghc
CPP             := cpp -P

#-------------------------------------------------------------------------------
# File rules

all : talk.pdf

#-------------------------------------------------------------------------------
# Pattern rules

%.pdf : %.tex talk.sty talk.fmt pause.h
	$(LATEXMK) -pdf $<

%.tex : %.lhs
	$(CPP) $< | $(LHS2TEX) --file-directives --poly -slatex > $@

%.tex : %.lagda
	$(CPP) $< | $(LHS2TEX) --agda --file-directives --poly -slatex > $@

%.hs : %.lhs talk.fmt
	$(CPP) $< | $(LHS2TEX) --newcode -shscode > $@

#-------------------------------------------------------------------------------
# Other rules

clean :
	rm -f *~ *.aux *.bbl *.blg *.log *.vrb *.toc *.out *.nav *.snm *.rel *.mpx *.ptb *.fdb_latexmk *.pdfsync printf.o a.out

realclean : clean
	rm -f *.pdf

#-------------------------------------------------------------------------------
# Other

.PHONY : clean realclean

# .SECONDARY :

