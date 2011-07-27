# Makefile for LFMTP 11

destdir=$(HOME)/public_html

# commands
#	  -- 'sort = ON' \
# sorting destroys the important invariant that Proceedings need to appear
# AFTER the referencing InProceedings
bibtool=bibtool -- 'preserve.key.case = ON' \
	  -- 'check.double = ON' \
	  -- 'check.double.delete = ON' \
	  -- 'delete.field = { abstract }' \
	  -- 'delete.field = { dvi }' \
	  -- 'delete.field = { postscript }' \
	  -- 'delete.field = { pdf }' 
#	  -- 'delete.field = { url }' 
catcfg=sed -e "s/%.*//g" <
latex=latex
pdflatex=pdflatex

files=churchFnbe.tex macros.tex

.PHONY : all debugMake

.PRECIOUS : %.dvi %.ps %.gz %.pdf %.tex


default : lfmtp11.pdf

pack : lfmtp11.zip

lfmtp11.zip : lfmtp11.pdf lfmtp11.tex lfmtp11.bbl macros.tex auto-lfmtp11.bib
	zip $@ $^




# lfmtp11
##################################################################

# initially, run latex once to create an .aux file
# mark .aux file as fresh by creating a stamp _aux
lfmtp11_aux : lfmtp11.tex macros.tex
	-$(pdflatex) lfmtp11.tex;
	touch $@;

# then, run bibtex
lfmtp11.bbl : lfmtp11_aux auto-lfmtp11.bib 
	-bibtex lfmtp11;

# finally, finish by running latex twice again
# this will update .aux, but leave the stamp _aux intact
# (otherwise we would not converge and make would never
# return a "Nothing to do")
lfmtp11.pdf : lfmtp11.bbl
	-$(pdflatex) lfmtp11.tex;
	$(pdflatex) lfmtp11.tex;

#lfmtp11.pdf : lfmtp11.dvi
#	dvipdfm -o $@ $<

## pdflatex does not work since we are including a .ps file
#lfmtp11.pdf : lfmtp11.bbl
#	pdflatex lfmtp11.tex;
#	pdflatex lfmtp11.tex;

# auto-lfmtp11.bib is only defined if bibtool is present and all.bib exists

ifneq ($(shell which bibtool),)
ifneq ($(shell ls all.bib),)
auto-lfmtp11.bib : lfmtp11_aux all.bib
	echo "%%%% WARNING! AUTOMATICALLY CREATED BIBFILE" > $@;
	echo "%%%% DO NOT EDIT! ALL CHANGES WILL BE LOST!" >> $@ ;
	echo "" >> $@ ;
	$(bibtool) -x lfmtp11.aux -i all.bib >> $@;
endif
endif

# Templates (reverted back to simple templates)


talk% : talk%.pdf
	cp -p $? $(destdir)/;
	touch $@;

talk%.pdf : talk%.tex
	pdflatex $<;


%.tex : %.lhs
	lhs2TeX --poly -i lhs2TeX.fmt $< > $@
# /usr/local/share/lhs2tex-1.13/

% : %.dvi %.ps.gz %.pdf # %.tar.gz
	cp -p $? $(destdir)/;
	touch $@;

%.dvi : %.tex 
	$(latex) $<;
	-bibtex $*;
	$(latex) $<;
	$(latex) $<;

%.pdf : %.dvi
	pdflatex $*.tex;

%.ps  : %.dvi
	dvips -o $@ $<;

%.gz : %
	cat $< | gzip > $@

## does not work since $ is evaluated before %
#%.tar : %.cfg $(shell cat %.cfg)
#	echo $?


#EOF
