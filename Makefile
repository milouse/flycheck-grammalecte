GRAMVER = 0.6.5

EMACS=emacs -Q -q --batch -nw --eval "(package-initialize)"

.PHONY: build deps clean uninstall

all: build

build: deps flycheck-grammalecte.elc
	$(info You should now add the following line to your .emacs.d/init.el file)
	$(info (load-file "$(PWD)/flycheck-grammalecte.elc"))

%.elc: %.el
	$(info Compiling $(PWD)/$<...)
	@$(EMACS) -f batch-byte-compile $<

deps: grammalecte/__init__.py

grammalecte/__init__.py:
	wget https://www.dicollecte.org/grammalecte/zip/Grammalecte-fr-v$(GRAMVER).zip
	mkdir Grammalecte-fr-v$(GRAMVER)
	unzip Grammalecte-fr-v$(GRAMVER).zip -d Grammalecte-fr-v$(GRAMVER)
	mv Grammalecte-fr-v$(GRAMVER)/grammalecte .

clean:
	rm -rf Grammalecte-fr-v$(GRAMVER)
	rm -f Grammalecte-fr-v$(GRAMVER).zip

uninstall: clean
	rm -rf grammalecte
	rm -f flycheck-grammalecte.elc
