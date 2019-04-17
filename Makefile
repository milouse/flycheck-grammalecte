GRAMVER = 0.6.5

EMACS=emacs -Q -q --batch -nw --eval "(package-initialize)"

.PHONY: build clean uninstall

all: build

build: grammalecte flycheck-grammalecte.elc
	$(info You should now add the following line to your .emacs.d/init.el file)
	$(info (load-file "$(PWD)/flycheck-grammalecte.elc"))

%.elc: %.el
	$(info Compiling $(PWD)/$<...)
	@$(EMACS) -f batch-byte-compile $<

Grammalecte-fr-v$(GRAMVER).zip:
	curl -O https://grammalecte.net/grammalecte/zip/Grammalecte-fr-v$(GRAMVER).zip

Grammalecte-fr-v$(GRAMVER): Grammalecte-fr-v$(GRAMVER).zip
	mkdir -p Grammalecte-fr-v$(GRAMVER)
	unzip -o Grammalecte-fr-v$(GRAMVER).zip -d Grammalecte-fr-v$(GRAMVER)

grammalecte: Grammalecte-fr-v$(GRAMVER)
	cp -R Grammalecte-fr-v$(GRAMVER)/grammalecte .

clean:
	rm -rf Grammalecte-fr-v$(GRAMVER)
	rm -f Grammalecte-fr-v$(GRAMVER).zip

uninstall: clean
	rm -rf grammalecte
	rm -f flycheck-grammalecte.elc
