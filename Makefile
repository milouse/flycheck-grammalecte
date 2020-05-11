GRAMVER = $(shell curl -s https://grammalecte.net/index.html | sed -n 's|^ *<p id="version_num">\([0-9.]*\)</p>|\1|p')

EMACS=emacs -Q --batch -nw

.PHONY: build clean uninstall

.INTERMEDIATE: Grammalecte-fr-v$(GRAMVER).zip dash.zip flycheck.zip

all: build

build: grammalecte flycheck-grammalecte.elc

flycheck-grammalecte.elc: flycheck-grammalecte.el dash.el-master/dash.el flycheck-master/flycheck.el
	$(EMACS) --eval "(add-to-list 'load-path \"dash.el-master\")" \
		--eval "(add-to-list 'load-path \"flycheck-master\")" \
		-f batch-byte-compile flycheck-grammalecte.el

dash.zip:
	curl -Lso dash.zip https://github.com/magnars/dash.el/archive/master.zip

dash.el-master/dash.el: dash.zip
	unzip -qo dash.zip

flycheck.zip:
	curl -Lso flycheck.zip https://github.com/flycheck/flycheck/archive/master.zip

flycheck-master/flycheck.el: flycheck.zip
	unzip -qo flycheck.zip

Grammalecte-fr-v$(GRAMVER).zip:
	curl -sO https://grammalecte.net/grammalecte/zip/Grammalecte-fr-v$(GRAMVER).zip

Grammalecte-fr-v$(GRAMVER): Grammalecte-fr-v$(GRAMVER).zip
	mkdir -p Grammalecte-fr-v$(GRAMVER)
	unzip -qo Grammalecte-fr-v$(GRAMVER).zip -d Grammalecte-fr-v$(GRAMVER)

grammalecte:
	[ ! -d Grammalecte-fr-v$(GRAMVER) ] && $(MAKE) Grammalecte-fr-v$(GRAMVER)
	cp -R Grammalecte-fr-v$(GRAMVER)/grammalecte .

clean:
	rm -rf Grammalecte-fr-v*
	rm -f debug "#example.org#"

uninstall: clean
	rm -rf grammalecte dash.el-master flycheck-master
	rm -f flycheck-grammalecte.elc
