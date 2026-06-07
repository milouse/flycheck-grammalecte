EMACS=emacs -Q --batch -nw

.PHONY: autoloads clean cleanall

define LOADDEFS_TPL
(add-to-list 'load-path (directory-file-name\n\
........................(or (file-name-directory #$$) (car load-path))))
endef
#' (ends emacs font-face garbage due to previous single quote)

grammalecte-loaddefs.el:
	rm -f $@
	$(EMACS) -L $(PWD) \
		--eval "(setq-default backup-inhibited t)" \
		--eval "(loaddefs-generate \"$(PWD)\" \"$(PWD)/$@\" nil \"$(subst ., ,$(LOADDEFS_TPL))\")"

autoloads: grammalecte-loaddefs.el

clean:
	rm -f *.zip
	rm -rf Grammalecte-fr-v*

cleanall: clean cleandemo
	rm -rf grammalecte *-master
	rm -f grammalecte-loaddefs.el

grammalecte:
	$(EMACS) --eval "(setq grammalecte-settings-file \"/dev/null\")" \
			-l grammalecte.el -f grammalecte-download-grammalecte

######### Demo related targets

.PHONY: demo demo-with-grammalecte demo-classic demo-classic-with-grammalecte \
	demo-deps cleandemo

EMACS_DEMO = emacs --init-directory "$(PWD)/test-home" --debug-init

demo: demo-deps
	$(EMACS_DEMO) -l test-home/use-package.el example.org example.tex

demo-with-grammalecte: demo-deps grammalecte
	$(EMACS_DEMO) -l test-home/use-package.el example.org example.tex

demo-classic: demo-deps
	$(EMACS_DEMO) -l test-home/classic.el example.org example.tex

demo-classic-with-grammalecte: demo-deps grammalecte
	$(EMACS_DEMO) -l test-home/classic.el example.org example.tex

demo-deps: cleandemo build autoloads dash-master flycheck-master epl-master
	touch debug

cleandemo:
	rm -rf grammalecte
	rm -f debug "#example.org#"
	rm -f test-home/grammalecte-cache.el

######### Dependencies

epl_author = cask
dash.el_author = magnars
flycheck_author = flycheck

%.zip:
	curl -Lso $@ https://github.com/$($(@:%.zip=%)_author)/$(@:%.zip=%)/archive/master.zip

%-master: %.zip
	unzip -qo $<
