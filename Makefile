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
	rm -rf Grammalecte-fr-v*

cleanall: clean cleandemo
	rm -rf grammalecte vendor
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

demo-deps: cleandemo | vendor/dash vendor/flycheck
	touch debug

cleandemo:
	rm -rf grammalecte
	rm -f debug "#example.org#"
	rm -f test-home/grammalecte-cache.el

######### Dependencies

dash_url = https://elpa.gnu.org/packages/dash-2.20.0.tar
flycheck_url = https://elpa.nongnu.org/nongnu/flycheck-36.0.tar

define SETUP_DEP =
curl -Lso $@.tar $($(@F)_url)
tar -C vendor -xf $@.tar
rm $@.tar
mv vendor/$(@F)-[0-9.]* $@
endef

vendor:
	mkdir vendor
vendor/dash: | vendor
	$(SETUP_DEP)
vendor/flycheck: | vendor
	$(SETUP_DEP)
