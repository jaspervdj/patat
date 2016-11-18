extra/patat.1: README.md extra/make-man
	./extra/make-man >$@

extra/make-man: extra/make-man.hs
	ghc -o $@ $<

man: extra/patat.1

test:
	bash test.sh

.PHONY: man test
