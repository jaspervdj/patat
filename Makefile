PANDOC_MINOR_VERSION=$(shell ghc-pkg latest pandoc | sed 's/.*-//' | cut -d. -f2)

extra/patat.1: README.md extra/make-man
	./extra/make-man >$@

extra/make-man: extra/make-man.hs
	ghc -DPANDOC_MINOR_VERSION=${PANDOC_MINOR_VERSION} -o $@ $<

man: extra/patat.1

# Also check if we can generate the manual.
test: man
	bash test.sh

.PHONY: man test
