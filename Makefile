PANDOC_MINOR_VERSION=$(shell ghc-pkg latest pandoc | sed 's/.*-//' | cut -d. -f2)
PATAT_BUILD_DATE=$(shell date '+%B %d, %Y' -d "$(shell git log -1 --format=%cd --date=short)")

extra/patat.1: README.md extra/make-man
	PATAT_BUILD_DATE="$(PATAT_BUILD_DATE)" ./extra/make-man >$@

extra/make-man: extra/make-man.hs
	ghc -DPANDOC_MINOR_VERSION=${PANDOC_MINOR_VERSION} -Wall -o $@ $<

man: extra/patat.1

# Also check if we can generate the manual.
test: man
	bash test.sh

clean:
	rm -f extra/patat.1
	rm -f extra/make-man

.PHONY: man test clean
