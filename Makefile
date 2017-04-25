# The minor version is passed to the build.  This is used to do some CPP to
# solve incompatibilities.
PANDOC_MINOR_VERSION=$(shell ghc-pkg latest pandoc | sed 's/.*-//' | cut -d. -f2)

# We use `?=` to set SOURCE_DATE_EPOCH only if it is not present.  Unfortunately
# we can't use `git --date=unix` since only very recent git versions support
# that, so we need to make a round trip through `date`.
SOURCE_DATE_EPOCH?=$(shell date '+%s' \
					   --date="$(shell git log -1 --format=%cd --date=rfc)")

# Prettify the date.
SOURCE_DATE=$(shell env LC_ALL=C TZ=UTC date '+%B %d, %Y' -d "@${SOURCE_DATE_EPOCH}")

extra/patat.1: README.md extra/make-man
	SOURCE_DATE="$(SOURCE_DATE)" ./extra/make-man >$@

extra/make-man: extra/make-man.hs
	ghc -DPANDOC_MINOR_VERSION=${PANDOC_MINOR_VERSION} -Wall -o $@ $<

extra/patat.bash-completion:
	patat --bash-completion-script patat >$@

completion: extra/patat.bash-completion

man: extra/patat.1

# Also check if we can generate the manual.
test: man
	bash test.sh

clean:
	rm -f extra/patat.1
	rm -f extra/make-man
	rm -f extra/patat.bash-completion

.PHONY: man completion test clean
