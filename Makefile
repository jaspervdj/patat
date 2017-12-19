# We use `?=` to set SOURCE_DATE_EPOCH only if it is not present.  Unfortunately
# we can't use `git --date=unix` since only very recent git versions support
# that, so we need to make a round trip through `date`.
SOURCE_DATE_EPOCH?=$(shell date '+%s' \
					   --date="$(shell git log -1 --format=%cd --date=rfc)")

extra/patat.1: README.md
	SOURCE_DATE_EPOCH="$(SOURCE_DATE_EPOCH)" patat-make-man >$@

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
