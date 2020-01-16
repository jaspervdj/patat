ARCH=$(shell uname -m)
UNAME=$(shell uname | tr 'A-Z' 'a-z')

PATAT_BINARY=$(HOME)/.local/bin/patat
PATAT_VERSION=$(shell sed -n 's/^Version: *//p' *.cabal)
PATAT_PACKAGE=fregot-v$(PATAT_VERSION)-$(UNAME)-$(ARCH)

GHR_VERSION=0.13.0
GHR_NAME=ghr_v$(GHR_VERSION)_$(UNAME)_amd64
GHR_BINARY=$(HOME)/.local/bin/ghr

ifeq ($(UNAME), darwin)
ARCHIVE=zip
ARCHIVE_CREATE=zip -r
ARCHIVE_EXTRACT=unzip
else
ARCHIVE=tar.gz
ARCHIVE_CREATE=tar czf
ARCHIVE_EXTRACT=tar xvzf
endif

ifeq ($(UNAME), darwin)
# We use `?=` to set SOURCE_DATE_EPOCH only if it is not present.  Unfortunately
# we can't use `git --date=unix` since only very recent git versions support
# that, so we need to make a round trip through `date`.
SOURCE_DATE_EPOCH?=$(shell date '+%s' \
					   --date="$(shell git log -1 --format=%cd --date=rfc)")
else
SOURCE_DATE_EPOCH?=$(shell git log -1 --format=%cd --date=unix)
endif

build: $(PATAT_BINARY)

$(PATAT_PACKAGE).$(ARCHIVE): $(PATAT_BINARY)
	mkdir $(PATAT_PACKAGE)
	cp $(PATAT_BINARY) $(PATAT_PACKAGE)/
	cp README.md $(PATAT_PACKAGE)/
	cp CHANGELOG.md $(PATAT_PACKAGE)/
	cp LICENSE $(PATAT_PACKAGE)/
	$(ARCHIVE_CREATE) $(PATAT_PACKAGE).$(ARCHIVE) $(PATAT_PACKAGE)

$(PATAT_BINARY):
	stack build -j1 --copy-bins --pedantic

# GHR is used to upload releases to GitHub.
$(GHR_BINARY):
	curl -Lo /tmp/$(GHR_NAME).$(ARCHIVE) \
	    https://github.com/tcnksm/ghr/releases/download/v$(GHR_VERSION)/$(GHR_NAME).$(ARCHIVE)
	cd /tmp && $(ARCHIVE_EXTRACT) $(GHR_NAME).$(ARCHIVE)
	mv /tmp/$(GHR_NAME)/ghr $(GHR_BINARY)
	ghr --version

# Man page.
extra/patat.1: README.md $(PATAT_BINARY)
	SOURCE_DATE_EPOCH="$(SOURCE_DATE_EPOCH)" patat-make-man >$@

# Bash completion.
extra/patat.bash-completion:
	patat --bash-completion-script patat >$@

.PHONY: completion
completion: extra/patat.bash-completion

.PHONY: man
man: extra/patat.1

# Also check if we can generate the manual.
.PHONY: test
test: man
	bash tests/golden.sh

.PHONY: clean
clean:
	rm -f extra/patat.1
	rm -f extra/make-man
	rm -f extra/patat.bash-completion
