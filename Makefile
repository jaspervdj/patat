ARCH=$(shell uname -m)
UNAME=$(shell uname | tr 'A-Z' 'a-z')

PATAT_BINARY=$(HOME)/.local/bin/patat
PATAT_VERSION=$(shell sed -n 's/^Version: *//p' *.cabal)
PATAT_PACKAGE=patat-v$(PATAT_VERSION)-$(UNAME)-$(ARCH)

GHR_VERSION=0.13.0
GHR_NAME=ghr_v$(GHR_VERSION)_$(UNAME)_amd64
GHR_BINARY=$(HOME)/.local/bin/ghr

UPX_VERSION=3.94
UPX_NAME=upx-$(UPX_VERSION)-amd64_$(UNAME)
UPX_BINARY=$(HOME)/.local/bin/upx

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
SOURCE_DATE_EPOCH?=$(shell git log -1 --format=%cd --date=unix)
else
SOURCE_DATE_EPOCH?=$(shell date '+%s' \
                       --date="$(shell git log -1 --format=%cd --date=rfc)")
endif

ifeq ($(UNAME), darwin)
COMPRESS_BIN_DEPS=
COMPRESS_BIN=ls
else
COMPRESS_BIN_DEPS=$(UPX_BINARY)
COMPRESS_BIN=upx
endif

# Default target.
build: $(PATAT_BINARY)

# Upload a release.
release: $(PATAT_PACKAGE).$(ARCHIVE) $(GHR_BINARY)
	ghr -u jaspervdj -r patat v$(PATAT_VERSION) $(PATAT_PACKAGE).$(ARCHIVE)

$(PATAT_PACKAGE).$(ARCHIVE): $(PATAT_BINARY) extra/patat.1 $(COMPRESS_BIN_DEPS)
	mkdir $(PATAT_PACKAGE)
	cp $(PATAT_BINARY) $(PATAT_PACKAGE)/
	$(COMPRESS_BIN) $(PATAT_PACKAGE)/patat
	cp README.md $(PATAT_PACKAGE)/
	cp CHANGELOG.md $(PATAT_PACKAGE)/
	cp LICENSE $(PATAT_PACKAGE)/
	cp extra/patat.1 $(PATAT_PACKAGE)/
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

# UPX is used to compress the resulting binary.  We currently don't use this on
# Mac OS.
$(UPX_BINARY):
	curl -Lo /tmp/$(UPX_NAME).tar.xz \
	    https://github.com/upx/upx/releases/download/v$(UPX_VERSION)/$(UPX_NAME).tar.xz
	cd /tmp && tar xf $(UPX_NAME).tar.xz
	mv /tmp/$(UPX_NAME)/upx $(UPX_BINARY)
	upx --version

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
