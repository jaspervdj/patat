ARCH=$(shell uname -m)
UNAME=$(shell uname | tr 'A-Z' 'a-z')

PATAT_BINARY=$(HOME)/.local/bin/patat
PATAT_TAG?=v$(shell sed -n 's/^Version: *//p' *.cabal)
PATAT_PACKAGE=patat-$(PATAT_TAG)-$(UNAME)-$(ARCH)

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

SOURCE_DATE_EPOCH?=$(shell git log -1 --format=%cd --date=unix)

ifeq ($(UNAME), darwin)
COMPRESS_BIN_DEPS=
COMPRESS_BIN=ls
else
COMPRESS_BIN_DEPS=$(UPX_BINARY)
COMPRESS_BIN=upx
endif

# Default target.
.PHONY: build
build: $(PATAT_BINARY)

# When we want to do a release.
.PHONY: artifact
artifact: $(PATAT_PACKAGE).$(ARCHIVE)
	mkdir -p artifacts
	cp $(PATAT_PACKAGE).$(ARCHIVE) artifacts/

$(PATAT_PACKAGE).$(ARCHIVE): $(PATAT_BINARY) extra/patat.1 $(COMPRESS_BIN_DEPS)
	mkdir -p $(PATAT_PACKAGE)
	cp $(PATAT_BINARY) $(PATAT_PACKAGE)/
	$(COMPRESS_BIN) $(PATAT_PACKAGE)/patat
	cp README.md $(PATAT_PACKAGE)/
	cp CHANGELOG.md $(PATAT_PACKAGE)/
	cp LICENSE $(PATAT_PACKAGE)/
	cp extra/patat.1 $(PATAT_PACKAGE)/
	$(ARCHIVE_CREATE) $(PATAT_PACKAGE).$(ARCHIVE) $(PATAT_PACKAGE)

$(PATAT_BINARY):
	cabal install --installdir="$(dir $(PATAT_BINARY))"

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
	SOURCE_DATE_EPOCH="$(SOURCE_DATE_EPOCH)" cabal exec -- patat-make-man >$@

# Bash completion.
extra/patat.bash-completion: $(PATAT_BINARY)
	cabal exec -- patat --bash-completion-script patat >$@

.PHONY: completion
completion: extra/patat.bash-completion

.PHONY: man
man: extra/patat.1

# Also check if we can generate the manual.
.PHONY: test
test: man
	cabal test

.PHONY: clean
clean:
	rm -f extra/patat.1
	rm -f extra/make-man
	rm -f extra/patat.bash-completion
