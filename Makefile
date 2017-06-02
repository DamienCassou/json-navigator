CASK        ?= cask
EMACS       ?= emacs
DIST        ?= dist
EMACSFLAGS   = --batch -Q
EMACSBATCH   = $(EMACS) $(EMACSFLAGS)

VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)
PROJ_ROOT   := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

EMACS_D      = ~/.emacs.d
USER_ELPA_D  = $(EMACS_D)/elpa

SRCS         = $(filter-out %-pkg.el, $(wildcard *.el))
TESTS        = $(wildcard test/*.el)
TAR          = $(DIST)/json-navigator-$(VERSION).tar


.PHONY: all check lint deps install uninstall reinstall clean-all clean clean-elc
all : deps $(TAR)

deps :
	$(CASK) install

install : $(TAR)
	$(EMACSBATCH) -l package -f package-initialize \
	--eval '(package-install-file "$(PROJ_ROOT)/$(TAR)")'

uninstall :
	rm -rf $(USER_ELPA_D)/json-navigator-*

reinstall : clean uninstall install

clean-all : clean
	rm -rf $(PKG_DIR)

clean-elc :
	rm -f *.elc

clean : clean-elc
	rm -rf $(DIST)
	rm -f *-pkg.el

$(TAR) : $(DIST) $(SRCS)
	$(CASK) package $(DIST)

$(DIST) :
	mkdir $(DIST)

check : lint

lint : $(SRCS) clean-elc
	# Byte compile all and stop on any warning or error
	${CASK} emacs $(EMACSFLAGS) \
	--eval "(setq byte-compile-error-on-warn t)" \
	-L . -f batch-byte-compile ${SRCS} ${TESTS}

	# Run package-lint to check for packaging mistakes
	${CASK} emacs $(EMACSFLAGS) \
	-l package-lint.el \
	-f package-lint-batch-and-exit ${SRCS}
