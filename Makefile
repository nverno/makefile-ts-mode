SHELL = /bin/bash

TSDIR     ?= $(CURDIR)/tree-sitter-make
TS_REPO   ?= https://github.com/alemuller/tree-sitter-make
TS_BRANCH ?= main
TESTDIR   ?= $(TSDIR)/examples

all:
	@

dev: $(TSDIR)
$(TSDIR):
	@git clone -b $(TS_BRANCH) --depth=1 $(TS_REPO)
	@printf "\33[1m\33[31mNote\33[22m npm build can take a while\e[0m\n" >&2
	cd $(TSDIR) &&                                         \
		npm --loglevel=info --progress=true install && \
		npx tree-sitter generate

.PHONY: parse-%
parse-%:
	cd $(TSDIR) && npx tree-sitter parse $(TESTDIR)/$(subst parse-,,$@)

clean:
	$(RM) -r *~

distclean: clean
	$(RM) -rf $$(git ls-files --others --ignored --exclude-standard)
