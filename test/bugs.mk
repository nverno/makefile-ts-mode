# -*- mode: makefile-ts -*-
X = 1                          # doesn't add node for end of line comments

FOO = $(info \
	foo says $(1)) \
      ${info bar says $(1)}  # ${...} function calls unrecognized

# Errors on parens in quotes in assignments
# BAR = echo -f $(P) \
# 	--eval "(setq error here)"

# treats function calls as shell_text
all:
	$(call FOO,\
	bar)
	@echo hi \
	echo hey$(info BAR is $(BAR)) there

# with empty recipe, thinks following conditional is part of recipe
# test:
# 	@

# FOO=$(foreach bar,$(BARS), $(foreach baz,$(BAZS),foo-$(bar)-$(baz)))
# define ABC
# XYZ = Hello World
# endef

# doesnt error if ifdef variable is on newline w/o escape
ifndef \
USE_BAR
BAR = 1
else
BAR = 2
endif

bar :
	echo $(BAR)

# Makefile
define logger
  echo $(1)
endef

foo:
	$(call logger, '(string_param_with_parentheses)')
	$(call logger, '(spaces are OK)')
	$(call logger, "(double quotes are fine)")
