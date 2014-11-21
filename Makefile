PROJECT   = eon

# Options ##############################################################
EUNIT_OPTS = [verbose]
ERLC_OPTS = +debug_info +nowarn_shadow_vars +warnings_as_errors

# Dependecies ##########################################################
DEPS = stdlib2

dep_stdlib2 = git://github.com/kivra/stdlib2.git master

# Standard targets #####################################################
include erlang.mk

ifneq ($(wildcard test/),)
ebin/$(PROJECT).app:: $(shell find test -type f -name \*.erl)
	$(if $(strip $?),$(call compile_erl,$?))
endif

repl:
	@erl -pa ../$(PROJECT)/ebin -pa ../$(PROJECT)/deps/**/ebin

eunit: ERLC_OPTS = $(TEST_ERLC_OPTS) # to have -ifdef(TEST) working properly
eunit: clean app
	$(gen_verbose) erl -noshell -pa ebin -eval 'eunit:test({application, $(PROJECT)}, [])' -s init stop

# eof
