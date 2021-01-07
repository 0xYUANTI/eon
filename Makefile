PROJECT   = eon

# Options ##############################################################
ERLC_OPTS = +debug_info +nowarn_shadow_vars +warnings_as_errors
AUTOPATCH += stdlib2

# Dependecies ##########################################################
DEPS = stdlib2

dep_stdlib2 = git://github.com/kivra/stdlib2.git master

# Standard targets #####################################################
include erlang.mk

# eof
