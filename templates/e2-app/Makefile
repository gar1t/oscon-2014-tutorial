PROJECT = sample

DEPS = e2

dep_e2 = https://github.com/gar1t/e2.git

include erlang.mk

shell:
	ERL_LIBS=deps erl -pa ebin -s reloader -s $(PROJECT)
