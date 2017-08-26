PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar3

.PHONY: all clean

all:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
