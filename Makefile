REBAR = ./rebar

.PHONY: all compile test clean
.PHONY: test eunit xref dialyze
.PHONY: release release_minor release_major release_patch

all: compile

compile:
	@$(REBAR) compile

clean:
	@find . -name "*~" -exec rm {} \;
	@$(REBAR) clean

test: compile eunit xref dialyze

eunit: all
	ERL_FLAGS="-sname eunit" $(REBAR) eunit

xref: all
	@$(REBAR) xref

dialyze: all ~/.dialyzer_plt
	dialyzer -Wno_return -nn --plt ~/.dialyzer_plt ebin

~/.dialyzer_plt:
	- dialyzer -nn --output_plt ~/.dialyzer_plt --build_plt \
           --apps erts kernel stdlib crypto public_key inets eunit xmerl

release_major: test
	./bin/release.sh major

release_minor: test
	./bin/release.sh minor

release_patch: test
	./bin/release.sh patch

release: relase_patch
