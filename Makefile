DESCRIPTION = small JSON encoder and decoder

BUILD_DEPS = erl_md
TEST_DEPS = JSONTestSuite

dep_erl_md = git https://github.com/mbj4668/erl_md
dep_JSONTestSuite = git https://github.com/nst/JSONTestSuite

include erl.mk

erl.mk:
	curl -s -O https://raw.githubusercontent.com/mbj4668/erl.mk/main/$@

all: doc/mjson.md

doc/mjson.md: src/mjson.erl
	deps/erl_md/gen-md.sh $< > $@

test: test-roundtrip test-suite

test-roundtrip:
	@test/test-mjson --roundtrip \
	  $(wildcard deps/JSONTestSuite/test_parsing/y_*.json)

test-suite: deps/JSONTestSuite/.patched
	python3 deps/JSONTestSuite/run_tests.py --filter test/mjson.only

deps/JSONTestSuite/.patched:
	cd deps/JSONTestSuite && \
	patch < ../../test/run_tests.py.patch && \
	touch .patched
