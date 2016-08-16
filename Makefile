
# BUILD_FLAGS=--pedantic --library-profiling --executable-profiling
BUILD_FLAGS=--pedantic

RUN=stack exec -- algo-s

init: stack.yaml

stack.yaml:
	stack init --prefer-nightly

run: build
	$(RUN) --help

docs:
	stack haddock
	open `stack path --local-doc-root`/index.html

package: test
	stack sdist --pvp-bounds both

deploy: test
	stack upload --pvp-bounds both

dev: watch

configure:
	cabal configure \
		--package-db=clear \
		--package-db=global \
		--package-db=`stack path --snapshot-pkg-db` \
		--package-db=`stack path --local-pkg-db`

install:
	stack install

tags: $(SRC)
	codex update

hlint:
	hlint *.hs src specs

clean:
	stack clean
	codex cache clean

distclean: clean
	rm stack.yaml

build:
	stack build $(BUILD_FLAGS)

test:
	stack test $(BUILD_FLAGS)

bench:
	stack bench $(BUILD_FLAGS)

watch:
	stack build $(BUILD_FLAGS) --fast --file-watch

watch-test:
	stack test --file-watch --pedantic # --test-arguments "-m TODO"

restart: distclean init build

rebuild: clean build

.PHONY: init run docs configure install hlint clean distclean build test
.PHONY: bench watch watch-test restart rebuild
