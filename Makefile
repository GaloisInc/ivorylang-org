EXEC=.cabal-sandbox/bin/ivorylang-org

default: build

.cabal-sandbox:
	@cabal sandbox init

.PHONY: ivorylang-org
ivorylang-org: .cabal-sandbox site.hs Sidebar.hs
	@cabal -j1 install
	./$(EXEC) clean

build: ivorylang-org
	./$(EXEC) build

preview: build
	./$(EXEC) watch

deploy:
	./$(EXEC) deploy

clean:
	-./ivorylang-org clean
	-rm -rf cabal.sandbox.config .cabal-sandbox
