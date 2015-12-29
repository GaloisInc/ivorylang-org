IVORYLANG_ORG_EXEC?=./.cabal-sandbox/bin/ivorylang-org

default: build

.cabal-sandbox:
	@cabal sandbox init

.PHONY: ivorylang-org
ivorylang-org: .cabal-sandbox site.hs Sidebar.hs
	@cabal install
	$(IVORYLANG_ORG_EXEC) clean

build: ivorylang-org
	$(IVORYLANG_ORG_EXEC) build

preview: build
	$(IVORYLANG_ORG_EXEC) watch

deploy:
	$(IVORYLANG_ORG_EXEC) deploy

clean:
	-$(IVORYLANG_ORG_EXEC) clean
	-rm -rf cabal.sandbox.config .cabal-sandbox
