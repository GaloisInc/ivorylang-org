EXEC=.cabal-sandbox/bin/smaccmpilot-org

default: build

.cabal-sandbox:
	@cabal sandbox init
	@cabal sandbox add-source ./smaccmpilot-org.cabal

.PHONY: smaccmpilot-org
smaccmpilot-org: .cabal-sandbox site.hs Sidebar.hs
	@cabal install
	./$(EXEC) clean

build: smaccmpilot-org
	./$(EXEC) build

preview: build
	./$(EXEC) preview

deploy: build
	./$(EXEC) deploy

clean:
	-./smaccmpilot-org clean
	-rm -rf cabal.sandbox.config .cabal-sandbox
