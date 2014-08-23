EXEC=.cabal-sandbox/bin/smaccmpilot-org

default: build

.cabal-sandbox:
	@cabal sandbox init

.PHONY: smaccmpilot-org
smaccmpilot-org: .cabal-sandbox site.hs Sidebar.hs
	@cabal -j1 install
	./$(EXEC) clean

build: smaccmpilot-org
	./$(EXEC) build

preview: build
	./$(EXEC) watch

deploy:
	./$(EXEC) deploy

clean:
	-./smaccmpilot-org clean
	-rm -rf cabal.sandbox.config .cabal-sandbox
