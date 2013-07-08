EXEC = cabal-dev/bin/smaccmpilot-org

default: build

clean:
	-./smaccmpilot-org clean
	-rm -rf cabal-dev

smaccmpilot-org: site.hs Sidebar.hs
	cabal-dev install
	./$(EXEC) clean

build: smaccmpilot-org
	./$(EXEC) build

preview: build
	./$(EXEC) preview

deploy: build
	./$(EXEC) deploy
