
default: build

clean:
	-./site clean
	-rm *.o *.hi site

site: site.hs Sidebar.hs
	ghc --make site.hs
	./site clean

build: site
	./site build

preview: build
	./site preview

