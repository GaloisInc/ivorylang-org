
default: build

clean:
	./site clean
	-rm site.o site.hi site

site: site.hs
	ghc --make site.hs
	./site clean

build: site
	./site build

preview: build
	./site preview

