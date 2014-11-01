DOC=ivory-rust

default: $(DOC).pdf

$(DOC).pdf: $(DOC).md
	pandoc $< -o $@

.PHONY: clean
clean:
	-rm -f $(DOC).pdf
