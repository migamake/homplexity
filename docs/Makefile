SRCS=concepts.dot legend.dot
TGTS=$(subst .dot,.png,$(SRCS)) $(subst .dot,.svg,$(SRCS))

all: $(TGTS)

%.png: %.dot
	dot $< -Tpng -o $@

%.svg: %.dot
	dot $< -Tsvg -o $@

view: concepts.svg
	eog concepts.svg

.PHONY: clean view all

clean:
	rm -f $(TGTS)

