SRCS=concepts.dot overview.dot metrics.dot legend.dot
TGTS=$(subst .dot,.png,$(SRCS)) $(subst .dot,.svg,$(SRCS))
PROG=dot

all: $(TGTS)

%.png: %.dot
	$(PROG) $< -Tpng -o $@

%.svg: %.dot
	$(PROG) $< -Tsvg -o $@

view: concepts.svg
	eog concepts.svg

.PHONY: clean view all

clean:
	rm -f $(TGTS)

