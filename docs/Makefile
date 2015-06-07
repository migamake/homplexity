all: concepts.svg concepts.png

concepts.png: concepts.dot
	dot concepts.dot -Tpng -o concepts.png

concepts.svg: concepts.dot
	dot concepts.dot -Tsvg -o concepts.svg

view: concepts.svg
	eog concepts.svg
