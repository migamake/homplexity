all: concepts.svg

concepts.svg: concepts.dot
	dot concepts.dot -Tsvg -o concepts.svg

view: concepts.svg
	eog concepts.svg
