.PHONY: partial default

tutorial.html: tutorial.lhs .hscolour
	HsColour -html tutorial.lhs > tutorial.html

tutorial-partial.html: tutorial.lhs .hscolour
	HsColour -partial -html tutorial.lhs > tutorial-partial.html

default: tutorial.html
partial: tutorial-partial.html
