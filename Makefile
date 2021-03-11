.PHONY: default
default: exe ;

exe:
	rm -f session-counter
	raco exe -o session-counter session-counter.rkt

dist: exe
	raco distribute dist/ session-counter

all: clean dist

clean:
	rm -rf dist/
	rm -rf compiled/
	rm -f session-counter