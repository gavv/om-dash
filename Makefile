TERM := xterm
export TERM

all: build lint

deps:
	eask install-deps --dev

build:
	eask compile

lint:
	eask lint elisp-lint --strict
	eask lint keywords --strict
	eask lint regexps --strict

clean:
	rm -f .eask
	rm *.elc
