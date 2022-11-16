build:
	stack build

repl:
	stack repl

test-repl:
	stack repl circle:test

watch:
	stack build --file-watch --fast

tests:
	stack test
