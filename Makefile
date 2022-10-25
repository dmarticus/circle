build:
	stack build

repl:
	stack repl

test-repl:
	stack repl unknot:test

watch:
	stack build --file-watch --fast

tests:
	stack test
