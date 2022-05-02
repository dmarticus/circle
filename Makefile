build:
	stack build

repl:
	stack repl

test-repl:
	stack repl unknot:tests

watch:
	stack build --file-watch --fast

test:
	stack test
