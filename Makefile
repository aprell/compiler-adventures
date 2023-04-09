all:
	@dune build

test:
	@dune test

clean:
	@dune clean

.PHONY: all clean test
