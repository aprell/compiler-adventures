all:
	@dune build

test:
	@test/expect.sh opt -O0
	@test/expect.sh opt -O1
	@test/expect.sh opt -O2
	@test/expect.sh interpret

clean:
	@dune clean

.PHONY: all clean test
