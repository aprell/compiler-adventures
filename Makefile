all:
	@dune build

test:
	@test/expect.sh opt -O0
	@test/expect.sh opt -O1
	@test/expect.sh opt -O2
	@test/expect.sh interpret
	@test/expect.sh compile -O0
	@test/expect.sh compile -O1
	@test/expect.sh compile -O2

clean:
	@dune clean

.PHONY: all clean test
