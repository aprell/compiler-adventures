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
	@test/expect.sh compile_and_run -O0
	@test/expect.sh compile_and_run -O1
	@test/expect.sh compile_and_run -O2
	@diff -u --color=always test/compile_and_run-O0.expected test/compile_and_run-O1.expected
	@diff -u --color=always test/compile_and_run-O1.expected test/compile_and_run-O2.expected

clean:
	@dune clean

.PHONY: all clean test
