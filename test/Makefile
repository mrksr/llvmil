
all:
	cat ../runtime/runtime.ll main.ll | llvm-as > test.bc
	llc test.bc -o test.s
	gcc test.s ../runtime/runtime.c -lgc -o test

run:
	./test

clean:
	-rm *.bc *.s
