DC=gdc
DCFLAGS=-Wall -Werror
DCTESTFLAG=--unittest

all: day03
.PHONY: clean all check

day03: day03.d
	$(DC) $(DCFLAGS) -o $@ $^

day03.unittest: day03.d
	$(DC) $(DCFLAGS) $(DCTESTFLAG) -o $@ $^

check: day03.unittest
	./day03.unittest

clean:
	rm -f ./day03 ./day03.unittest
