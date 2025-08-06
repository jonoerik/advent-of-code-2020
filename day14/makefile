GHCWARN = -Wall -Werror

all: day14
.PHONY: all clean
day14: day14.hs

%: %.hs
	ghc $(GHCWARN) -o $@ $^

clean:
	rm -f ./day14 ./day14.hi ./day14.o
