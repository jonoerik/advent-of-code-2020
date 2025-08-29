GHCWARN = -Wall -Werror

all: day14
.PHONY: all clean
day14: day14.hs

%: %.hs
	ghc $(GHCWARN) -o $@ $^

clean:
	rm -vf ./day14 ./day14.o ./day14.hi ./day14.dyn_o ./day14.dyn_hi
