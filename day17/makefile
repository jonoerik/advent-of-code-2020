GHCWARN = -Wall -Werror

all: day17
.PHONY: all clean
day17: day17.hs

%: %.hs
	ghc $(GHCWARN) -o $@ $^

clean:
	rm -vf ./day17 ./day17.o ./day17.hi ./day17.dyn_o ./day17.dyn_hi
