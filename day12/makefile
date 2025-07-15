COBCWARN = -W

all: day12
.PHONY: all clean
day12: day12.cbl

%: %.cbl
	cobc $(COBCWARN) -free -x $^ -o $@

clean:
	rm -f ./day12
