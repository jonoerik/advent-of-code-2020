COBCWARN = -W

all: day10
.PHONY: all clean
day10: day10.cbl

%: %.cbl
	cobc $(COBCWARN) -free -x $^ -o $@

clean:
	rm -f ./day10
