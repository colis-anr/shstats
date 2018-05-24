.PHONY: all debug bench clean

all:
	$(MAKE) -C src

bench:
	$(MAKE) -C bench

clean:
	$(MAKE) -C src clean
	$(MAKE) -C bench clean
	rm -f bin/shparser
	[ ! -d bin ] || rmdir bin
