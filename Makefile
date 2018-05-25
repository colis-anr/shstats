.PHONY: build debug bench clean install
BINDIR=/usr/local/bin

build:
	$(MAKE) -C src

bench:
	$(MAKE) -C bench

clean:
	$(MAKE) -C src clean
	$(MAKE) -C bench clean
	rm -f bin/shparser
	[ ! -d bin ] || rmdir bin

install: build
	mkdir -p $(BINDIR)
	cp src/shstats.native $(DESTDIR)/$(BINDIR)/shstats


