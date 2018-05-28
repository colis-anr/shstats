
BINDIR=/usr/local/bin

build:
	$(MAKE) -C src
	mkdir -p bin
	cp src/shstats.native bin/shstats

clean:
	$(MAKE) -C src clean
	rm -rf bin

install: build
	mkdir -p $(BINDIR)
	cp bin/shstats $(DESTDIR)/$(BINDIR)/shstats

.PHONY: all clean install
