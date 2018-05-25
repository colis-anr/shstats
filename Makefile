
BINDIR=/usr/local/bin

build:
	$(MAKE) -C src

clean:
	$(MAKE) -C src clean

install: build
	mkdir -p $(BINDIR)
	cp src/shstats.native $(DESTDIR)/$(BINDIR)/shstats

.PHONY: all clean install
