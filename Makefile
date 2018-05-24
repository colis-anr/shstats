
build:
	jbuilder build @install

bench: build
	make -C bench

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean
	find -name '*~' -delete

.PHONY: build install uninstall clean
