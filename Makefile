PREFIX ?= /usr/local

all:
	@echo "Usage: make (install|uninstall)"

.PHONY: all install uninstall

install:
	install -Dm 0755 -T xdg-ninja.sh '$(DESTDIR)$(PREFIX)/bin/xdg-ninja'
	install -d '$(DESTDIR)$(PREFIX)/share/xdg-ninja/'
	cp -r programs '$(DESTDIR)$(PREFIX)/share/xdg-ninja/'
	install -Dm 0644 -t '$(DESTDIR)$(PREFIX)/share/doc/xdg-ninja/' LICENSE README.md
	install -Dm 0644 -t '$(DESTDIR)$(PREFIX)/share/man/man1/' man/xdg-ninja.1

uninstall:
	rm -rf '$(DESTDIR)$(PREFIX)/bin/xdg-ninja' \
	       '$(DESTDIR)$(PREFIX)/share/xdg-ninja' \
	       '$(DESTDIR)$(PREFIX)/share/doc/xdg-ninja' \
	       '$(DESTDIR)$(PREFIX)/share/man/man1/xdg-ninja.1'
