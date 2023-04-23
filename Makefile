PREFIX ?= /usr/local

all:
	@echo "Usage: make (install|uninstall)"

.PHONY: all install uninstall

install:
	install -Dm 0755 xdg-ninja.sh $(DESTDIR)$(PREFIX)/bin/xdg-ninja
	install -d $(DESTDIR)$(PREFIX)/share/xdg-ninja/
	cp -r programs $(DESTDIR)$(PREFIX)/share/xdg-ninja/
	install -d $(DESTDIR)$(PREFIX)/share/doc/xdg-ninja/
	install -m 0644 LICENSE README.md $(DESTDIR)$(PREFIX)/share/doc/xdg-ninja/

uninstall:
	rm -rf $(DESTDIR)$(PREFIX)/bin/xdg-ninja \
	       $(DESTDIR)$(PREFIX)/share/xdg-ninja \
	       $(DESTDIR)$(PREFIX)/share/doc/xdg-ninja
