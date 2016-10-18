# ledgerexport-tax
# See LICENSE file for copyright and license details.

# Usage:
# ------
# make ledgerexport-tax
# As root: make install
# make clean
# To remove:
# As root: make uninstall

include config.mk

SRC = ledgerexport-tax.lisp ledgerexport-tax.asd package.lisp
TARGET = ledgerexport-tax
# OPTS = 

all: options ${TARGET}

options:
	@echo ledgerexport-tax build options:
	@echo "--entry main"
	
$(TARGET): 
	@echo ${CC} --entry main --output ${TARGET}
	@${CC} --entry main --output ${TARGET} 
   
clean:
	@echo cleaning...
	@rm -fv ${TARGET} ${TARGET}-${VERSION}.tar.gz
	
dist: clean
	@echo creating dist tarball
	@mkdir -p ${TARGET}-${VERSION}
	@cp -R LICENSE.txt Makefile config.mk README.adoc \
		${TARGET}.1 ${SRC} ${TARGET}-${VERSION}
	@tar -cf ${TARGET}-${VERSION}.tar ${TARGET}-${VERSION}
	@gzip ${TARGET}-${VERSION}.tar
	@rm -rf ${TARGET}-${VERSION}

install: all
	@echo installing application to ${DESTDIR}${PREFIX}/bin
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@cp -f ${TARGET} ${DESTDIR}${PREFIX}/bin
	@chmod 755 ${DESTDIR}${PREFIX}/bin/${TARGET}
	@echo Generating man page, using asciidoc:
	@echo a2x --doctype=manpage --format=manpage ${TARGET}.1.adoc
	@a2x --doctype=manpage --format=manpage ${TARGET}.1.adoc
	@echo installing manual page to ${DESTDIR}${MANPREFIX}/man1
	@mkdir -p ${DESTDIR}${MANPREFIX}/man1
	@sed "s/VERSION/${VERSION}/g" < ${TARGET}.1 > ${DESTDIR}${MANPREFIX}/man1/${TARGET}.1
	@chmod 644 ${DESTDIR}${MANPREFIX}/man1/${TARGET}.1
	
uninstall:
	@echo removing application from ${DESTDIR}${PREFIX}/bin
	@rm -f ${DESTDIR}${PREFIX}/bin/${TARGET}
	@echo removing manual page from ${DESTDIR}${MANPREFIX}/man1
	@rm -f ${DESTDIR}${MANPREFIX}/man1/${TARGET}.1
	
.PHONY: all options clean dist install uninstall
