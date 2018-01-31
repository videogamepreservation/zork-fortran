# Makefile for creating dungeon

# 20121204 bkw: modified for use with gnu fortran 4.7.x

ifeq ($(DEBUG),1)
GDT = gdt.for
DLINE = -fd-lines-as-code
else
DLINE = -fd-lines-as-comments
endif

FORS =  actors.for ballop.for clockr.for demons.for\
	dgame.for dinit.for dmain.for dso1.for dso2.for\
	dso3.for dso4.for dso5.for dso6.for dso7.for\
	dsub.for dverb1.for dverb2.for lightp.for\
	nobjs.for np.for np2.for np3.for nrooms.for objcts.for\
	rooms.for sobjs.for sverbs.for verbs.for villns.for\
	np1.for blkdata.for rtim.for $(GDT)

dungeon: $(FORS)
	g77 $(FFLAGS) -fbackslash $(DLINE) -o dungeon $(FORS)

clean:
	rm -f dungeon

install: dungeon
	mkdir -p $(DESTDIR)/usr/bin $(DESTDIR)/usr/share/games/dungeon
	install -s dungeon $(DESTDIR)/usr/bin
	install *.dat $(DESTDIR)/usr/share/games/dungeon
