CC            = cc

CFLAGS        = -O

#FC            = fort77
FC            = g77
#FC            = gfortran

#FFLAGS        = -O3 +ppu +FPD  -I./
#FFLAGS        = -O -a +ppu $(INCDIR)
FFLAGS	       = -g -O2 -I. \
		-fno-automatic -finit-local-zero -fugly-complex \
		-fno-globals -Wno-globals \
		-fno-second-underscore


LD	      = ld

DEST	      = ../lib

EXTHDRS	      =

HDRS	      =

INSTALL	      = install

LIBRARY	      = libbases50.a

MAKEFILE      = Makefile

OBJS	      = bases.o \
		bhinit.o \
		bhplot.o \
		bhrset.o \
		bhsave.o \
		bhsum.o \
		bscast.o \
		bschck.o \
		bsdate.o \
		bsdsum.o \
		bsetgu.o \
		bsetgv.o \
		bsgeti.o \
		bsinfo.o \
		bsinit.o \
		bsintg.o \
		bsisum.o \
		bslist.o \
		bsordr.o \
		bsprnt.o \
		bsread.o \
		bssetd.o \
		bssetp.o \
		bstcnv.o \
		bstime.o \
		bsunix.o \
		bsutim.o \
		bswrit.o \
		dhfill.o \
		dhinit.o \
		dhplot.o \
		drloop.o \
		drn.o \
		shcler.o \
		shfill.o \
		shinit.o \
		shplot.o \
		shrset.o \
		shupdt.o \
		spchck.o \
		sphist.o \
		spinfo.o \
		sprgen.o \
		spring.o \
		xhchck.o \
		xhfill.o \
		xhgrid.o \
		xhinit.o \
		xhordr.o \
		xhplot.o \
		xhrnge.o \
		xhsave.o \
		xhscle.o

PRINT	      = pr

SHELL	      = /bin/sh

SRCS	      = bases.f \
		bhinit.f \
		bhplot.f \
		bhrset.f \
		bhsave.f \
		bhsum.f \
		bscast.f \
		bschck.f \
		bsdate.f \
		bsdsum.f \
		bsetgu.f \
		bsetgv.f \
		bsgeti.f \
		bsinfo.f \
		bsinit.f \
		bsintg.f \
		bsisum.f \
		bslist.f \
		bsordr.f \
		bsprnt.f \
		bsread.f \
		bssetd.f \
		bssetp.f \
		bstcnv.f \
		bstime.f \
		bsunix.f \
		bsutim.f \
		bswrit.f \
		dhfill.f \
		dhinit.f \
		dhplot.f \
		drloop.f \
		drn.f \
		shcler.f \
		shfill.f \
		shinit.f \
		shplot.f \
		shrset.f \
		shupdt.f \
		spchck.f \
		sphist.f \
		spinfo.f \
		sprgen.f \
		spring.f \
		xhchck.f \
		xhfill.f \
		xhgrid.f \
		xhinit.f \
		xhordr.f \
		xhplot.f \
		xhrnge.f \
		xhsave.f \
		xhscle.f

SYSHDRS	      =

.f.o:
	$(FC) $(FFLAGS) -c $<

all::           $(LIBRARY)

#all::		install

$(LIBRARY):	$(OBJS)
		@echo  "Loading $(LIBRARY) ... "
		@ar cru $(LIBRARY) $(OBJS)
		@echo "done"

clean:;		@rm -f $(OBJS) core *~ *.a

clobber:;	@rm -f $(OBJS) $(LIBRARY) core tags

depend:;	@mkmf -f $(MAKEFILE) LIBRARY=$(LIBRARY) DEST=$(DEST)

echo:;		@echo $(HDRS) $(SRCS)

extract:;	@ar x $(DEST)/$(LIBRARY)

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(LIBRARY)
	        @echo Installing $(LIBRARY) in $(DEST)
	        @if [ $(DEST) != . ]; then \
	        (rm -f $(DEST)/$(LIBRARY); $(INSTALL) -c $(LIBRARY) $(DEST)); fi

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:         $(DEST)/$(LIBRARY)

$(DEST)/$(LIBRARY): $(SRCS) $(HDRS) $(EXTHDRS)
	        @$(MAKE) -f $(MAKEFILE) ROOT=$(ROOT) DEST=$(DEST) install
