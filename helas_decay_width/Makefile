OBJS	      = decay_width.o

FC            = g77

INCLUDES      = -I. 

FFLAGS        = -g -O2 $(INCLUDES) \
                -fno-automatic -finit-local-zero -fugly-complex \
                -fno-globals -Wno-globals \
                -fno-second-underscore 

DEST	      = ./

EXTHDRS	      = 

HDRS	      =

INSTALL	      = install

LD	      =  g77

LIBPHY = /home/yoshi/lib


LIBDIR = -L$(LIBPHY)

LDOPTS	      = $(LIBDIR)

LIBS_BASES = -lbases50 -ldhelas3


LIBS     	= $(LIBS_BASES)

PROGRAM       = bases

SRCS	      = decay_width.f

SYSHDRS	      =

.f.o:
	$(FC) -c $(FFLAGS) $<
 
all:    $(PROGRAM)

$(PROGRAM):     $(OBJS) 
	$(LD) $(LDOPTS) $(OBJS) $(LIBS) -o $(PROGRAM)

driver.f: nexternal.inc pmass.inc ngraphs.inc coupl.inc


clean:; @rm -f $(OBJS) $(PROGRAM) last.kumac* core *~ 

.SUFFIXES: .a .o .f
