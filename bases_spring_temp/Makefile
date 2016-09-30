FC            = g77

FFLAGS	      = -O3 $(INCLUDES) -ffixed-line-length-132

LIBDIR        = -L./bases50

LDFLAGS       = $(LIBDIR)

LIBS_BASES    = -lbases50

LIBS          = $(LIBS_BASES)

MAKEFILE      = Makefile

OBJS          = bfunc.o userin.o rambo.o

OBJS_BASES    = mainb.o usrout.o $(OBJS)
OBJS_SPRING   = mains.o spevnt.o $(OBJS)

.f.o:
	$(FC) -c $(FFLAGS) $<

bases:  $(OBJS_BASES)
	$(FC) $(LDFLAGS) $(OBJS_BASES) $(LIBS) -o $@

spring:  $(OBJS_SPRING)
	$(FC) $(LDFLAGS) $(OBJS_SPRING) $(LIBS) -o $@

clean:
	@rm *.o *~ *# spring bases