F77      = g77
FFLAGS   = -O -ffixed-line-length-132
PHYSHOME = $(HOME)/Dropbox/Study_D/physlib
LIBPHY   = $(PHYSHOME)/lib_mac
#LIBPHY  = $(PHYSHOME)/lib_heg
INCLUDES = -I./ -I$(PHYSHOME)/inc
LIBDIR   = -L$(LIBPHY)
LIBS     = -lmylib 
OBJS     = runge2.o

.f.o:
	$(F77) $(FFLAGS) $(INCLUDES) -c $<

check: $(OBJS)
	 $(F77) $(FFLAGS) $(LIBDIR) $(OBJS) $(LIBS) -o $@

clean: 
	@rm *.o *~ check *#