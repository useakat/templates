F77      = g77
FFLAGS   = -O -ffixed-line-length-132
LIBPHY   = $(LIB)
INCLUDES = -I./ -I$(PHYSLIB)/inc
LIBDIR   = -L$(LIBPHY)
LIBS     = -lmylib
OBJS     = matrix.o

.f.o:
	$(F77) $(FFLAGS) $(INCLUDES) -c $<

check: $(OBJS)
	 $(F77) $(FFLAGS) $(LIBDIR) $(OBJS) $(LIBS) -o $@

clean: 
	@rm *.o *~ *#
