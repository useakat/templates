CC = cc

CFLAGS = -O

FC = g77
#FC	= gfortran

HOME = /Users/yoshi/Documents/Study/phys

INCLUDES = -I./ -I$(HOME)/hml/inc -I./HELAS

FFLAGS = -O3 $(INCLUDES)

#LD	=  gfortran
LD =  g77

LDFLAGS = -L$(HOME)/lib

LIBS = -lhml -lhmlutil -ldhelas31 -lbases50 -llibrary

MAKEFILE = Makefile

OBJS = bfunc.o userin.o opendata.o pdf.o pdfwrap.o Ctq6Pdf.o coupsm.o matrix.o

OBJS_BASES = mainb.o usrout.o $(OBJS)

.f.o:
	$(FC) -c $(FFLAGS) $<

bases:  $(OBJS_BASES)
	$(LD) $(LDFLAGS) $(OBJS_BASES) $(LIBS) -o $@

clean:
	@rm *.o
###
