#

ROOTCFLAGS = $(shell root-config --cflags)
ROOTLIBS   = $(shell root-config --libs)
CXXFLAGS   = $(ROOTCFLAGS) -Wall
CXXLIBS    = $(ROOTLIBS)

#SRCS	      =	gkps.cxx
SRCS		  = $(wildcard *.cxx)

OBJS 	 	  = ${SRCS:.cxx=.o}

CC	      =	g++

INCLUDES      =	-I.

#FFLAGS        =	-g -O2 $(INCLUDES) \
#		-fno-automatic -finit-local-zero -fugly-complex \
#		-fno-globals -Wno-globals \
#		-fno-second-underscore
FFLAGS        = -Wall -O3 $(INCLUDES) -ffixed-line-length-132


DEST          =	./

INSTALL       =	install

LD            =	g++

LDOPTS        =

PROGRAM	      = LHEAnalysis

SHELL	      =	/bin/sh

ALLDEFINES    =	$(INCLUDES) 

DEPENDFLAGS   = 

DEPEND        =	makedepend

.cxx.o:
		$(CC) -c $(CXXFLAGS) $<

all:	$(PROGRAM)

$(PROGRAM):	$(OBJS) $(LIBS)
	$(LD) $(LDOPTS) $(LIBS) $(CXXLIBS)  $(OBJS) -o $(PROGRAM)

clean:;	@rm -f $(OBJS) $(PROGRAM) last.kumac* core *~ 

.SUFFIXES: .a .o .cxx

