C**********************************************************************
      SUBROUTINE BSISUM( NN, NW )
C*****************************                                        *
C* (purpose)                                                          *
C*    Global sum of the integer NN for a parallel processor           *
C*    This program has meaning only for the parallel processor.        *
C*    For the other computers, this must be dummy routine.             *
C* (Argument)                                                         *
C*    NN  : Integer data to be broadcasted                            *
C*    NW  : Number of words (I*4 unit )                               *
C* (Author)                                                           *
C*    S. Kawabata   May 1992                                          *
C**********************************************************************
      IMPLICIT NONE

      INTEGER NW,NN(NW)

      INCLUDE 'bswork.h'

*#ifdef INTEL
*         call gisum( NN, 2, IWORK)
*#elif AP
*         call c2isum( NGOOD  , NEFF(1) , IR )
*         call c2isum( NEGTIV , NEFF(2) , IR )
*#elif NCUBE
*         call isumrn( NN , 2 )
*#endif
      RETURN
      END
