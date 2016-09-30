      double precision ptcut
      parameter (ptcut = 20.d0)
      double precision etacut
      parameter (etacut = 2.5d0)
c      double precision drcut
c      parameter (drcut = 0.4d0)
        
      double precision whatmin, taulmin
      common /ccuts/ whatmin, taulmin
      save /ccuts/
