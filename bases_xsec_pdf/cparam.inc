	integer nini, nfin, nexternal
	parameter (nini = 2, nfin = 2, nexternal=nini+nfin)
	integer ncolor, nwavefuncs
	parameter (ncolor = 6, nwavefuncs = 7)
	integer ngraphs
	parameter (ngraphs = 6)

	integer ndim
	parameter (ndim = 3*nfin - 2 + 1)
	integer nwild
	parameter (nwild = ndim)
