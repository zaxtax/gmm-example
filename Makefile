all:
	ghc --make -O2 GMMRunner.hs
clean:
	rm *.o *.hi
simplify:
	export LOCAL_MAPLE="`which maple`"
	simplify gmm_gibbs.hk > gmm_gibbs_simp.hk
