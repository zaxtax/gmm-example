all:
	ghc --make -O2 GMMRunner.hs
clean:
	rm *.o *.hi GMMRunner GmmSweeps
simplify:
	export LOCAL_MAPLE="`which maple`"
	hk-maple -c Simplify --timelimit 300 gmm_gibbs.hk > gmm_gibbs_simp.hk
experiment:
	bash run_sweeps.sh
