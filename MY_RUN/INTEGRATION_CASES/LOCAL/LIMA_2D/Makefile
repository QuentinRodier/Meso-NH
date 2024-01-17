all:
	cd 001_prep_ideal_case && run_prep_ideal_case_xyz
	cd 002_mesonh_LSPRO    && run_mesonh_xyz
	cd 002_mesonh_NOLADJ     && run_mesonh_xyz
	cd 004_python          && run_python

clean:
	cd 001_prep_ideal_case && clean_prep_ideal_case_xyz
	cd 002_mesonh_LSPRO    && clean_mesonh_xy
	cd 002_mesonh_NOLADJ     && clean_mesonh_xyz
	cd 004_python          && clean_python
