all:
	cd 001_prep_pgd    && run_prep_pgd_xyz 
	cd 002_prep_ideal  && run_prep_ideal_case_xyz 
	cd 003_mesonh      && run_mesonh_xyz 
	cd 004_diag        && run_diag_xyz
	cd 005_python      && run_python 

clean:
	cd 001_prep_pgd    && clean_prep_pgd_xyz 
	cd 002_prep_ideal  && clean_prep_ideal_case_xyz 
	cd 003_mesonh      && clean_mesonh_xyz 
	cd 004_diag        && clean_diag_xyz 
	cd 005_python      && clean_python 
