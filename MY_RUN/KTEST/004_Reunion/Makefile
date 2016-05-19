all:
	cd 001_prep_pgd	       && get_pgd_files 
	cd 001_prep_pgd        && run_prep_pgd_xyz 
	cd 002_prep_ideal_case && run_prep_ideal_case_xyz 
	cd 003_mesonh          && run_mesonh_xyz 
	cd 004_convdia         && run_conv2dia 
	cd 005_diaprog         && run_diaprog 

all_ncl_lfi2cdf:
	cd 001_prep_pgd	       && get_pgd_files 
	cd 001_prep_pgd        && run_prep_pgd_xyz 
	cd 002_prep_ideal_case && run_prep_ideal_case_xyz 
	cd 003_mesonh          && run_mesonh_xyz 
	cd 004_convdia         && run_conv2dia 
	cd 005_ncl_lfi2cdf     && run_ncl 

all_ncl_extractdia:
	cd 001_prep_pgd	       && get_pgd_files 
	cd 001_prep_pgd        && run_prep_pgd_xyz 
	cd 002_prep_ideal_case && run_prep_ideal_case_xyz 
	cd 003_mesonh          && run_mesonh_xyz 
	cd 004_convdia         && run_conv2dia 
	cd 005_ncl_extractdia  && run_ncl 
clean:
	cd 001_prep_pgd	       && clean_get_pgd_files 
	cd 001_prep_pgd        && clean_prep_pgd_xyz 
	cd 002_prep_ideal_case && clean_prep_ideal_case_xyz 
	cd 003_mesonh          && clean_mesonh_xyz 
	cd 004_convdia         && clean_conv2dia 
	cd 005_diaprog         && clean_diaprog 
	cd 005_ncl_lfi2cdf     && clean_ncl 
	cd 005_ncl_extractdia  && clean_ncl 
