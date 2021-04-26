all:
	cd 001_prep_pgd	       && get_pgd_files 
	cd 001_prep_pgd        && run_prep_pgd_xyz 
	cd 002_prep_ideal_case && run_prep_ideal_case_xyz 
	cd 003_mesonh          && run_mesonh_xyz 
	cd 004_diag            && run_diag_xyz 
	cd 006_ncl             && run_ncl
        ifneq "$(MNH_PYTHON)" "NO"
	cd 007_python          && run_python
        endif

all_ncl_lfi2cdf:
	cd 001_prep_pgd	       && get_pgd_files 
	cd 001_prep_pgd        && run_prep_pgd_xyz 
	cd 002_prep_ideal_case && run_prep_ideal_case_xyz 
	cd 003_mesonh          && run_mesonh_xyz 
	cd 005_ncl_lfi2cdf     && run_ncl 

all_ncl_extractdia:
	cd 001_prep_pgd	       && get_pgd_files 
	cd 001_prep_pgd        && run_prep_pgd_xyz 
	cd 002_prep_ideal_case && run_prep_ideal_case_xyz 
	cd 003_mesonh          && run_mesonh_xyz 
	cd 005_ncl_extractdia  && run_ncl 

all_ncl_nc4:
	cd 001_prep_pgd	       && get_pgd_files 
	cd 001_prep_pgd        && run_prep_pgd_xyz 
	cd 002_prep_ideal_case && run_prep_ideal_case_xyz 
	cd 003_mesonh          && run_mesonh_xyz 
	cd 005_ncl_nc4  && run_ncl 
	cd 006_ncl             && run_ncl

clean:
	cd 001_prep_pgd	       && clean_get_pgd_files 
	cd 001_prep_pgd        && clean_prep_pgd_xyz 
	cd 002_prep_ideal_case && clean_prep_ideal_case_xyz 
	cd 003_mesonh          && clean_mesonh_xyz 
	cd 004_diag            && clean_diag_xyz 
	cd 005_ncl_lfi2cdf     && clean_ncl 
	cd 005_ncl_extractdia  && clean_ncl 
	cd 005_ncl_nc4         && clean_ncl 
	cd 006_ncl             && clean_ncl
        ifneq "$(MNH_PYTHON)" "NO"
	cd 007_python          && clean_python
        endif
