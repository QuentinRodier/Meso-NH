all:
	cd 001_prep_ideal      && run_prep_ideal_case_xyz
	cd 002_mesonh          && run_mesonh_xyz
	cd 003_diag            && run_diag_xyz
	cd 004_convdia         && run_conv2dia
	cd 005_diaprog         && run_diaprog
	cd 006_plotresults     && run_plotresults

clean:
	cd 001_prep_ideal      && clean_prep_ideal_case_xyz
	cd 002_mesonh          && clean_mesonh_xyz
	cd 003_diag            && clean_diag_xyz
	cd 004_convdia         && clean_conv2dia
	cd 006_plotresults     && clean_plotresults
