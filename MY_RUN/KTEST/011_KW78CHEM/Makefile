all:
	cd 001_prep_ideal_case && run_prep_ideal_case_xyz
	cd 002_mesonh          && run_mesonh_xyz
	cd 003_diag            && run_diag_xyz
        ifneq "$(MNH_PYTHON)" "NO"
	cd 004_python          && run_python
        endif
	cd 005_ncl             && run_ncl

clean:
	cd 001_prep_ideal_case && clean_prep_ideal_case_xyz
	cd 002_mesonh          && clean_mesonh_xyz
	cd 003_diag            && clean_diag_xyz
        ifneq "$(MNH_PYTHON)" "NO"
	cd 004_python          && clean_python
        endif
	cd 005_ncl             && clean_ncl

python:
	cd 004_python          && run_python
