all:
	cd 001_prep_ideal_case && run_prep_ideal_case_xyz
	cd 002_mesonh          && run_mesonh_xyz
ifneq "$(MNH_PYTHON)" "NO"
	cd 003_python          && run_python
endif

clean:
	cd 001_prep_ideal_case && clean_prep_ideal_case_xyz
	cd 002_mesonh          && clean_mesonh_xyz
ifneq "$(MNH_PYTHON)" "NO"
	cd 003_python          && clean_python
endif

python:
	cd 003_python          && run_python
