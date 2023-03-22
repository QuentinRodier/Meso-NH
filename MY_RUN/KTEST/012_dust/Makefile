all: E001_pgd1  E002_ecmwf2lfi E003_run E004_restart
ifneq "$(MNH_PYTHON)" "NO"
all: E005_python
 endif

E001_pgd1 :
	cd 001_pgd1	       && run_prep_pgd_xyz 
E002_ecmwf2lfi :
	cd 002_ecmwf2lfi       && run_ecmwf2lfi_xyz
E003_run:
	cd 003_run	       && run_mesonh_xyz
E004_restart:
	cd 004_restart	       && run_mesonh_xyz
E005_python:
	cd 005_python	       && run_python
clean:
	cd 001_pgd1	       && clean_prep_pgd_xyz
	cd 002_ecmwf2lfi       && clean_ecmwf2lfi_xyz
	cd 003_run	       && clean_mesonh_xyz
	cd 004_restart	       && clean_mesonh_xyz
        ifneq "$(MNH_PYTHON)" "NO"
	cd 005_python	       && clean_python
        endif

python:
	cd 005_python          && run_python
