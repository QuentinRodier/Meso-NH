all: E001_pgd1 E002_arp2lfi E003_mesonh E004_diag
ifneq "$(MNH_PYTHON)" "NO"
all: E005_python
endif

E001_pgd1 :
	cd 001_pgd1	       && get_chimie_files
	cd 001_pgd1	       && run_prep_pgd_xyz
E002_arp2lfi :
	cd 002_arp2lfi	       && run_arp2lfi_xyz
E003_mesonh:
	cd 003_mesonh          && run_mesonh_xyz
E004_diag:
	cd 004_diag            && run_diag_xyz
E005_python:
	cd 005_python          && run_python

clean:
	cd 001_pgd1            && clean_prep_pgd_xyz
	cd 002_arp2lfi	       && clean_arp2lfi_xyz
	cd 003_mesonh	       && clean_mesonh_xyz
	cd 004_diag  	       && clean_diag_xyz
        ifneq "$(MNH_PYTHON)" "NO"
	cd 005_python          && clean_python
        endif

python:
	cd 005_python          && run_python
