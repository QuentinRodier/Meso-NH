all: E001_pgd1 E002_pgd2 E003_nest E004_arp2lfi E005_spa_mod1_mod2 E006_preal E008_run2 E009_diag
ifneq "$(MNH_PYTHON)" "NO"
all: E010_python
endif
all: E011_spectre

E001_pgd1 :
	cd 001_pgd1	       && get_pgd_files 
	cd 001_pgd1	       && run_prep_pgd_xyz 
E002_pgd2:
	cd 002_pgd2	       && run_prep_pgd_xyz 
E003_nest:
	cd 003_nest            && run_prep_nest_pgd_xyz
E004_arp2lfi :
	cd 004_arp2lfi	       && run_arp2lfi_xyz
E005_spa_mod1_mod2:
	cd 005_spa_mod1_mod2   && run_spawning_xyz
E006_preal:
	cd 006_preal	       && run_preal_xyz
E008_run2:
	cd 008_run2	       && run_mesonh_xyz
E009_diag:
	cd 009_diag	       && run_diag_xyz
E010_python:
	cd 010_python	       && run_python
E011_spectre:
	cd 011_spectre	       && run_spectre_xyz

clean:
	cd 001_pgd1	       && clean_prep_pgd_xyz
	cd 002_pgd2	       && clean_prep_pgd_xyz
	cd 003_nest            && clean_prep_nest_pgd_xyz
	cd 004_arp2lfi	       && clean_arp2lfi_xyz
	cd 005_spa_mod1_mod2   && clean_spawning_xyz
	cd 006_preal	       && clean_preal_xyz
	cd 008_run2	       && clean_mesonh_xyz
	cd 009_diag	       && clean_diag_xyz
        ifneq "$(MNH_PYTHON)" "NO"
	cd 010_python	       && clean_python
        endif
python:
	cd 010_python          && run_python
