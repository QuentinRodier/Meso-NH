all:
	cd 1_input_mnh/   && get_pgd_files
	cd 1_input_mnh/   && run_prep_mesonh_xyz
	cd 2_input_toy/   && run_prep_toy
	cd A_cpl_mnh_toy/ && run_mesonh_xyz
	cd A_cpl_mnh_toy/ && run_plot
clean:
	cd 1_input_mnh/   && clean_prep_mesonh_xyz
	cd 2_input_toy/   && clean_prep_toy
	cd A_cpl_mnh_toy/ && clean_mesonh_xyz
	cd A_cpl_mnh_toy/ && clean_plot
