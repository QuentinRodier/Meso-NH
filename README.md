# Meso-NH / EOL : 

- Before the clone : install Large File Storage (LFS)
```
export PATH="/soft/irsrvsoft1/expl/eb/r11/centos_7/easybuild/software/Core/git-lfs/2.11.0/bin:${PATH}"
git lfs install
git lfs fetch --all
```
- Clone with https : 
```
git clone https://gitlab.ifpen.fr/r174/eolien/meso-nh/eol.git
```
- Compile :
```
cd src/
./configure
./job_make_mesonh_pc_ifpen
```
