#!/bin/bash
#Purpose: transform LanA atlas from fsaverage nifti to fsaverage6 gifti files
#Notes: Gifti files were created through two steps. 1. Conversion to fsaverage6 space. 2. Conversion to gifti format.
#Written by M. Peterson, Nielsen Brain and Behavior Lab under MIT License 2023

#Conversion from fsaverage to fsaverage6
mri_surf2surf --srcsubject fsaverage --trgsubject fsaverage6 --hemi rh --sval LH_LanA_n804.nii.gz --tval LH_LanA_n804_fs6.nii.gz
mri_surf2surf --srcsubject fsaverage --trgsubject fsaverage6 --hemi rh --sval RH_LanA_n804.nii.gz --tval RH_LanA_n804_fs6.nii.gz

#Conversion from nifti to gifti
mri_convert LH_LanA_n804_fs6.nii.gz LH_LanA_n804.shape.gii
mri_convert RH_LanA_n804_fs6.nii.gz RH_LanA_n804.shape.gii
