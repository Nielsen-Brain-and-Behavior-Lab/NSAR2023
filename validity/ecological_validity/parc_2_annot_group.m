% Wrapper script to turn GROUP-AVERAGED parcellation files into FreeSurfer annotation
% files. 
%
% Assumes ind_parcellation output from Kong2019 CBIG pipeline.
% Written by M. Peterson, Nielsen Brain and Behavior Lab

%To run: 
%	 1. Claim computing resources using salloc (ex: `salloc --mem-per-cpu 6G --time 2:00:00 --x11`)
%    2. Source your CBIG config file to set up CBIG environment.	 
%    3. Load matlab module: `ml matlab/r2018b`
%	 4. Enter the command `LD_PRELOAD= matlab`


%% HCP ALL -1000 ITERATIONS
project_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/generate_profiles_and_ini_params/group';
out_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/MSHBM_GROUP_vis';

if(~exist(out_dir))
        mkdir(out_dir);
end

group_filename = 'group_matched.mat';
file = fullfile(project_dir,group_filename);
lh_name = strcat('sub-GROUP','_lh.annot');
rh_name = strcat('sub-GROUP','_rh.annot');
lh_output_file = fullfile(out_dir,lh_name);
rh_output_file = fullfile(out_dir,rh_name);
CBIG_SaveParcellationToFreesurferAnnotation(file, lh_output_file, rh_output_file);


%% HCP ALL -2000 ITERATIONS
project_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/generate_profiles_and_ini_params/group';
out_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/MSHBM_GROUP_vis2000';

if(~exist(out_dir))
        mkdir(out_dir);
end

group_filename = 'group_2000ini_matched_17N.mat';
file = fullfile(project_dir,group_filename);
lh_name = strcat('sub-GROUP','_lh.annot');
rh_name = strcat('sub-GROUP','_rh.annot');
lh_output_file = fullfile(out_dir,lh_name);
rh_output_file = fullfile(out_dir,rh_name);
CBIG_SaveParcellationToFreesurferAnnotation(file, lh_output_file, rh_output_file);

%% HCP ALL -5000 ITERATIONS
project_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/generate_profiles_and_ini_params/group';
out_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/parc_output_fs6_HCP_ALL/quant_metrics/MSHBM_GROUP_vis5000';

if(~exist(out_dir))
        mkdir(out_dir);
end

group_filename = 'group_5000ini_matched_17N.mat';
file = fullfile(project_dir,group_filename);
lh_name = strcat('sub-GROUP','_lh.annot');
rh_name = strcat('sub-GROUP','_rh.annot');
lh_output_file = fullfile(out_dir,lh_name);
rh_output_file = fullfile(out_dir,rh_name);
CBIG_SaveParcellationToFreesurferAnnotation(file, lh_output_file, rh_output_file);







