% Purpose: Convert lh_labels and rh_labels to gifti shape files for later WB
% visualization
% Inputs: lh_labels and rh_labels from generate_network_overlap.m and template gifti
% files in appropriate resolution. 
% Outputs: .shape.gii files containing AI values.
%
% Note: Template .shape.gii files can be created using mris_vol2surf in
% combination with bbregister. See the following script: project_surface_FS_AVG_FS6.sh
%
% Written by M. Peterson, Nielsen Brain and Behavior Lab, under MIT License 2022

% To run: 
%	 1. Claim computing resources using salloc (ex: `salloc --mem-per-cpu 20G --time 48:00:00 --x11`)
%	 2. Load matlab module: `ml matlab/r2018b`
%	 3. Enter the command `LD_PRELOAD= matlab`
%%

%GROUP
group = 'HCPD';

% Set paths and variables
out_dir = strcat('/fslgroup/fslg_HBN_preproc/compute/HCPD_analysis/parc_output_fs6_HCPD_REST/quant_metrics/network_overlap/network_overlap_output/', group);
gifti_template_dir = '/fslgroup/fslg_spec_networks/compute/results/fsaverage_surfaces';

% Loop through each network
for network = 1:17
    
    %load overlap data
    infile = strcat(group, '_NETWORK_', num2str(network), '_FS6_OVERLAP.mat');
    inputfull = fullfile(out_dir, infile);
    if isfile(inputfull)
        load(inputfull)
    
        %grab resolution
        resolution = size(overlap_lh_labels,1);
          
        %output filenames
        fname_lh = strcat(group, '_NETWORK_', num2str(network), '_FS6_OVERLAP_lh.shape.gii');
        fname_rh = strcat(group, '_NETWORK_', num2str(network), '_FS6_OVERLAP_rh.shape.gii');
        full_lh = fullfile(out_dir, fname_lh);
        full_rh = fullfile(out_dir, fname_rh);
    
        %load in template .shape.gii file 
        g_left = gifti(fullfile(gifti_template_dir, 'FS6_lh.shape.gii'));
        g_right = gifti(fullfile(gifti_template_dir, 'FS6_rh.shape.gii'));
    
        %replace vertex values in templates with overlap values
        metric = single(ones(resolution, 1));
        g_left.cdata = metric;
        g_right.cdata = metric;
        g_left.cdata = overlap_lh_labels;
        g_right.cdata = overlap_rh_labels;
    
        %save output
        save(g_left, char(full_lh));
        save(g_right, char(full_rh));
    else
    end
end
