% Purpose: Calculate network for each MS-HBM network
% Inputs: Hungarian-matched individual network parcellations
% Outputs: rh_labels and lh_labels containing overlap values for
% each network
%
% Written by M. Peterson, Nielsen Brain and Behavior Lab, under MIT License
% 2024

% To run: 
%	 1. Claim computing resources using salloc (ex: `salloc --mem-per-cpu 30G --time 48:00:00 --x11`)
%	 2. Load matlab module: `ml matlab/r2018b`
%	 3. Enter the command `LD_PRELOAD= matlab`
%%

%GROUP
group = 'HCP_DISC';

% Set paths
project_dir = '/fslgroup/grp_hcp/compute/HCP_analysis/DISC_AND_REP/ind_parcellation';
out_dir = strcat('/fslgroup/grp_hcp/compute/HCP_analysis/DISC_AND_REP/network_overlap_output/', group);

%HCP- DISC
   filename = '/fslgroup/fslg_spec_networks/compute/code/HCP_analysis/network_overlap_fig/hcp_disc/study3_HCPDISC_IDS_230911.txt'; 
   delimiter = {''};
   formatSpec = '%s%[^\n\r]';
   fileID = fopen(filename,'r');
   dataArray = textscan(fileID, formatSpec, 'Delimiter', delimiter, 'TextType', 'string',  'ReturnOnError', false);
   fclose(fileID);
   subjids = [dataArray{1:end-1}];
   clearvars filename delimiter formatSpec fileID dataArray ans;


% Loop through each network
for network = 1:17
    % Initialize vectors to store aggregated labels for each network
    overlap_lh_labels = zeros(40962, 1);
    overlap_rh_labels = zeros(40962, 1);
    
    
    %Loop through each subject in group list
    for i = 1:length(subjids)
    sub=subjids(i);    


	%load individual parc
    parc_file = strcat('Ind_parcellation_MSHBM_sub', sub, '_w200_MRF30_matched.mat');
    parc_full = fullfile(project_dir, parc_file);
    load(parc_full)
  
        
    % Set all elements in the vector to 0 except those that match network
    lh_labels(lh_labels ~= network) = 0;
    rh_labels(rh_labels ~= network) = 0;
    
    % Turn just the elements that match network N to 1
    lh_labels(lh_labels == network) = 1;
    rh_labels(rh_labels == network) =1;
  
    % Update aggregated labels for network N
    overlap_lh_labels = overlap_lh_labels + lh_labels;
    overlap_rh_labels = overlap_rh_labels + rh_labels;

    %Clear out variable that changes on a subject basis
    clear rh_labels;
    clear lh_labels;
    
    end

    
%write overlap output as overlap_rh_labels and overlap_lh_labels in a .mat file
    if(~exist(out_dir))
        mkdir(out_dir);
    end
    
 save(fullfile(out_dir, strcat(group, '_NETWORK_', num2str(network), '_FS6_OVERLAP.mat')), 'overlap_lh_labels','overlap_rh_labels');

end