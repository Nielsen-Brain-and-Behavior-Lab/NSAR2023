# Task Effects Analysis (NSD dataset)

## Description
In the case that a large quantity of data is needed to derive a reliable estimate of specialization, one might consider including task data in addition to any resting-state data in order to increase the amount of available data per participant. However, in this situation it would be prudent to know if task data provides the same or similar estimates as those from resting-state data. To address this concern, the NSD dataset was selected since it has a large quantity of both resting-state and task-based fMRI data per participant. Following preprocessing, a minimum of 12 resting-state runs were available for each participant, so the first 12 available resting-state runs and the first 12 available task runs were utilized (resting-state and task runs were of the same duration). Individual parcellations were then generated based on various combinations of runs within task type: even-numbered runs, odd-numbered runs, the first half of runs, the second half of runs, and two random selections of runs (without replacement). A dice coefficient was then computed to compare parcellation label overlap within task (e.g., between even and odd-numbered resting-state runs) and between tasks (e.g., between odd-numbered runs from resting-state and task runs). This comparison procedure was repeated for the NSAR intraclass correlation coefficient.

## Contents
1. Example network parcellation scripts
2. Example network surface area script
3. Example dice coefficient script
