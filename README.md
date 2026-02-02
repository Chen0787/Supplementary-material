# L2 Reading Time Data: VP Ellipsis and UID Hypothesis

## Overview

This repository provides datasets and R scripts for analyzing self-paced reading times from Chinese EFL learners, investigating verb phrase (VP) repetition and ellipsis, adapted from Schäfer et al. (2021).  

Data include word-level and VP-region reading times, trial information, and participant-level measures (working memory, L2 proficiency). Scripts reproduce residualized reading time analyses and generate publication-ready figures.

For full details, see:  
**"Testing Uniform Information Density in Second Language Comprehension: Evidence from Verb Phrase Ellipsis."**

## Repository Contents

### Data
- `/data/experiment1_data.csv` — Experiment 1 raw data (anonymized)  
- `/data/experiment2_data.csv` — Experiment 2 raw data (anonymized)  

### Scripts
- `/scripts/experiment2_analysis.R` — R script for Experiment 2 analyses  

### Materials
- `experimental_materials.doc` — Experimental stimuli for both experiments, adapted from Schäfer et al. (2021)  

### Documentation
- `README.md` — This file

## Experimental Design and Materials

- Stimuli and design adapted from Schäfer et al. (2021).  
- Sentences contain **long or short VPs**.  
- Reading times recorded **word-by-word** in a self-paced reading paradigm.  
- **Comprehension questions** follow each sentence.  
- Data include:  
  - Word-level RTs (`RT1`–`RT24`)  
  - VP-region RTs (`VP1_RT(ms)`, `VP2_RT(ms)`)  
  - VP length (`Long_LEN`, `Short_LEN`, `VP_LEN`)  
  - L2 proficiency (`lextale`)  
  - Working memory (`working_memory`)  
  - Trial info (`subject`, `trial_id`, `type`, `sentence`, `question`)  
  - Responses and accuracy (`response`, `is_correct`)  

> Materials preserve consistency with Schäfer et al. (2021) while adding individual difference measures for L2 processing.

## Data Description

- `experiment1_data.csv` — Experiment 1 data (anonymized)  
- `experiment2_data.csv` — Experiment 2 data (anonymized)  
- Participant identifiers removed or pseudonymized.  
- Column names match the R scripts; ensure correct variable references in analyses.

## Analysis Script

`experiment2_analysis.R` performs:  
1. Data cleaning and outlier removal  
2. Residualization of reading times by VP length  
3. Mixed-effects modeling (`lme4`, `lmerTest`)  
4. Generation of publication-ready visualizations  

Required R packages: `readxl`, `dplyr`, `tidyr`, `lme4`, `lmerTest`, `emmeans`, `ggplot2`, `ggeffects`, `showtext`, `ragg`, `car`.

## Usage

1. Clone or download the repository.  
2. Open `experiment2_analysis.R` in R/RStudio.  
3. Adjust input file paths if necessary.  
4. Run the script to reproduce analyses and figures.  
5. Figures are saved as high-resolution `.jpg` files.

## Privacy and Ethics

- All data are anonymized; subject identifiers (`subject_name`) removed or pseudonymized.  
- Only de-identified reading times, trial info, and aggregate measures are shared.  
- Users should follow standard ethical guidelines for human-subject research.

## Citation

> Schäfer, L., Lemke, R., Drenhaus, H., & Reich, I. (2021). The Role of UID for the Usage of Verb Phrase Ellipsis: Psycholinguistic Evidence From Length and Context Effects. *Frontiers in Psychology, 12*, 661087. https://doi.org/10.3389/fpsyg.2021.661087  
> Adapted materials and analysis scripts: [Your Repository/OSF link]

## Contact

**ZENG Tao**, College of Foreign Languages, Hunan University, Changsha 410082, China  
taozengclarry@hnu.edu.cn
