
# Mapping Education Access in West Africa
## Analysis of Schooling Pathways using DHS, MICS and other data sources

This project is where I put together my technical skills and my interest in social impact. I use **RStudio** to process raw datasets (Stata format) from **The DHS Program** to understand why some children start school late and how that affects their future.

---



### Why I am doing this project
I believe education is the most important tool for equal opportunity. Before starting as a freelance analyst, I worked for 3 years in **Global Health** as a consultant for the **World Bank (GFF)**.

I launched this project because of a specific problem: **late enrollment**. In West Africa, many children start school much later than the official age. I want to show that this isn't just a small delay; it is a "hidden" driver of bigger problems.

Based on recent evidence in Côte d'Ivoire (like **Whitehead et al., 2024**), starting late creates a huge age gap in the same classroom. My goal with this analysis is to show how these gaps lead to:

* **Lower Literacy & Numeracy**: When ages are too different in one class, teaching becomes difficult and learning outcomes drop.

* **Higher Dropout Rates**: Children who start late are more likely to leave school before finishing.

I am using my data skills to track these patterns in **Côte d'Ivoire, Senegal, Niger, Nigeria, Benin, and Ghana**. By showing where the "enrollment bottlenecks" are, we can help design better policies to give every child a fairer start

---

### How I organize this project
I use a structured workflow to make sure my results are reliable and easy to follow. This comes from my experience managing data pipelines at the World Bank and my training at TSE:

* **00_r_script/**: This is the "brain" of the project. It contains the entire analytical pipeline, divided into logical steps:

  * **/00_master.R**: The "brain" of the project that runs all scripts in order.

  * **/01_import.R**: Handles data entry using haven to read Stata files.

  * **/02_dqa.R**: My Data Quality Assessment script. Before any analysis, I perform 7 distinct checks (missingness, duplicates, outliers, range constraints, etc.) to ensure the raw data is trustworthy.

  * **/03_data_cleaning.R**: Where I handle the conversions and final tidying based on the DQA results.

  * **/04_data_analysis.R**: Focuses on the core research questions and statistical testing.

  * **/05_data_visualization.R**: Generates the final outputs for the reports.

* **01_Output/**: here you will find the main results and tables subdivided into 2 subfolders to distinguish DQA from Analysis
    * **/DQA**
    * **/Analysis**
    
* **02_Presentations/**:
    * **/visualization**: This is for the charts and (interactive) maps I create to show the data clearly. I use libraries like ggplot2 and leaflet to make the data accessible for policy-makers.
    * **/Reports**: I use RMarkdown to create stakeholder-ready documents that combine my code with clear explanations.
    
* **Country_Folders/** (e.g., `Cote_d_Ivoire/`):
    * **/01_Raw**: This stays empty because I don't share the raw DHS data publicly for privacy reasons.
    * **/02_Clean**: This is where the analysis-ready files are stored after the DQA and cleaning process.

---

### Technical Approach
I use R to clean and analyze the data, even though the raw files from the DHS Program are usually in Stata (.dta) format. I follow UNESCO and World Bank EdStats standards so the results make sense to international organizations.

---

### Note on Data Privacy
To respect the rules of DHS and MICS, I do not share the raw data here. If you want to run my code, you need to request the data at dhsprogram.com and put the files in the 01_Raw folders.

---

**Kadidja Sidibé** *Freelance Data Analyst | Former World Bank Consultant* | *MSc Public Policy & Development (Toulouse School of Economics / TSE)* | *📧 kad.sdb@gmail.com*

