**#Project Description**
The project deals with an investigation of indirect markers for metabolic syndrome in children with Type 1 diabetes (T1DM). These components of metabolic syndrome may fluctuate in the case of children who are on exogenous insulin therapy. It is therefore important to investigate the stability of these components which will be used to assess metabolic syndrome. To assess the strength of association we perform a meta-analysis of observational studies with cohort of children with T1DM.

**#Steps to analysis:**
1.	You can load the script in R studio and download the CSV for data provided in your repository for the project keeping track of the file path.
2.	Prior to running the script, you need to install all the packages listed in the script. This might take a few minutes. Recall the library for the packages.
3.	A CSV file (**MetS_vs_NoMetS.csv**) contains the mean, SD, and sample size of the cohort divided into patients with and without metabolic syndrome. The same file is then subjected to the script titled (**Meta_Analysis_Script.R**) giving the file path where you stored the data.
4.	Some of the parameters such as duration of diabetes, insulin units, HbA1c, Waist circumference, and lipid profile are analyzed separately by selecting specific columns for analysis. During each parameter, the specific column is selected to become a data frame and then subjected to calculate the standard mean difference.
5.	These columns are subjected to the command “esc_mean_sd” which calculates the standard mean difference.
6.	The difference is then visualized using “forest()” command to plot forest plot.
7.	Precision and homogeneity are tested using the “funnel()” command which looks for the symmetry of datasets in a triangle.
8.	The process is repeated after the removal of datasets to see the difference in the cumulative standard mean difference.

**Note** To select a specific column for each command use the "$" sign after the data frame name.
For more details on the project please refer to the medRxiv link given below doi: https://doi.org/10.1101/2023.05.23.23290372

**#Contributors to the project** 
Sukeshini Khandagale (SIU JRF) 
Email id: sukeshini[dot]Khandagale[@]ssbs.edu.in
Symbiosis School of Biological Sciences (SSBS)
Symbiosis International University, Lavale, Pune 411015

Dr. Satyajeet Khare (Assistant Professor)
Email id: Satyajeet[dot]khare[@]ssbs.edu.in
Symbiosis School of Biological Sciences (SSBS)
Symbiosis International University, Lavale, Pune 411015
