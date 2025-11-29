# README — Paquete de Replicación
________________________________________

Democracy and Security: The Impact of Regime Type on the Level of Physical Security

This repository contains the complete replication package of the undergraduate thesis “Democracia y seguridad: el impacto del tipo de régimen sobre el nivel de seguridad física” carried out at Universidad Torcuato Di Tella by Santiago Festa, with Carlos Gervasoni as advisor.

The objective of the work is to evaluate how an increase in the level of democracy can generate an increase in the level of physical security. For this, data from V-Dem, GBD, WB, UNDP, and WEO among others are used.

The replication package includes the codes to create the database, create electoral democracy indices using V-Dem indicators, analyze univariate and bivariate descriptive statistics, analyze the assumptions of the difference-in-differences (DiD) model, and run the OLS and DiD models.

The codes are written in R and must be executed manually, since several steps require user review. This includes both an analysis with the original V-Dem indices and an alternative reconstruction of the electoral democracy index to avoid problems of conceptual circularity.
________________________________________
1. Repository Structure
   
- /code/
    - 01_crear_base.R
    - 02_var_control.R
    - variable_weo.R
    - variable_wb.R
    - 03_estadisticas_descriptivas.R
    - 04_supuestos_DiD.R
    - 05_modelos.R
    - A1_MCMC.R
    - A2_ajuste_post_MCMC.R
    - A3_polyarchy_rep.R
- /data/
    - raw/
    - processed/
- /output/
    - descriptivas/
    - supuestos/
    - modelos/
- /documentation/
    - diccionario_variables.pdf
    - notas_metodologicas.pdf
    - session_info.txt

  ________________________________________
  2. Possible Analysis Paths

  The project can be replicated in two different ways:
    ________________________________________
  A) Simple Path

  Uses the original V-Dem indices. This path is included to preserve transparency and comparability, but it is not used in the final results due to problems of circularity between some indicators of electoral democracy and physical security:

1.	01_crear_base.R
2.	02_var_control.R
3.	03_estadisticas_descriptivas.R
4.	04_supuestos_DiD.R
5.	05_modelos.R

________________________________________
B) Path of Modification of the Electoral Democracy Index through MCMC

Uses MCMC to replicate the sub-indices that compose the electoral democracy index. This additional path arises because some indicators included by V-Dem in the construction of the electoral democracy index are conceptually related to phenomena of physical security, which can generate tautology problems in the models. The results presented in the work are obtained following this process:

1.	A1_MCMC.R
2.	A2_ajuste_post_MCMC.R
3.	A3_polyarchy_rep.R
4.	01_crear_base.R
5.	02_var_control.R
6.	03_estadisticas_descriptivas.R
7.	04_supuestos_DiD.R
8.	05_modelos.R

This process is more intensive in time and memory, especially MCMC.R can generate cache saturation problems and collapse the R session.

________________________________________
3. Manual Interventions Required

During replication, the user must perform:

- Decision on the use of original or replicated indices
- Modification of the indicators used
- Evaluation of the assumptions of the difference-in-differences model
  
Each script indicates in comments where to intervene.

________________________________________
4. Description of the Scripts

  - 01_crear_base.R

Builds the main database integrating V-Dem and GBD. Must be modified if Path B is used.

- 02_var_control.R

Adds and formats control variables. Automatically calls variable_weo.R and variable_wb.R.

- 03_estadisticas_descriptivas.R

Generates univariate and bivariate descriptive statistics, as well as identifying outliers.

- 04_supuestos_DiD.R

Evaluates each of the four assumptions of the Difference-in-Differences model.

- 05_modelos.R

  Runs the OLS and DiD models and exports the results in tables and graphs.
________________________________________
Optional Scripts

- A1_MCMC.R

  Replicates the V-Dem indices using Markov Chain Monte Carlo.
  
- A2_ajuste_post_MCMC.R

  Adjusts the indices and evaluates their quality.
  
- A3_polyarchy_rep.R

  Reconstructs the electoral democracy index from the new indices created.
________________________________________
5. Suggested Citation
   
Festa, S. (2025). Democracia y seguridad: el impacto del tipo de régimen sobre el nivel de seguridad física. Universidad Torcuato Di Tella.
