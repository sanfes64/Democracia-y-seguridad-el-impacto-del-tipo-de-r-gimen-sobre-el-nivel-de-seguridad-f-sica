# README — Paquete de Replicación
Democracia y seguridad: el impacto del tipo de régimen sobre el nivel de seguridad física

Este repositorio contiene el paquete de replicación completo de la tesis de grado
“Democracia y seguridad: el impacto del tipo de régimen sobre el nivel de seguridad física”, realizada en la Universidad Torcuato Di Tella por Santiago Festa, con Carlos Gervasoni como tutor.
El objetivo del trabajo es evaluar cómo un aumento en el nivel de la democracia puede generar un aumento en el nivel de seguridad física. Para esto se utilizan datos de V-Dem, GBD, WB, PNUD y WEO entre otras.
El paquete de replicación incluye los códigos para crear la base de datos, crear índices de democracia electoral utilizando indicadores de V-Dem, analizar las estadísticas descriptivas univariadas y bivariadas, analizar los supuestos del modelo de diferencias en diferencias (Dif-in-Dif) y realizar los modelos de OLS y Dif-in-Dif.
Los códigos están escritos en R y deben ejecutarse manualmente, ya que varios pasos requieren revisión por parte del usuario. Esto incluye tanto un análisis con los índices originales de V-Dem como una reconstrucción alternativa del índice de democracia electoral para evitar problemas de circularidad conceptual
________________________________________
1. Estructura del repositorio
/code/
    01_crear_base.R
    02_var_control.R
    variable_weo.R
    variable_wb.R
    03_estadisticas_descriptivas.R
    04_supuestos_DiD.R
    05_modelos.R
    A1_MCMC.R
    A2_ajuste_post_MCMC.R
    A3_polyarchy_rep.R

/data/
    raw/
    processed/

/output/
    descriptivas/
    supuestos/
    modelos/

/documentation/
    diccionario_variables.pdf
    notas_metodologicas.pdf
    session_info.txt

________________________________________
2. Caminos posibles del análisis
El proyecto puede reproducirse de dos formas distintas:
________________________________________
A) Camino sencillo
Utiliza los índices originales de V-Dem. Este camino se incluye para preservar transparencia y comparabilidad, pero no es el utilizado en los resultados finales debido a los problemas de circularidad entre algunos indicadores de democracia electoral y seguridad física:
1.	01_crear_base.R
2.	02_var_control.R
3.	03_estadisticas_descriptivas.R
4.	04_supuestos_DiD.R
5.	05_modelos.R
________________________________________
B) Camino de modificación del índice de democracia electoral mediante MCMC
Utiliza MCMC para replicar los sub-índices que componen el índice de democracia electoral. Este camino adicional surge porque algunos indicadores incluidos por V-Dem en la construcción del índice de democracia electoral están conceptualmente relacionados con fenómenos de seguridad física, lo que puede generar problemas de tautología en los modelos. Los resultados presentados en el trabajo son obtenidos siguiendo este proceso:
1.	A1_MCMC.R
2.	A2_ajuste_post_MCMC.R
3.	A3_polyarchy_rep.R
4.	01_crear_base.R
5.	02_var_control.R
6.	03_estadisticas_descriptivas.R
7.	04_supuestos_DiD.R
8.	05_modelos.R
Este proceso es más intensivo en tiempo y memoria, especialmente MCMC.R puede generar problemas de saturación del caché y colapsar la sesión de R.
________________________________________
3. Intervenciones manuales necesarias
Durante la replicación, el usuario debe realizar:
•	Decisión sobre el uso de índices originales o replicados,
•	Modificación de los indicadores utilizados
•	Evaluación de los supuestos del modelo de diferencias en diferencias
Cada script indica en comentarios dónde intervenir.
________________________________________
4. Descripción de los scripts
01_crear_base.R
Construye la base principal integrando V-Dem y GBD. Debe modificarse si se utiliza el camino B.
02_var_control.R
Agrega y formatea las variables de control. Llama automáticamente a variable_weo.R y variable_wb.R.
03_estadisticas_descriptivas.R
Genera estadísticas descriptivas univariadas y bivariadas, además de identificar outliers.
04_supuestos_DiD.R
Evalúa cada uno de los cuatro supuestos del modelo de Diferencias en Diferencias.
05_modelos.R
Ejecuta los modelos OLS y Dif-in-Dif y exporta los resultados en tablas y gráficos.
________________________________________
Scripts opcionales
•	A1_MCMC.R – Replica los índices de V-Dem utilizando cadenas de Montecarlo Markov.
•	A2_ajuste_post_MCMC.R – Ajusta los índices y evalúa su calidad.
•	A3_polyarchy_rep.R – Reconstruye el índice de democracia electoral a partir de los nuevos índices creados.
________________________________________
5. Cita sugerida
Festa, S. (2025). Democracia y seguridad: el impacto del tipo de régimen sobre el nivel de seguridad física. Universidad Torcuato Di Tella.
