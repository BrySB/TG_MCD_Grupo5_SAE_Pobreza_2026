\# Estimación de la Tasa de Pobreza en Colombia: Enfoque de Áreas Pequeñas (SAE) con Componentes Espaciales



Este repositorio contiene el desarrollo analítico, metodológico y el código fuente del proyecto de investigación para la \*\*Maestría en Ciencia de Datos\*\* (Pontificia Universidad Javeriana Cali), orientado a la estimación y predicción del \*\*Índice de Pobreza Multidimensional (IPM) 2024\*\* en escalas territoriales desagregadas (municipal) en Colombia.



\## 📌 Descripción del Proyecto

Debido a las limitaciones de tamaño muestral de las encuestas de hogares tradicionales como la Encuesta Nacional de Calidad de Vida (ECV) del DANE, la estimación directa del IPM solo cuenta con representatividad estadística válida a nivel macro (departamental). 



Este proyecto implementa metodologías de \*\*Estimación para Áreas Pequeñas (SAE)\*\* —iniciando con modelos de áreas estándar de \*\*Fay-Herriot\*\* y evolucionando hacia estructuras de autocorrelación espacial (como modelos SAR)— para extrapolar parámetros y generar predicciones sintéticas confiables a nivel micro (abarcando los municipios del país), asegurando la consistencia e interpretabilidad del indicador dentro de un soporte analítico óptimo.



\## 📊 Insumos y Bloques de Covariables

El modelo combina la estimación directa con una matriz de covariables municipales proveniente de registros administrativos sectoriales de cobertura nacional, estructurada en tres bloques fundamentales:

\* \*\*Salud:\*\* Cobertura de regímenes (subsidiado/contributivo), aseguramiento capado, tasas de mortalidad infantil y transformaciones logarítmicas.

\* \*\*Educación:\*\* Tasas de repitencia general, deserción escolar e índices de reprobación académica.

\* \*\*Infraestructura Básica:\*\* Porcentajes de acceso a acueducto, alcantarillado, aseo urbano y un indicador sintético ponderado de déficit habitacional.



\## 📁 Estructura del Repositorio

\* `/doc`: Contiene la documentación académica, el documento final y los archivos fuente de soporte técnico compilados desde Overleaf.

\* `/src`: Scripts de desarrollo y modelación espacial ajustados mediante REML en R.



\## 🛠️ Tecnologías Utilizadas

\* \*\*R / RStudio:\*\* Modelación estadística y espacial (`sf`, `spdep`).

\* \*\*LaTeX / Overleaf:\*\* Documentación y maquetación del artículo de investigación.



\## 👥 Autores y Contacto

Este proyecto fue desarrollado en colaboración por:



\* \*\*Brayan Salgado Blanco\*\* - \[bsalgado@javerianacali.edu.co](mailto:bsalgado@javerianacali.edu.co)

\* \*\*Erick Caicedo Ruiz\*\* - \[erickaicedo@javerianacali.edu.co](mailto:erickaicedo@javerianacali.edu.co)

\* \*\*Juan Trochez Guayara\*\* - \[alkah3st28@javerianacali.edu.co](mailto:alkah3st28@javerianacali.edu.co)



\---

\*Para dudas, sugerencias o acceso a los resultados del modelo de áreas pequeñas, por favor póngase en contacto con cualquiera de los autores.\*

