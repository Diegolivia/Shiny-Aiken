=======

# Shiny Aiken

*Shiny Aiken* es una aplicación interactiva desarrollada en R mediante el entorno *Shiny* (Chang et al., 2024). Permite a los usuarios realizar análisis psicométricos no solo para fines de investigación, sino también para enseñar conceptos y facilitar el análisis rutinario de pruebas educativas y psicológicas de manera sencilla y accesible (Martinková & Drabinová, 2018).

Esta herramienta se centra en la cuantificación de la validez de contenido mediante la **V de Aiken**, incluyendo el cálculo de intervalos de confianza asimétricos (método Score propuesto por Merino & Livia, 2009), la evaluación de la homogeneidad entre jueces y la comparación estadística entre dos grupos de evaluadores.

## 🌟 Características Principales

* **Cálculo de la V de Aiken:** Resultados detallados por cada ítem y una V de Aiken total para el instrumento.
* **Intervalos de Confianza (Score):** Cálculo preciso de los límites inferior (LI) y superior (LS) para la toma de decisiones.
* **Homogeneidad de Jueces:** Cálculo de la correlación promedio (Coeficiente H) entre los evaluadores.
* **Comparación de Grupos:** Herramienta integrada para comparar estadísticamente la V de Aiken entre dos grupos distintos de jueces (ej. clínicos vs. académicos).
* **Visualización Gráfica:** Gráficos generados de forma automática mostrando las estimaciones y sus respectivos intervalos de confianza frente a un punto de corte definido por el usuario.
* **Flexibilidad de Datos:** Capacidad para leer archivos Excel (`.xlsx`, `.xls`) y `.csv` en dos tipos de formatos u orientaciones distintas.

## 🚀 Cómo ejecutar la aplicación

No es necesario descargar, clonar el repositorio ni instalar paquetes adicionales en tu computadora de forma manual si cuentas con R.

### 1. Requisitos previos

Asegúrate de tener instalados los siguientes paquetes en tu entorno de R. Si no los tienes, puedes instalarlos ejecutando:

```R
install.packages(c("shiny", "readxl", "dplyr", "DT", "ggplot2", "tidyr"))

```

### 2. Ejecutar directamente desde GitHub

Una vez instalados los paquetes requeridos, puedes iniciar la aplicación ejecutando la siguiente línea de código en tu consola de R o RStudio:

```R
shiny::runGitHub("Shiny-Aiken", "Diegolivia")

```

La aplicación se descargará de manera temporal en segundo plano y se abrirá inmediatamente en tu navegador web o visor de RStudio.

## 📊 Formato de Datos

La aplicación es capaz de detectar automáticamente la orientación de tu base de datos o te permite configurarla manualmente. Soporta los dos formatos de entrada más comunes:

**Formato A (Ítems en filas, Jueces en columnas):**
Se requieren columnas que inicien con la palabra "Expert" para que la app las detecte automáticamente como jueces.

| Item | Expert1 | Expert2 | Expert3 | ... |
| --- | --- | --- | --- | --- |
| Item 1 | 4 | 3 | 4 | ... |
| Item 2 | 4 | 4 | 4 | ... |

**Formato B (Jueces en filas, Ítems en columnas):**
Se requieren columnas que inicien con la palabra "Item" para detectar las calificaciones. Puedes incluir una columna "Grupo" para posteriores comparaciones.

| Juez | Grupo | Item1 | Item2 | Item3 | ... |
| --- | --- | --- | --- | --- | --- |
| Juez01 | Clínicos | 4 | 3 | 4 | ... |
| Juez02 | Académicos | 3 | 4 | 4 | ... |

*(Nota: La aplicación incluye botones de demostración para cargar estos datos de ejemplo y explorar las funciones al instante).*

## 📚 Referencias

* Chang, W., et al. (2024). *shiny: Web Application Framework for R*.
* Martinková, P., & Drabinová, A. (2018). ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests. *The R Journal, 10*(2), 503–515.
* Merino, C., & Livia, J. (2009). Intervalos de confianza asimétricos para el índice la validez de contenido: Un programa Visual Basic para la V de Aiken. *Anales de Psicología, 25*(1), 169-171.
========
