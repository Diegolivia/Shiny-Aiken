# Shiny Aiken

*Shiny Aiken* is an interactive application developed in R using the *Shiny* framework (Chang et al., 2024). It allows users to perform psychometric analyses not only for research purposes but also to teach concepts and facilitate the routine analysis of educational and psychological tests in a simple and accessible way (Martinková & Drabinová, 2018).

This tool focuses on quantifying content validity using **Aiken's V**, including the calculation of asymmetric confidence intervals (Score method proposed by Merino & Livia, 2009), the evaluation of homogeneity among judges, and the statistical comparison between two groups of raters.

## 🌟 Main Features

* **Aiken's V Calculation:** Detailed results for each item and a total Aiken's V for the instrument.
* **Confidence Intervals (Score):** Precise calculation of the lower (LL) and upper (UL) limits for decision making.
* **Homogeneity of Judges:** Calculation of the average correlation (H Coefficient) among raters.
* **Group Comparison:** Built-in tool to statistically compare Aiken's V between two distinct groups of judges (e.g., clinical vs. academic).
* **Graphical Visualization:** Automatically generated plots displaying the estimates and their respective confidence intervals against a user-defined cutoff point.
* **Data Flexibility:** Ability to read Excel (`.xlsx`, `.xls`) and `.csv` files in two different formats or orientations.

## 🚀 How to run the application

You do not need to download, clone the repository, or manually install the app on your computer if you already have R.

### 1. Prerequisites

Make sure you have the following packages installed in your R environment. If you don't have them, you can install them by running:

```R
install.packages(c("shiny", "readxl", "dplyr", "DT", "ggplot2", "tidyr"))

```

### 2. Run directly from GitHub

Once the required packages are installed, you can start the application by running the following line of code in your R or RStudio console:

```R
shiny::runGitHub("Shiny-Aiken", "Diegolivia")
```

The application will be temporarily downloaded in the background and will open immediately in your web browser or RStudio viewer.

## 📊 Data Format

The application can automatically detect the orientation of your database or allows you to configure it manually. It supports the two most common input formats:

**Format A (Items in rows, Judges in columns):**
Columns starting with the word "Expert" are required for the app to automatically detect them as judges.

| Item | Expert1 | Expert2 | Expert3 | ... |
| --- | --- | --- | --- | --- |
| Item 1 | 4 | 3 | 4 | ... |
| Item 2 | 4 | 4 | 4 | ... |

**Format B (Judges in rows, Items in columns):**
Columns starting with the word "Item" are required to detect the ratings. You can include a "Group" column for later comparisons.

| Judge | Group | Item1 | Item2 | Item3 | ... |
| --- | --- | --- | --- | --- | --- |
| Judge01 | Clinical | 4 | 3 | 4 | ... |
| Judge02 | Academic | 3 | 4 | 4 | ... |

*(Note: The application includes demo buttons to load these example datasets and explore the features instantly).*

## 📚 References

* Chang, W., et al. (2024). *shiny: Web Application Framework for R*.
* Martinková, P., & Drabinová, A. (2018). ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests. *The R Journal, 10*(2), 503–515.
* Merino, C., & Livia, J. (2009). Intervalos de confianza asimétricos para el índice la validez de contenido: Un programa Visual Basic para la V de Aiken. *Anales de Psicología, 25*(1), 169-171.
