# Bayesian Multiple Linear Regression in R

![R Logo](r_logo.png)

## Overview

This R code repository presents an empirical Bayesian approach to modeling multiple linear regression. The investigation utilizes the empirical Bayes method to derive coefficient distributions that explain mortality rates, using the same dataset employed by G.C. McDonald and R.C. Schwing [1]. The process begins with the fitting of a multiple linear regression model, wherein non-significant explanatory variables are eliminated during construction. The resulting coefficients 𝛽𝑖 from the linear model are transformed into the primary axes of prior information, giving the analysis an empirical Bayesian perspective. Additionally, a Bayesian linear regression model is constructed and the results are compared.

## Background

Bayesian methods provide a powerful approach to statistical modeling by incorporating prior knowledge into the analysis. The empirical Bayesian approach combines the strengths of frequentist and Bayesian methodologies, using the data itself to inform the prior distribution. This allows for a more data-driven and robust estimation of model parameters.

## Usage

The provided R code demonstrates the process of empirical Bayesian multiple linear regression. By following the code, you can replicate the analysis described above and gain insights into the empirical Bayesian approach to modeling.

## Reference

For a comprehensive understanding of the empirical Bayesian approach in multiple linear regression and the comparison of results with traditional Bayesian regression, refer to the original source:

[Vanessa Alcalde, Luciano Garrido, Pablo Martinez Angerosa. "Modelos Bayesianos."](https://github.com/PabloMartinezAngerosa/Modelo-Regresion-Lineal-Bayesiana-/blob/main/paper.pdf)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

*Disclaimer: This project is intended for educational and research purposes. Bayesian modeling involves statistical concepts that may require a solid understanding for proper interpretation.*
