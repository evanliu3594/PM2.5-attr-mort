****

# Genre
This project records my up-to-date progress in assessing the PM<sub>2.5</sub> health burden. And I'm planning to build it as the enhanced BenMAP model. P.S.: _**the PM25-attr-mort refers to the PM<sub>2.5</sub>-attributable-mortality**_

# Changelog

## 1.0-release 

the Initial publish version of the PM25-attr-mort with a series of refined and easy-to-use functions to assess PM<sub>2.5</sub> health burden using the IER model.

## 2.0-release

PM<sub>2.5</sub>-Attr-Mort v2.0 incorporated both GEMM and IER model in calculating the PM<sub>2.5</sub> health burden, by refering to different concentration-responce lookup-table.

# Usage
Extract & replace Pop, PM2.5 and age-structure data with yours(PLZ remember keep data in the same strcuture), then run the functions in R, while Rstudio is preferred.

# Developer note
I made the calculation process generalized to be able to adopt to any attributable death estimations by applying population table, incidence table, concentration table and the concentration-correspondingly relative risk lookup table together.
