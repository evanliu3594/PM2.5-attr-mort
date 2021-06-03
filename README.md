****

# Genre
This project records my up-to-date progress in assessing the PM<sub>2.5</sub> health burden. And I'm planning to build it as the enhanced BenMAP model. P.S.: _**the PM25-attr-mort refers to the PM<sub>2.5</sub>-attributable-mortality**_

# Changelog

## 1.0-release 

the Initial publish version of the PM25-attr-mort with a series of refined and easy-to-use functions to assess PM<sub>2.5</sub> health burden using the IER model.

## 2.0-release

PM<sub>2.5</sub>-Attr-Mort v2.0 incorporated both GEMM and IER model in calculating the PM<sub>2.5</sub> health burden, by refering to different concentration-responce lookup-table.

## 3.0-release

PM<sub>2.5</sub>-Attr-Mort v3.0 flushes the original linear algebra calculation by a more tidyr and database based syntax, which greatly improves the extensible for the scales of the model. 

## 4.0-doing

PM<sub>2.5</sub>-Attr-Mort v4.0 will be attached with fullly potential of easy extention and incorporation of multi-scale of input data. Which means this calculation model are finally capabale of community, urban, regional, national and global scale calculations - with just specify the scale of your data- and calculation will be done.

# Usage
Extract & replace the FID-info, Pop, PM<sub>2.5</sub> and age-structure data with yours(PLZ remember keep data in the same strcuture), then run the functions in R/Rstudio.

# Developer note
I made the calculation process generalized to be able to adopt to any attributable death estimations by applying population table, incidence table, concentration table and the concentration-correspondingly relative risk lookup table together.
