****

# Genre
This project records my up-to-date progress in assessing the PM<sub>2.5</sub> health burden according to PM<sub>2.5</sub> pollutions across China. Theoretically, it is also applicable to other regions with accordingly population, pollution, baseline mortality data.

P.S.: the **PM2.5-attr-mort** refers to the **PM<sub>2.5</sub>-attributable-mortality**

# Supported Researches
1. Liu Y, Zhu G, Zhao Z, et al. Population aging might have delayed the alleviation of China's PM2.5 health burden[J]. Atmospheric Environment, 2022, 270: 118895.

2. Tang R, Zhao J, Liu Y, et al. Air quality and health co-benefits of China’s carbon dioxide emissions peaking before 2030[J]. Nature Communications, 2022, 13 (1): 1008.

3. Wang H, He X, Liang X, et al. Health benefits of on-road transportation pollution control programs in China[J]. Proceedings of the National Academy of Sciences, 2020, 117 (41): 25370-25377.


# Usage

1. Clone the repo and open it

2. Replace the sample data in `./Data/` with your customized ones: 
!!Attention: Data  Structure Must Be Kept!!

    - for China's estimation, you'll need to customize:
        - the `FID_information`, which stores the coordination and geophysical information of each grid cell
        - the `Grid_PM.csv`, which stores the annual-average PM concentrations (by column) of each grid cell
        - the `Grid_Pop.csv`, which stores the population size (by column) of each grid cell

    - for other regions, additionally customize:
        - the age structure data    
        - the mortality rate data
    - Note that all filenames shall be specified later in the `DataLoad` section in the `HealthBurdenCalc.R`.

3. Open the `PM25-attr-mort.Rproj` file in Rstudio.

4. In script `HealthBurdenCalc.R`, specify filenames in the `DataLoad` section if data is customized.

5. Run codes in `HealthBurdenCalc.R` by rows to calculate and summarise the result.


# Changelog

## 1.0-release 

the Initial published version of the PM25-attr-mort with a series of refined and easy-to-use functions to assess PM<sub>2.5</sub> health burden using the IER model.

## 2.0-release

PM<sub>2.5</sub>-Attr-Mort v2.0 incorporated both GEMM and IER model in calculating the PM<sub>2.5</sub> health burden, by refering to different concentration-responce lookup-table.

## 3.0-release

PM<sub>2.5</sub>-Attr-Mort v3.0 flushes the original linear algebra calculation by a more tidyr and database based syntax, which greatly improves the extensible for the scales of the model. 

## 4.0-doing

PM<sub>2.5</sub>-Attr-Mort v4.0 will be attached with fullly potential of easy extention and incorporation of multi-scale of input data. Which means this calculation model are finally capabale of community, urban, regional, national and global scale calculations - with just specify the scale of your data- and calculation will be done.
# Developer note
I made the calculation process generalized to be able to adopt to any attributable death estimations by applying population table, incidence table, concentration table and the concentration-correspondingly relative risk lookup table together.
