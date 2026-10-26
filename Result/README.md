This directory stores calculation outputs.

`HealthBurdenCalc.R` writes country aggregates by total, endpoint, agegroup, and endpoint × agegroup. File names follow:

`{model}_{at}_{by}_Build{YYMMDD}.xlsx`

`DrivingFactors.R` writes its 24-sequence workbook as `{model}_DrivingFactors_{start}-{end}_Build{YYMMDD}.xlsx`. The directory is gitignored; retain only results chosen for release.
