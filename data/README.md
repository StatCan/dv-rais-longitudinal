## Data files used in the Shiny app / Fichiers de données utilisés dans l'application Shiny

Data files are downloaded from the Statistics Cannada webpage and saved into R's native binary format (Rds).
These files are created using the following commands, with the function `download_data.R` in the `R` directory.

Les fichiers de données sont téléchargés depuis la page web de Statistique Cananda et enregistrés dans le format binaire natif de R (Rds).
Ces fichiers sont créés à l'aide des commandes suivantes, avec la fonction `download_data.R` dans le répertoire `R`.

```R

library(tidyverse)
source("R/download_data.R")

pathway <- download_data(
    "37100193", c("sex", "trade", "ind"),
    file_name="pathway")
mobility_measures <- download_data(
    "37100205", c("trade", "mode", "years", "type", "ind"),
    file_name="mobility_measures")
mobility_matrix <- download_data(
    "37100204", c("trad", "mode", "years", "type", "to"),
    file_name="mobility_matrix")

```