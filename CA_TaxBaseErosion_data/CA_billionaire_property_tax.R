library(dplyr)

df <- read.csv("C:/Users/jrdor/OneDrive/Desktop/CA_Tax_Base_Erosion/Data/PropTaxAssessedValueStateCounty.csv")

df_filtered <- df %>%
  filter(
    (`Assessment.Year.From` == 2025) &
    County %in% c("Los Angeles", "San Francisco", "San Mateo", "Santa Clara")
  )

write.csv(df_filtered, "C:/Users/jrdor/OneDrive/Desktop/CA_Tax_Base_Erosion/Data/PropTaxAssessedValueStateCounty_filtered.csv", row.names = FALSE)