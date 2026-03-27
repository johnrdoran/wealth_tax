library(dplyr)

df <- read.csv("C:/Users/jrdor/OneDrive/Desktop/CA_Tax_Base_Erosion/Data/TaxSalesByCounty.csv")

#Filter to 2010-2024 aggregate for LA, SF,SM, and SC
df_filtered <- df %>%
  filter(
    (`Calendar.Year` >= 2010) &
    (Quarter == "A") &
    County %in% c("Los Angeles", "San Francisco", "San Mateo", "Santa Clara")
)

write.csv(df_filtered, "C:/Users/jrdor/OneDrive/Desktop/CA_Tax_Base_Erosion/Data/TaxSalesByCounty_clean.csv", row.names = FALSE)