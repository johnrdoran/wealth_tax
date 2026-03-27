library(dplyr)
library(data.table)
library(tidyr)

base_path <- "C:/Users/jrdor/OneDrive/Desktop/CA_Tax_Base_Erosion/Data/CES_data"
out_path  <- base_path



#Variables from CES' FMLI
keep_vars <- c("NEWID", "FINLWT21", "FINCBTXM", "INC_RANK",
               "TOTEXPCQ", "TOTEXPPQ", "STATE", "POPSIZE",
               "APPARCQ", "APPARPQ", "HOUSCQ", "HOUSPQ")

#Aggregate FMLI data
read_fmli_year <- function(year) {
  yr      <- sprintf("%02d", year %% 100)
  yr_next <- sprintf("%02d", (year + 1) %% 100)
  
  pattern <- paste0("fmli", yr, "[1-4]x?\\.csv$|fmli", yr_next, "1x?\\.csv")
  files   <- list.files(out_path, pattern = pattern, full.names = TRUE)
  
  basenames  <- basename(files)
  has_x      <- gsub("x\\.csv$", ".csv", basenames)
  drop_plain <- basenames %in% has_x[grepl("x\\.csv$", basenames)]
  files     <- files[!drop_plain]
  
  cat("Year", year, "- using:", paste(basename(files), collapse = ", "), "\n")
  
  dfs <- lapply(files, function(f) {
    dt <- fread(f, colClasses = "character")
    setnames(dt, toupper(names(dt)))
    available <- intersect(keep_vars, names(dt))
    dt[, ..available]
  })
  
  bind_rows(dfs) |> mutate(survey_year = year)
}

fmli_all <- bind_rows(lapply(2010:2024, read_fmli_year))


#Variables from CES' MTBI
taxable_uccs <- c(
  #Furniture and Appliances
  "230111", "230112", "230113", "230114", "230115",
  #Electronics
  "270101", "270102", "270103", "270104",
  #Vehicles
  "450110", "450210", "450310",
  #Restaurant Meals
  "190111", "190112", "190113", "190114",
  #Alcoholic Beverages
  "790110", "790120", "790220",
  #Personal Care Products
  "650110", "650210"
)

#Aggregate MTBI data
read_mtbi_year <- function(year) {
  yr      <- sprintf("%02d", year %% 100)
  yr_next <- sprintf("%02d", (year + 1) %% 100)
  
  pattern <- paste0("mtbi", yr, "[1-4]x?\\.csv$|mtbi", yr_next, "1x?\\.csv$")
  files   <- list.files(out_path, pattern = pattern, full.names = TRUE)
  
  basenames <- basename(files)
  has_x     <- gsub("x\\.csv$", ".csv", basenames)
  drop_plain <- basenames %in% has_x[grepl("x\\.csv$", basenames)]
  files     <- files[!drop_plain]
  
  cat("MTBI Year", year, "- using:", paste(basename(files), collapse = ", "), "\n")
  
  dfs <- lapply(files, function(f) {
    dt <- fread(f, colClasses = "character")
    setnames(dt, toupper(names(dt)))
    dt
  })
  
  bind_rows(dfs) |> mutate(survey_year = year)
  
}

mtbi_all <- bind_rows(lapply(2010:2024, read_mtbi_year))

#Filter and aggregate
mtbi_taxable <- mtbi_all |>
  mutate(UCC  = as.character(UCC),
         COST = as.numeric(COST)) |>
  filter(UCC %in% taxable_uccs) |>
  group_by(NEWID, survey_year) |>
  summarise(taxable_expend_mtbi = sum(COST, na.rm = TRUE), .groups = "drop")

#Pull needed FMLI variables
ces <- fmli_all |>
  mutate(across(c(FINLWT21, FINCBTXM, TOTEXPCQ, TOTEXPPQ, STATE, POPSIZE, INC_RANK,
                  APPARCQ, APPARPQ, HOUSCQ, HOUSPQ), as.numeric)) |>
  filter(STATE == 6) #California FIPS = 6

#Merge with MTBI
ces_merged <- ces |>
  left_join(mtbi_taxable, by = c("NEWID", "survey_year")) |>
  mutate(taxable_expend_mtbi = replace_na(taxable_expend_mtbi, 0))

#Estimate consumption share by percentile
ces_ca <- ces_merged |>
  mutate(
    INC_RANK      = INC_RANK * 100,
    taxable_expend = (
      (APPARCQ + APPARPQ) +
      (HOUSCQ + HOUSPQ)
    ) * 2 + taxable_expend_mtbi,
    inc_group = case_when(
      INC_RANK >= 99 ~ "Top 1%",
      INC_RANK >= 95 ~ "Top 1-5%",
      INC_RANK >= 90 ~ "Top 5-10%",
      TRUE           ~ "Bottom 90%"
    )
  )

#Weighted consumption share by year and income group
consumption_shares <- ces_ca |>
  group_by(survey_year, inc_group) |>
  summarise(
    wtd_expend = sum(taxable_expend * FINLWT21, na.rm = TRUE),
    n_obs      = n(),
    .groups    = "drop"
  ) |>
  group_by(survey_year) |>
  mutate(total_expend = sum(wtd_expend),
         cons_share   = wtd_expend / total_expend)


#Top 1% Share by Year
top1_share <- consumption_shares |>
  filter(inc_group == "Top 1%") |>
  select(survey_year, cons_share)

#Merge FMLI with CDTFA
cdtfa <- read.csv("C:/Users/jrdor/OneDrive/Desktop/CA_Tax_Base_Erosion/Data/TaxSalesByCounty_clean.csv")

merged <- cdtfa |>
  group_by(Calendar.Year) |>
  summarise(total_taxable = sum(Total.Taxable.Transactions, na.rm = TRUE)) |>  # adjust column name to match yours
  left_join(top1_share, by = c("Calendar.Year" = "survey_year")) |>
  mutate(
    est_top1_transactions = total_taxable * cons_share,         # top 1% share of taxable sales
    est_tax_revenue       = est_top1_transactions * 0.0725      # apply 7.25% state rate
  )

write.csv(merged, "C:/Users/jrdor/OneDrive/Desktop/CA_Tax_Base_Erosion/Data/CA_Billionaire_Sales_Tax.csv", row.names = FALSE)