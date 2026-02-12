library(dplyr)


diabetes <- read.csv("~/dtGAP/data-raw/diabetes.csv")
penguins <- read.csv("~/dtGAP/data-raw/penguins_size.csv")

galaxy <- readr::read_tsv(
  "~/dtGAP/data-raw/690_visualizing_galaxy.tsv"
)
wine_quality_red <- readr::read_tsv(
  "~/dtGAP/data-raw/wine-quality-red.tsv"
)


train_covid_raw <- readr::read_tsv(
  "https://raw.githubusercontent.com/trangdata/Pre_Surv_COVID_19/master/data/processed_covid_train.tsv"
)
colnames(train_covid_raw) <- iconv(colnames(train_covid_raw), from = "UTF-8", to = "ASCII")
attr(train_covid_raw, "spec") <- NULL

test_covid_raw <- readr::read_tsv(
  "https://raw.githubusercontent.com/trangdata/Pre_Surv_COVID_19/master/data/processed_covid_test.tsv"
)

train_covid <- train_covid_raw %>%
  dplyr::select(
    LDH = "Lactate dehydrogenase",
    hs_CRP = "High sensitivity C-reactive protein",
    Lymphocyte = "(%)lymphocyte",
    Outcome = Type2
  ) %>%
  mutate(Outcome = as.factor(Outcome)) %>%
  na.omit()

test_covid <- test_covid_raw %>%
  dplyr::select(
    LDH = "Lactate dehydrogenase",
    hs_CRP = "High sensitivity C-reactive protein",
    Lymphocyte = "(%)lymphocyte",
    Outcome = "outcome"
  ) %>%
  mutate(Outcome = as.factor(Outcome))


Psychosis_Disorder <- read.delim("~/dtGAP/data-raw/SAPSSANS9550.txt")
wine <- read.delim("~/dtGAP/data-raw/wine_scaled.txt")
wine <- wine %>% dplyr::select(-no)

usethis::use_data(diabetes, penguins, wine,
  train_covid, test_covid, Psychosis_Disorder,
  wine_quality_red, galaxy,
  overwrite = TRUE
)
