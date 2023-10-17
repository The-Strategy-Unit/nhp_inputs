# peers

library(tidyverse)
library(NHSRtools)
library(PostcodesioR)
library(sf)
library(ggrepel)

countries <- httr::modify_url(
  url = "https://services1.arcgis.com",
  path = c(
    "ESMARspQHYMw9BZ9",
    "arcgis",
    "rest",
    "services",
    "Countries_December_2022_GB_BUC",
    "FeatureServer",
    "0",
    "query"
  ),
  query = list(
    outFields = "*",
    where = "1=1",
    f = "geojson"
  )
) |>
  read_sf() |>
  summarise() |>
  st_transform(27700) |>
  st_cast("MULTILINESTRING")

ods_trusts <- ods_get_trusts()

providers <- ods_trusts |>
  pull(post_code) |>
  stringr::str_remove_all("\\s+") |>
  unique() |>
  # chunk vector into groups of size [0, 100]
  # inverse of flatten
  (\(.x) split(.x, ceiling(seq_along(.x) / 100)))() |>
  map(\(.x) bulk_postcode_lookup(list(postcodes = .x))) |>
  flatten() |>
  map("result") |>
  map(\(.x) .x[c("postcode", "eastings", "northings")]) |>
  bind_rows() |>
  inner_join(ods_trusts, by = join_by(postcode == post_code)) |>
  st_as_sf(coords = c("eastings", "northings"), crs = 27700) |>
  mutate(distance = st_distance(geometry, countries)[, 1])

ggplot() +
  geom_sf(data = countries) +
  geom_sf(data = providers |> filter(org_id == "RL4")) +
  geom_sf(data = providers |>
    filter(org_id == "RL4") |>
    st_nearest_points(countries) |>
    st_as_sf())

local({
  p <- sample_n(providers, 10)

  leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::addMarkers(
      data = st_transform(p, crs = 4326),
      popup = ~ paste0(name, ": ", scales::comma(as.numeric(distance), 0.1, 0.001), "km")
    ) |>
    leaflet::addPolylines(
      data = p |>
        st_nearest_points(countries) |>
        st_as_sf() |>
        st_transform(crs = 4326),
      weight = 3,
      opacity = 1
    ) |>
    leaflet::addPolylines(
      data = st_transform(countries, 4326),
      weight = 1,
      color = "black",
      opacity = 1
    )
})



con <- get_con("HESdata")

withr::with_dir("C:/Users/thomas.jemmett/OneDrive - Midlands and Lancashire CSU/", {
  mff <- read_csv("mff_values.csv", col_types = "dcd") |>
    filter(year == 201819) |>
    select(provider, mff = value)
})

lsoa_scores <- tbl(con, dbplyr::in_catalog("Reference", "Demography", "DIM_tbDomainsOfDeprivationByLSOA")) |>
  filter(Population_Snapshot_Date == "2015-07-01") |>
  select(LSOA_Code, imd_score = IMD_Score)

op_counts <- tbl(con, "tbOutpatients") |>
  inner_join(lsoa_scores, by = join_by(lsoa11 == LSOA_Code)) |>
  filter(
    fyear == 201819,
    attended %in% c("5", "6"),
    atentype %in% c("1", "2", "3", "21", "22", "23"),
  ) |>
  group_by(procode3) |>
  summarise(
    n = n(),
    across(imd_score, ~ mean(.x, na.rm = TRUE)),
    age15 = sum(ifelse(apptage <= 15, 1, 0), na.rm = TRUE),
    age6074 = sum(
      case_when(
        apptage < 60 ~ 0,
        apptage >= 75 ~ 0,
        .default = 1
      ),
      na.rm = TRUE
    ),
    ccgs = n_distinct(ccg_responsibility),
    urban = sum(
      case_match(
        rururb_ind,
        1 ~ 1,
        5 ~ 1,
        .default = 0
      ),
      na.rm = TRUE
    )
  ) |>
  collect()

aae_counts <- tbl(con, "tbAandE") |>
  filter(fyear == 201819, aeattendcat == "1") |>
  inner_join(lsoa_scores, by = join_by(lsoa11 == LSOA_Code)) |>
  group_by(procode3) |>
  summarise(
    n = n(),
    across(imd_score, ~ mean(.x, na.rm = TRUE)),
    age15 = sum(ifelse(arrivalage <= 15, 1, 0), na.rm = TRUE),
    age6074 = sum(
      case_when(
        arrivalage < 60 ~ 0,
        arrivalage >= 75 ~ 0,
        .default = 1
      ),
      na.rm = TRUE
    ),
    ccgs = n_distinct(ccg_responsibility),
    urban = sum(
      case_match(
        rururb_ind,
        1 ~ 1,
        5 ~ 1,
        .default = 0
      ),
      na.rm = TRUE
    )
  ) |>
  collect()

ip <- tbl(con, "tbInpatients") |>
  filter(FYEAR == 201819)

ip_diags <- tbl(con, "tbInpatientsDiagnoses") |>
  filter(DIAGORDER == 1)

ip_hrg <- local({
  hrg <- ip |>
    count(PROCODE3, hrg_chapter = LEFT(SUSHRG, 1)) |>
    collect()

  national_p <- hrg |>
    count(hrg_chapter, wt = n, name = "ap") |>
    mutate(across(ap, ~ .x / sum(.x)))

  hrg |>
    mutate(p = n / sum(n), .by = PROCODE3) |>
    inner_join(national_p, by = join_by(hrg_chapter)) |>
    summarise(ds = sum(p * log(p / ap)), .by = PROCODE3)
})

ip_lsoa <- ip |>
  inner_join(lsoa_scores, by = join_by(LSOA11 == LSOA_Code)) |>
  group_by(PROCODE3) |>
  summarise(across(imd_score, ~ mean(.x, na.rm = TRUE))) |>
  collect()

ip_diags <- ip |>
  inner_join(ip_diags, by = join_by(EPIKEY)) |>
  filter(DIAGNOSIS %LIKE% "[KR]%") |>
  count(PROCODE3, name = "diags") |>
  collect()

ip_counts <- ip |>
  group_by(PROCODE3) |>
  summarise(
    n = n(),
    age15 = sum(ifelse(ADMIAGE <= 15, 1, 0), na.rm = TRUE),
    age6074 = sum(
      case_when(
        ADMIAGE < 60 ~ 0,
        ADMIAGE >= 75 ~ 0,
        .default = 1
      ),
      na.rm = TRUE
    ),
    ccgs = n_distinct(CCG_RESPONSIBILITY),
    urban = sum(
      case_match(
        RURURB_IND,
        1 ~ 1,
        5 ~ 1,
        .default = 0
      ),
      na.rm = TRUE
    )
  ) |>
  collect()

df <- bind_rows(
  ip_counts |>
    left_join(ip_diags, by = join_by(PROCODE3)) |>
    left_join(ip_lsoa, by = join_by(PROCODE3)) |>
    rename(procode3 = PROCODE3),
  aae_counts,
  op_counts
) |>
  mutate(across(imd_score, ~ .x * n)) |>
  summarise(
    across("diags", ~ sum(.x / n, na.rm = TRUE)),
    across(c("n", "age15", "age6074", "urban", "imd_score"), ~ sum(.x, na.rm = TRUE)),
    across("ccgs", max),
    .by = "procode3"
  ) |>
  mutate(
    across(c("age15", "age6074", "urban", "imd_score"), ~ .x / n),
  ) |>
  inner_join(
    ip_hrg,
    by = join_by(procode3 == PROCODE3)
  ) |>
  inner_join(
    mff,
    by = join_by(procode3 == provider)
  ) |>
  inner_join(
    providers |>
      st_drop_geometry() |>
      mutate(across(distance, as.numeric)),
    by = join_by(procode3 == provider)
  )

df_long <- df |>
  rename(provider_name = name) |>
  pivot_longer(where(is.numeric)) |>
  mutate(across(value, ~ scale(.x)[, 1]), .by = "name")

peers <- df_long |>
  rename(provider = procode3) |>
  inner_join(
    rename(df_long, peer = procode3),
    by = join_by(name),
    relationship = "many-to-many"
  ) |>
  filter(provider != peer) |>
  mutate(value = (value.x - value.y)^2) |>
  summarise(
    across(value, ~ sqrt(sum(.x))),
    .by = c("provider", "peer")
  ) |>
  mutate(rank = dplyr::dense_rank(value), .by = provider) |>
  arrange(provider, rank)

peers |>
  filter(provider == "RL4", peer == "RNA")

df_m <- df |>
  select(-procode3, -name) |>
  as.matrix()
rownames(df_m) <- df$procode3

df_pca <- prcomp(df_m, center = TRUE, scale = TRUE)

km <- kmeans(df_m, 10)
clusters <- enframe(km$cluster, "provider", "cluster")

(df_pca$rotation |>
  as.data.frame() |>
  (\(.x) mutate(.x, column = rownames(.x)))() |>
  as_tibble() |>
  ggplot(aes(PC1, PC2)) +
  geom_segment(aes(xend = 0, yend = 0)) +
  geom_point(aes(text = column)) +
  geom_point(
    data = df_pca |>
      predict(newdata = df_m) |>
      as_tibble() |>
      mutate(provider = rownames(df_m)) |>
      inner_join(
        select(ods_trusts, org_id, name),
        by = join_by(provider == org_id)
      ) |>
      inner_join(
        clusters,
        by = join_by(provider)
      ),
    alpha = 0.2,
    aes(text = name, colour = as.factor(cluster))
  )) |>
  plotly::ggplotly()

tibble(
  pc = paste0("PC", seq_along(df_pca$sdev)),
  variance_explained = (df_pca$sdev^2) / sum(df_pca$sdev^2)
) |>
  mutate(across(pc, fct_rev)) |>
  ggplot(aes(variance_explained, pc)) +
  geom_col()

tibble(
  pc = seq_along(df_pca$sdev),
  variance_explained = df_pca$sdev^2 / sum(df_pca$sdev^2),
  cumulative_variance = cumsum(variance_explained)
) |>
  ggplot(aes(pc, variance_explained)) +
  geom_col(fill = "dodgerblue") +
  geom_line(aes(y = cumulative_variance)) +
  geom_point(aes(y = cumulative_variance))
