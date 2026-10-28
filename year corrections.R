### Fixing observations that have the wrong years (checked with Agouti) ###

library(dplyr)
library(lubridate)

observations_check <- observations %>%
  left_join(
    deployments %>%
      dplyr::select(deploymentID, deploymentStart, deploymentEnd),
    by = "deploymentID"
  ) %>%
  mutate(
    eventStart_dt = ymd_hms(eventStart),
    eventEnd_dt = ymd_hms(eventEnd),
    deploymentStart_dt = ymd_hms(deploymentStart),
    deploymentEnd_dt = ymd_hms(deploymentEnd)
  )

observations_check <- observations_check %>%
  mutate(
    event_within_deployment =
      eventStart_dt >= deploymentStart_dt &
      eventEnd_dt <= deploymentEnd_dt
  )

wrong_obs <- observations_check %>%
  filter(event_within_deployment == FALSE) %>%
  dplyr::select(
    deploymentID,
    locationName,
    deploymentStart,
    deploymentEnd,
    eventStart,
    eventEnd,
    event_within_deployment
  )

write_xlsx(
  wrong_obs,
  "wrong_obs.xlsx"
)

n_distinct(wrong_obs$deploymentID)

year_corrections <- tibble::tribble(
  ~deploymentID, ~start_year, ~end_year,
  "2f3e2471-1963-445c-a3d4-9c9dde8a8e94",      2023,        2023,
  "31fd6467-8a46-47a3-a356-0dcde8c77f76",      2024,        2024, 
  "d2df5af9-4cfe-4fcd-b018-000c049cc98e",      2023,        2023,
  "0018b853-4223-4fdb-91dc-f00d40ef44ad",      2026,        2026,
  "26a94005-fc52-4a9c-9772-211dce0dbb38",      2026,        2026,
  "2ac7565f-3b22-4109-934f-19b9c7a9ccd1",      2026,        2026,
  "6956b530-1370-42f2-b503-5eef40f12762",      2026,        2026,
  "818ef385-a2c4-45b4-a6b1-4630a97a9b8f",      2026,        2026,
  "9a5d76a7-a376-4dce-b801-1d2f3ee2bd56",      2026,        2026,
  "b80fbb99-36a1-4879-8788-88ef1d76115d",      2026,        2026,
  "d9937756-debe-459d-ac1f-6c3bca53b829",      2026,        2026,
  "ea859d96-3c7b-4030-8a89-2f7ea1c2483e",      2026,        2026,
  "250bc60d-9aaa-4041-8075-716ff435a6b1",      2026,        2026,
)

names(year_corrections)
year_corrections
names(observations)

observations <- observations %>%
  left_join(
    year_corrections,
    by = "deploymentID"
  ) %>%
  mutate(
    eventStart = if_else(
      !is.na(start_year),
      format(
        update(ymd_hms(eventStart), year = start_year),
        "%Y-%m-%dT%H:%M:%S%z"
      ),
      eventStart
    ),
    eventEnd = if_else(
      !is.na(end_year),
      format(
        update(ymd_hms(eventEnd), year = end_year),
        "%Y-%m-%dT%H:%M:%S%z"
      ),
      eventEnd
    )
  ) %>%
  dplyr::select(-start_year, -end_year)

observations %>%
  filter(deploymentID == "ea859d96-3c7b-4030-8a89-2f7ea1c2483e") %>%
  dplyr::select(deploymentID, eventStart, eventEnd)

wrong_CAR7 <- observations %>%
  filter(deploymentID == "31fd6467-8a46-47a3-a356-0dcde8c77f76") %>%
  dplyr::select(deploymentID, eventStart, eventEnd)

write_xlsx(
  wrong_CAR7,
  "wrong_CAR7.xlsx"
)

# check if worked 
observations_doublecheck <- observations %>%
  left_join(
    deployments %>%
      dplyr::select(deploymentID, deploymentStart, deploymentEnd),
    by = "deploymentID"
  ) %>%
  mutate(
    eventStart_dt = ymd_hms(eventStart),
    eventEnd_dt = ymd_hms(eventEnd),
    deploymentStart_dt = ymd_hms(deploymentStart),
    deploymentEnd_dt = ymd_hms(deploymentEnd)
  )

observations_doublecheck <- observations_doublecheck %>%
  mutate(
    event_within_deployment =
      eventStart_dt >= deploymentStart_dt &
      eventEnd_dt <= deploymentEnd_dt
  )

wrong_obs_part2 <- observations_doublecheck %>%
  filter(event_within_deployment == FALSE) %>%
  dplyr::select(
    deploymentID,
    locationName,
    deploymentStart,
    deploymentEnd,
    eventStart,
    eventEnd,
    event_within_deployment
  )

write_xlsx(
  wrong_obs_part2,
  "wrong_obs_part2.xlsx"
)
