library(tidyverse)


fulldat <- fulldat %>% 
  mutate(gender = case_when(gender %in% c(0, "male") ~ "male",
                            gender %in% c("TGW", 3, "tgw", "transgender") ~ "TGW",
                            gender %in% c(1, "female") ~"female",
                            gender == 4 ~ "TGM",
                            gender %in% c(5, "other", "non-binary") ~ "other",
                            TRUE ~ gender)) %>% 
  filter(!(gender %in%  c("A3. Gender"))) %>% 
  mutate(tgw = ifelse(gender == "TGW" | (sex == 0 & gender == "female"), 1, 0),
         tgm_other = ifelse(gender == "TGM" | (sex == 1 & gender %in% c("male", "other") ), 1, 0))

## Next steps
## Define MSM
## Define clients
## define FSW/TGSW/MSW
## Define duration
## Check duration calcs
## Make duration cats
## Define HIV status
## Define age
