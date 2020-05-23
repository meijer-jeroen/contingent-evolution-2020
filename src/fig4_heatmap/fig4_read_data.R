source('src/utility/readMGX.R')

folders <- list.dirs('data/raw/main_experiment/', 
                     full.names = TRUE, 
                     recursive = FALSE)
output_dir <- 'data/temp/'

mgx <- readMGX(folders)

becomes_strong_singleton <-
  c(
    '102',
    '104',
    '106',
    '201',
    '204',
    '206',
    '303',
    '304',
    '305',
    '401',
    '402',
    '404',
    '405',
    '406',
    '407',
    '410',
    '501',
    '502',
    '504',
    '505',
    '601',
    '603',
    '605',
    '702',
    '706',
    '801',
    '802',
    '805',
    '806',
    '901',
    '903',
    '905'
  )

mgx <- mutate(mgx, 
              final_commtype = 
                case_when(seed %in% (as.character(becomes_strong_singleton)) ~ 'Autonomous', 
                          TRUE ~ 'Cross-feeding')
)


# Lets check for every time point how many energy reactions are in the population, 
# and categorize them based as resource-based, building-block based, or both. 
# cut-offs are somewhat arbitrary but don't influence results very much

energy_reactions <- mgx %>% 
  select(-final_commtype) %>% 
  group_by(sim, time_point) %>% 
  select(sim, time_point, starts_with("A"), starts_with('C'), starts_with('E')) %>% 
  select(-c("C-->E+I", "C-->A+I")) %>% 
  gather(reaction, fraction, -sim, -time_point) %>% 
  mutate(reaction_type = case_when(substr(reaction, 1, 1) %in% c('A', 'E') ~ 'building-block', 
                                   substr(reaction, 1, 1) == 'C' ~ 'resource',
                                   TRUE ~ 'something went wrong'
  )
  ) %>% 
  select(-reaction) %>% 
  ungroup() %>% 
  group_by(sim, time_point, reaction_type) %>% 
  slice(which.max(fraction)) 

energy_reactions <- 
  energy_reactions %>% 
  spread(reaction_type, fraction)

energy_reactions <- mutate(energy_reactions, ene_type = 
                             case_when(`building-block` > 0.4 & resource > 0.4 ~ 'hybrid',
                                       `building-block` > 0.4 ~ 'building block', 
                                       resource > 0.4 ~ 'resource', 
                                       TRUE ~ 'BB and resource energy types both < 0.4')
)

mgx$ene_type <- energy_reactions$ene_type
rm(energy_reactions)

imp_reactions <- mgx %>% 
  select(-final_commtype) %>% 
  group_by(sim, time_point) %>% 
  select(sim, time_point, `I*-->Aimport`, `I*-->Eimport`) %>% 
  gather(reaction, fraction, -sim, -time_point) %>% 
  ungroup() %>% 
  spread(reaction, fraction) %>% 
  mutate(cur_commtype = case_when(`I*-->Aimport` > 0.8 & `I*-->Eimport` > 0.8 ~ 'autonomous',
                                `I*-->Aimport` < 0.2 & `I*-->Eimport` < 0.2 ~ 'missing importers',
                                TRUE ~ 'cross-feeding')
  )

mgx$cur_commtype <- imp_reactions$cur_commtype
rm(imp_reactions)

# Label most abundant energy reaction
energy_reactions <- mgx %>%
  group_by(sim, time_point) %>%
  select(sim, time_point, starts_with("A"), starts_with('C'), starts_with('E')) %>%
  select(-c("C-->E+I", "C-->A+I")) %>%
  gather(reaction, fraction, -sim, -time_point) %>%
  slice(which.max(fraction))

mgx$ene_react <- energy_reactions$reaction
rm(energy_reactions)



mgx <- select(mgx, time_point, seed, sim, population, cur_commtype, final_commtype, ene_type, ene_react, everything())
cat('saving data \n')
save(file = paste0(output_dir, 'fig4.rdata'), mgx)
