library(dplyr)
elec <- tibble(
  cycle = c('Aug 13, 2020 to Sep 14, 2020',
            'Sep 14, 2020 to Oct 14, 2020',
            'Oct 14, 2020 to Nov 12, 2020',
            'Nov 12, 2020 to Dec 15, 2020',
            'Dec 15, 2020 to Jan 15, 2021',
            'Jan 15, 2021 to Feb 17, 2021',
            'Feb 17, 2021 to Mar 18, 2021',
            'Mar 18, 2021 to Apr 16, 2021',
            'Apr 16, 2021 to May 14, 2021'),
  supply = c(28.13,
             6.86,
             11.05,
             9.76,
             13.30,
             16.29,
             8.44,
             12.12,
             13.18),
  delivery = c(63.42,
               13.25,
               17.55,
               22.31,
               24.55,
               23.99,
               18.84,
               20.51,
               20.62),
  total = c(140.73,
            56.04,
            64.80,
            73.10,
            77.61,
            82.39,
            63.42,
            69.41,
            69.68))

elec1 <- elec %>%
  filter(!grepl('aug',cycle,ignore.case = TRUE)) %>%
  mutate(sply_dlv_aftertax = (supply + delivery)* (1+8.875/100),
         share_elec_yaya = (total - sply_dlv_aftertax) / 2) 

elec2 <- elec %>%
  filter(grepl('aug',cycle,ignore.case = TRUE)) %>%
  mutate(sply_dlv_aftertax = (supply + delivery)* (1+8.875/100),
         share_elec_yaya = (total - sply_dlv_aftertax) / 3,
         share_elec_hogaga = sply_dlv_aftertax/2*10/32 + share_elec_yaya) %>%
  bind_rows(elec1) %>%
  janitor::adorn_totals('row')
  
  
