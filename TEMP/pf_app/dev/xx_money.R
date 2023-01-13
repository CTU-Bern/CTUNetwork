
# devtools::load_all()
pf::update_pf()
library(pf)
all_tabs <- getPFData()

library(tidyverse)

start_date <- as.Date("2021-01-01")
end_date <- as.Date("2021-12-31")
end_date <- Sys.Date()

all_tabs <- lapply(all_tabs, discard_all_NA)

dat <- prepTime(all_tabs) %>% 
  group_by(projnum, ctu_division) %>% 
  mutate(rate = max(UNITPRICE, na.rm = TRUE)) %>% 
  filter(str_detect(proj, "P|FTE")) %>% 
  arrange(proj, projnum, ctu_division, q_in_year) %>% 
  select(proj, projnum, ctu_division, year, q_in_year, rate, Timespent) %>% 
  filter(year == 2021) 
  summarize(Timespent = sum(Timespent),
            SUMNETTO = sum(SUMNETTO, na.rm = TRUE),
            rate = mean(UNITPRICE, na.rm = TRUE))


all_tabs %>% 
  "[["("financeposition") %>% 
  left_join()


x <- all_tabs %>% 
  "[["("activitydata") %>% 
  left_join(all_tabs %>% 
              "[["("financeposition") %>% 
              select(FK_PROJECT, UNITPRICE)) %>% 
  left_join(constructProjectParents(all_tabs$project),
            by = c("FK_PROJECT" = "PK_Project")) %>% 
  left_join(all_tabs %>% 
              "[["("project") %>% 
              select(PK_Project, ISLEAF),
            by = c("FK_PROJECT" = "PK_Project")) %>% 
  mutate(year = lubridate::year(BookedDate),
         quarter_in_year = lubridate::quarter(BookedDate)
         )

x <- prepTime(all_tabs) %>% 
  mutate(money = billable_time / 60 * UNITPRICE)

y <- prepTime(all_tabs) %>% 
  mutate(money = billable_time / 60 * UNITPRICE) %>% 
  left_join(all_tabs$project %>% select(cf_applicable_ordinance_2, PK_Project),
            by = c("top_project" = "PK_Project")) %>% 
  mutate(ordnance = str_replace(cf_applicable_ordinance_2, "If ", ""),
         ordnance = str_replace(ordnance, "/.{1,}$", "")) %>% 
  group_by(ctu_projectName, projnum, ctu_division, projecttype, year, q_in_year, billable, ordnance) %>% 
  summarize(across(c(billable_time, nonbillable_time, money), sum, na.rm = TRUE),
            ) %>% 
  mutate(across(ends_with("time"), ~ .x / 60)) %>% 
  filter(year > 2019) %>% 
  left_join(projectSize(all_tabs)) %>%
  mutate(quarter = year + ((1/4 * q_in_year) - 0.25)) %>% 
  ungroup() %>% 
  mutate(ordnance = ifelse(!is.na(ordnance), ordnance, "unknown"))

z <- y %>% 
  group_by(ctu_division, projecttype, year, q_in_year, quarter, billable, ordnance, size) %>% 
  summarize(across(c(billable_time, nonbillable_time, money), sum, na.rm = TRUE),
            n = nrow(.data))

z  %>% 
  ggplot(aes(x = quarter, y = money, fill = ctu_division, alpha = ordnance)) +
  geom_col() +
  coord_flip() +
  scale_alpha_manual(values = c(1, .8, .6, .4)) +
  guides(alpha = guide_legend(title = "Ordnance", nrow = 2, byrow = TRUE, reverse = TRUE),
         fill = guide_legend(title = "Division", nrow = 1, byrow = TRUE, reverse = TRUE)) +
  theme(panel.background = element_rect(fill = NA),
        legend.position = "bottom", 
        legend.box = "vertical")
z  %>% 
  ggplot(aes(x = quarter, y = money, fill = ctu_division, alpha = size)) +
  geom_col() +
  coord_flip() +
  scale_alpha_manual(values = c(1, .8, .6)) +
  guides(alpha = guide_legend(title = "Size", nrow = 1, byrow = TRUE, reverse = TRUE),
         fill = guide_legend(title = "Division", nrow = 1, byrow = TRUE, reverse = TRUE)) +
  theme(panel.background = element_rect(fill = NA),
        legend.position = "bottom", 
        legend.box = "vertical")


# Ordnance needs adding

y %>% 
  group_by(year, q_in_year, ctu_division, size)
summarise



y %>% 
  filter(projecttype == "Consulting") %>% 
  ggplot(aes(x = year + q_in_year / 4 - .25, y = nonbillable_time, fill = ctu_projectName)) +
  geom_bar(position="stack", stat="identity") +
  guides(fill = guide_none())


# hours booked at the wrong level...
x <- all_tabs %>% 
  "[["("activitydata") %>% 
  left_join(all_tabs %>% 
              "[["("financeposition") %>% 
              select(FK_PROJECT, UNITPRICE)) %>% 
  left_join(constructProjectParents(all_tabs$project),
            by = c("FK_PROJECT" = "PK_Project")) %>% 
  left_join(all_tabs %>% 
              "[["("project") %>% 
              select(PK_Project, ISLEAF),
            by = c("FK_PROJECT" = "PK_Project")) %>% 
  filter(ISLEAF == 0) %>% 
  select(matches("name"), Timespent, BookedDate)

x %>% 
  group_by(ctu_projectName) %>% 
  summarise(Timespent = sum(Timespent), 
            start = min(BookedDate),
            end = max(BookedDate)) %>% 
  mutate(Timespent = Timespent / 60)





all_tabs %>% 
  "[["("project") %>% 
  left_join(constructProjectParents(all_tabs %>% 
                                      "[["("project"))) %>% 
  filter(str_detect(ctu_projectName, "STREAM")) %>% 
  select(ISLEAF, matches("name")) %>% 
  View()
  names()



# financerecord - invoices
# financeposition - items on invoices, workpackages?
# financearticle - list of article types in PF
# 


findvar(all_tabs, "parent")
findvar(all_tabs, "financearticle")

all_tabs$financepositiongroup %>% View()

finart_parent <- constructFinanceArticleParents(all_tabs$financearticle)
all_tabs$financearticle %>% 
  dplyr::left_join(finart_parent) %>% 
  discard_all_NA() %>% View


xx <- all_tabs$financeposition %>% 
  discard_all_NA() %>% 
  left_join(constructProjectParents(all_tabs$project),
            by = c("FK_PROJECT" = "PK_Project")) %>% 
  left_join(all_tabs$financerecord %>% 
              discard_all_NA() %>% 
              select(PK_FINANCERECORD, FK_PROJECT),
            by = c("FK_FINANCERECORD" = "PK_FINANCERECORD")) %>% 
  left_join(constructProjectParents(all_tabs$project) %>% 
              rename_with(function(x) paste0("p1_", x)) %>% 
              select(p1_PK_Project, p1_ProjectName),
            by = c("FK_PROJECT.y" = "p1_PK_Project")) %>%
  left_join(all_tabs$project %>% 
              select(PK_Project, CaseId, FK_CUSTOMER),
            by = c("FK_PROJECT.y" = "PK_Project")) %>% 
  mutate(projType = str_extract(CaseId, "^.{1,}(?=-)"),
         create_year = lubridate::year(CREATEDATE),
         create_quarter = lubridate::quarter(CREATEDATE),
         created = create_year + create_quarter / 4) %>% 
  group_by(CaseId, projType, create_year, create_quarter, created, ctu_division) %>% 
  summarize(brutto = sum(SUMBRUTTO),
            netto = sum(SUMNETTO)) %>% 
  filter(created > 2021.25)
  View()

all_tabs$financerecord %>% 
  discard_all_NA() %>% 
  View()

