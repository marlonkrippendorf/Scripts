rm(list=ls())

# load necessary packages
library("tidyverse")
library("sf")
library("haven")
library("modelsummary")
library("ggpubr")

# define folders
alloc <- "C:\\Users\\marlo\\Documents\\PhDEQUALFIN\\Chapter_1_Allocation"
allocoutput <- "C:\\Users\\marlo\\Documents\\PhDEQUALFIN\\Chapter_1_Allocation\\Allocation-Outputs"
world <- "C:\\Users\\marlo\\Documents\\PhDEQUALFIN\\Datasets\\Political_Boundaries__States_and_Provinces"

# load data files 
godad_finvar <- read_stata(paste0(alloc,"/godad_riomarkers_ethnicity_panel_unsc_plad_finvar.dta")) # load final godad stata file
worldshp <- st_read(paste0(world,"/Political_Boundaries%3A_States_and_Provinces.shp")) # load world shapefile

# join the two datasets based on isocode
godad_finvar <- rename(godad_finvar, ISO_CODE = iso_code)
godad_finvar <- godad_finvar %>%
  mutate(ISO_CODE = haven::as_factor(ISO_CODE))
godad_finvar <- mutate(godad_finvar, ISO_CODE = as.character((ISO_CODE))) # both iso_codes need to be in character format
godad_finvar_geo <- worldshp %>%
  left_join(godad_finvar %>% 
      select(
        ISO_CODE, paymentyear, project_count, disb, project_count_start, comm,
        ident_region_leaderbirth, ident_region_leaderethn,
        eur_disb, ger_disb, eur_comm, eur_count_comm,
        aus_disb, bel_disb, den_disb, fin_disb, fra_disb, gre_disb, ice_disb,
        ire_disb, ita_disb, lux_disb, net_disb, nor_disb, por_disb, spa_disb,
        swe_disb, swi_disb, uk_disb
      ),
  by = "ISO_CODE")
godad_finvar_geo <- st_as_sf(godad_finvar_geo) # convert godad_finvar_geo to an sf object
godad_finvar_geo <- st_transform(godad_finvar_geo, crs = "+proj=robin")

godad_finvar_geo <- mutate(godad_finvar_geo, l_disb = log(disb))
godad_finvar_geo <- mutate(godad_finvar_geo, l_eur_disb = log(eur_disb))
godad_finvar_geo <- mutate(godad_finvar_geo, paymentyear_f = factor(paymentyear)) # factor variable for paymentyear

#################################################################################################
### HISTOGRAMS OF INDIVIDUAL PROJECT DISBURSEMENTS OVER TIME ####################################
#################################################################################################

# load dataset on unique projects
godad_uniqueproj <- read_stata(paste0(alloc,"/godad_riomarkers_ethnicity_uniqueproj.dta"))
godad_uniqueproj <- filter(godad_uniqueproj, donor != "United States")
paymentyears <- c(min(godad_uniqueproj$paymentyear):max(godad_uniqueproj$paymentyear))
paymentyear_f <- factor(godad_uniqueproj$paymentyear, levels = paymentyears, labels = paymentyears)

summary(godad_uniqueproj$disb)
table(godad_uniqueproj$paymentyear)
table(godad_uniqueproj$donor)

sum_disb <- function(x) {
  tot_sum <- sum(x, na.rm = TRUE)
  return(tot_sum) 
}

datasummary(
  paymentyear_f*disb ~ Mean + Median + SD + Min + Max + P25 + P75 + N + sum_disb + PercentMissing,
  data = godad_uniqueproj,
  fmt = fmt_decimal(digits = 0),
  output = "markdown"
)


hist_list_1 <- list()
for (y in 1989:2004) {
  max_disb_y <- max(godad_uniqueproj$disb[godad_uniqueproj$paymentyear == y], na.rm = TRUE)
  hist_y <- ggplot(data=filter(godad_uniqueproj, paymentyear==y)) +
            geom_histogram(aes(x=disb), bins=100) +
            scale_x_continuous(
              limits = c(range(godad_uniqueproj$disb[godad_uniqueproj$paymentyear == y])),
              labels = scales::label_number(scale = 1e-6, suffix = "M", big.mark = ",")
            ) +
            scale_y_continuous(limits = c(0, 2000)) +
            geom_vline(xintercept = max_disb_y, linetype = "dashed", color = "#313030", linewidth = 0.3) +
            labs(title=paste("Disbursements in", y), x="Disbursements (USD)", y="Count")
  hist_list_1[[as.character(y)]] <- hist_y
}

gg_hist_1 <- ggarrange(plotlist = hist_list_1 ,
          ncol = 4,
          nrow = ceiling(length(hist_list_1) / 4))

ggsave(paste0(allocoutput,"/disbursement_histograms_1989_2004.png"),
       plot = gg_hist_1,
       width = 10, height = 7, units = "in", dpi = 300)

hist_list_2 <- list()
for (y in 2005:2021) {
  max_disb_y <- max(godad_uniqueproj$disb[godad_uniqueproj$paymentyear == y], na.rm = TRUE)
  hist_y <- ggplot(data=filter(godad_uniqueproj, paymentyear==y)) +
            geom_histogram(aes(x=disb), bins=100) +
            scale_x_continuous(
              limits = c(range(godad_uniqueproj$disb[godad_uniqueproj$paymentyear == y])),
              labels = scales::label_number(scale = 1e-6, suffix = "M", big.mark = ",")
            ) +
            scale_y_continuous(limits = c(0, 2000)) +
            geom_vline(xintercept = max_disb_y, linetype = "dashed", color = "#313030", linewidth = 0.3) +
            labs(title=paste("Disbursements in", y), x="Disbursements (USD)", y="Count")
  hist_list_2[[as.character(y)]] <- hist_y
}

gg_hist_2 <- ggarrange(plotlist = hist_list_2 ,
          ncol = 4,
          nrow = ceiling(length(hist_list_2) / 4))

ggsave(paste0(allocoutput,"/disbursement_histograms_2005_2021.png"),
       plot = gg_hist_2,
       width = 10, height = 7, units = "in", dpi = 300)



# add descriptive statistics of main variables
# total disbursements
datasummary(paymentyear_f * eur_disb + project_count + project_count_start + comm + ident_region_leaderbirth + ident_region_leaderethn + eur_disb + eur_comm + eur_count_comm ~
  Sum + Mean + Median + SD + Min + Max + P25 + P75 + N + PercentMissing, data = godad_finvar_geo, output = "markdown")
# German disbursements
datasummary(paymentyear_f * ger_disb + project_count + project_count_start + comm + ident_region_leaderbirth + ident_region_leaderethn + eur_disb + eur_comm + eur_count_comm ~
  Sum + Mean + Median + SD + Min + Max + P25 + P75 + N + PercentMissing, data = godad_finvar_geo, output = "markdown")

#################################################################################################
### TWO GRAPHS OF DISBURSEMENTS OVER TIME BY DONOR COUNTRY ######################################
#################################################################################################

# WITH AGGREGATE EUROPE
graph_donors_time <-ggplot(data=godad_finvar_geo %>% filter(paymentyear >= 1989)) +
  stat_summary(aes(x=paymentyear_f, y=eur_disb, color="Europe"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=eur_disb, color="Europe"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=aus_disb, color="Austria"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=aus_disb, color="Austria"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=bel_disb, color="Belgium"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=bel_disb, color="Belgium"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=den_disb, color="Denmark"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=den_disb, color="Denmark"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=fin_disb, color="Finland"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=fin_disb, color="Finland"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=fra_disb, color="France"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=fra_disb, color="France"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=ger_disb, color="Germany"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=ger_disb, color="Germany"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=gre_disb, color="Greece"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=gre_disb, color="Greece"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=ice_disb, color="Iceland"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=ice_disb, color="Iceland"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=ire_disb, color="Ireland"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=ire_disb, color="Ireland"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=ita_disb, color="Italy"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=ita_disb, color="Italy"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=lux_disb, color="Luxembourg"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=lux_disb, color="Luxembourg"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=net_disb, color="Netherlands"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=net_disb, color="Netherlands"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=nor_disb, color="Norway"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=nor_disb, color="Norway"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=por_disb, color="Portugal"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=por_disb, color="Portugal"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=spa_disb, color="Spain"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=spa_disb, color="Spain"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=swe_disb, color="Sweden"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=swe_disb, color="Sweden"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=swi_disb, color="Switzerland"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=swi_disb, color="Switzerland"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=uk_disb, color="United Kingdom"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=uk_disb, color="United Kingdom"), fun=sum, geom="line", group=1, linewidth=1) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::breaks_width(1e9),
    labels = scales::label_number(scale = 1e-9, suffix = "B", big.mark = ",")
  ) +
  scale_color_manual(values = c(
    "Europe" = "#1f77b4",
    "Austria" = "#ff7f0e",
    "Belgium" = "#2ca02c",
    "Denmark" = "#d62728",
    "Finland" = "#9467bd",
    "France" = "#8c564b",
    "Germany" = "#e377c2",
    "Greece" = "#7f7f7f",
    "Iceland" = "#bcbd22",
    "Ireland" = "#17becf",
    "Italy" = "#aec7e8",
    "Luxembourg" = "#ffbb78",
    "Netherlands" = "#98df8a",
    "Norway" = "#ff9896",
    "Portugal" = "#c5b0d5",
    "Spain" = "#c49c94",
    "Sweden" = "#f7b6d2",
    "Switzerland" = "#c7c7c7",
    "United Kingdom" = "#dbdb8d"
  )) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = c(0.10, 0.60),
        legend.background = element_rect(fill = "white", color = "black")) +
    labs(title="European Disbursements By Donor Country Over Time, in Billion USD",
       x="Payment Year",
      y="Disbursements (USD, billions)",
       color="Donor")

ggsave(paste0(allocoutput,"/disb_donors_time.png"),
      plot = graph_donors_time,
      width = 10, height = 7, units = "in", dpi = 300)

# WITHOUT AGGREGATE EUROPE
graph_donors_time_excl_eu <-ggplot(data=godad_finvar_geo %>% filter(paymentyear >= 1989)) +
  stat_summary(aes(x=paymentyear_f, y=aus_disb, color="Austria"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=aus_disb, color="Austria"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=bel_disb, color="Belgium"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=bel_disb, color="Belgium"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=den_disb, color="Denmark"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=den_disb, color="Denmark"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=fin_disb, color="Finland"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=fin_disb, color="Finland"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=fra_disb, color="France"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=fra_disb, color="France"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=ger_disb, color="Germany"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=ger_disb, color="Germany"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=gre_disb, color="Greece"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=gre_disb, color="Greece"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=ice_disb, color="Iceland"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=ice_disb, color="Iceland"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=ire_disb, color="Ireland"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=ire_disb, color="Ireland"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=ita_disb, color="Italy"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=ita_disb, color="Italy"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=lux_disb, color="Luxembourg"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=lux_disb, color="Luxembourg"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=net_disb, color="Netherlands"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=net_disb, color="Netherlands"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=nor_disb, color="Norway"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=nor_disb, color="Norway"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=por_disb, color="Portugal"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=por_disb, color="Portugal"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=spa_disb, color="Spain"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=spa_disb, color="Spain"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=swe_disb, color="Sweden"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=swe_disb, color="Sweden"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=swi_disb, color="Switzerland"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=swi_disb, color="Switzerland"), fun=sum, geom="line", group=1, linewidth=1) +
  stat_summary(aes(x=paymentyear_f, y=uk_disb, color="United Kingdom"), fun=sum, geom="point", size=3) +
  stat_summary(aes(x=paymentyear_f, y=uk_disb, color="United Kingdom"), fun=sum, geom="line", group=1, linewidth=1) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = scales::breaks_width(1e9),
    labels = scales::label_number(scale = 1e-9, suffix = "B", big.mark = ",")
  ) +
  scale_color_manual(values = c(
    "Austria" = "#ff7f0e",
    "Belgium" = "#2ca02c",
    "Denmark" = "#d62728",
    "Finland" = "#9467bd",
    "France" = "#8c564b",
    "Germany" = "#e377c2",
    "Greece" = "#7f7f7f",
    "Iceland" = "#bcbd22",
    "Ireland" = "#17becf",
    "Italy" = "#aec7e8",
    "Luxembourg" = "#ffbb78",
    "Netherlands" = "#98df8a",
    "Norway" = "#ff9896",
    "Portugal" = "#c5b0d5",
    "Spain" = "#c49c94",
    "Sweden" = "#f7b6d2",
    "Switzerland" = "#c7c7c7",
    "United Kingdom" = "#dbdb8d"
  )) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = c(0.10, 0.60),
        legend.background = element_rect(fill = "white", color = "black")) +
    labs(title="European Disbursements By Donor Country Over Time, in Billion USD",
       x="Payment Year",
      y="Disbursements (USD, billions)",
       color="Donor")

ggsave(paste0(allocoutput,"/disb_donors_time_excl_eu.png"),
      plot = graph_donors_time_excl_eu,
      width = 10, height = 7, units = "in", dpi = 300)
graph_donors_time_excl_eu

#################################################################################################
### SPATIAL MAPS OF VARIOUS VARIABLES ###########################################################
#################################################################################################

### plot log disbursements for 2015
plot <- godad_finvar_geo %>% filter(paymentyear == 2015) %>% ggplot() + 
  geom_sf(aes(fill = l_disb), color = NA) +
  scale_fill_gradient(low = "red", high = "green", na.value = "lightgrey") +
  theme_minimal() +
    labs(title = "Disbursements by Region",
       fill = "Disbursements (USD)")

### plot log disbursements for European donors for 2010, 2015, 2020
# Create country-level borders by dissolving provinces
world_country_borders <- worldshp %>% 
  group_by(COUNTRY) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_transform(crs = "+proj=robin")

# plot graphs: 
for (y in c(2010, 2015, 2020)) {
  plot_y <- ggplot() + 
    geom_sf(data = worldshp %>% st_transform(crs = "+proj=robin"), 
            fill = "lightgrey", color = "white", linewidth = 0.1) +
    geom_sf(data = godad_finvar_geo %>% filter(paymentyear == y), 
            aes(fill = l_eur_disb), color = NA) +
    geom_sf(data = world_country_borders,
            fill = NA, color = "#141414", linewidth = 0.01) +
    scale_fill_gradient(low = "red", high = "green", na.value = "lightgrey") +
    theme_minimal() +
    labs(title = paste("European Disbursements by Region in", y),
         fill = "Log Disbursements (USD)")
  ggsave(paste0(allocoutput,"/eur_disb_map_",y,".png"),
         plot = plot_y,
         width = 10, height = 7, units = "in", dpi = 300)
}



# Then in the plot:
plot <- ggplot() + 
  geom_sf(data = worldshp %>% st_transform(crs = "+proj=robin"), 
          fill = "lightgrey", color = NA) +
  geom_sf(data = godad_finvar_geo %>% filter(paymentyear == 2015), 
          aes(fill = l_disb), color = NA) +
  geom_sf(data = world_country_borders,
          fill = NA, color = "#141414", linewidth = 0.01) +
  scale_fill_gradient(low = "red", high = "green", na.value = "lightgrey") +
  theme_minimal() +
  labs(title = "Disbursements by Region in 2015",
       fill = "Log Disbursements (USD)")

ggsave(paste0(allocoutput,"/eur_disb_map_2015_borders.png"),
       plot = plot,
       width = 10, height = 7, units = "in", dpi = 300)



head(worldshp)
