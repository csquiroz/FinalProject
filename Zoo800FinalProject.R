### Zoo 800 - Final Project ###
### Cody Quiroz ###
### 15-December-2025 ###

# Loading Packages & Data -------------------------------------------------

#load packages
library(dplyr)
library(tidyr)
library(ggplot2)

#load data
data <- read.csv("DamDataBulk.csv") %>%
  mutate( #add small constants to remove zeros as they are BDL
    CH4.uM  = CH4.uM + 0.001,
    CO2.uM.25  = CO2.uM.25 + 0.001,
    CO2.uM = CO2.uM + 0.001,
    N2O.nM      = N2O.uM * 1000, #for better visuals, convert N2O from uM to nM
    N2O.nM.25   = N2O.uM.25 * 1000,
    N2O.nM.75   = N2O.uM.75 * 1000,
    Location    = factor(Location, levels = c("Upstream", "Downstream", "Post-Dam"))
  )


# CO2 Differences ---------------------------------------------------------
#CO2
paired_data_co2 <- data %>% #crating paired df
  dplyr::select(Site, Date, Location, CO2.uM) %>%
  tidyr::pivot_wider(
    names_from = Location,
    values_from = CO2.uM
  ) %>%
  tidyr::drop_na(Upstream, Downstream)

#calculate differences
diffs_co2 <- paired_data_co2$Downstream - paired_data_co2$Upstream

#normality checks
shapiro.test(diffs_co2) #p <0.05 so not normal, use Wilcoxon signed rank
qqnorm(diffs_co2) #QQ plot to visually confirm non-normality
qqline(diffs_co2, col = "red")

#Wilcoxon signed rank
wilcox.test(
  paired_data_co2$Upstream,
  paired_data_co2$Downstream,
  paired = TRUE
) #p=0.008308

#plot histogram
ggplot(data.frame(diffs = diffs_co2), aes(x = diffs)) +
  geom_histogram(
    aes(fill = after_stat(ifelse(x < 0, "Upstream", "Impoundment"))),
    binwidth = 20,
    boundary = 0,
    closed = "left",
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Upstream" = "turquoise3",
      "Impoundment" = "olivedrab3"
    ),
    name = expression(CO[2]~(mu*M)~Higher~Within)
  ) +
  geom_vline(
    xintercept = 0,
    color = "red",
    linewidth = 2,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = seq(
      floor(min(diffs_co2, na.rm = TRUE) / 20) * 20,
      ceiling(max(diffs_co2, na.rm = TRUE) / 20) * 20,
      by = 20
    )
  ) +
  labs(
    x = expression(Dissolved~CO[2]~difference~(mu*M)),
    y = "Count"
  ) +
  annotate(
    "text",
    x = max(diffs_co2, na.rm = TRUE),   # top-right x
    y = max(hist(diffs_co2, plot = FALSE)$counts), # top y
    label = "p = 0.0083",
    hjust = 0.5, 
    vjust = 1,
    size = 5
  )+
  theme_classic()


# CH4 Differences ---------------------------------------------------------
#create paired df for methane
paired_data_CH4 <- data %>%
  dplyr::select(Site, Date, Location, CH4.uM) %>%
  tidyr::pivot_wider(
    names_from = Location,
    values_from = CH4.uM
  ) %>%
  tidyr::drop_na(Upstream, Downstream)

#calculate diffs
diffs.ch4 <- paired_data_CH4$Downstream - paired_data_CH4$Upstream

shapiro.test(diffs.ch4) #p < 0.05 so not normal
qqnorm(diffs.ch4) #QQ plot to visually confirm non-normality
qqline(diffs.ch4, col = "red")

wilcox.test(
  paired_data_CH4$Upstream,
  paired_data_CH4$Downstream,
  paired = TRUE
) #p=0.005581

#plot ch4 diffs histogram
ggplot(data.frame(diffs = diffs.ch4), aes(x = diffs)) +
  geom_histogram(
    aes(fill = after_stat(ifelse(x < 0, "Upstream", "Impoundment"))),
    binwidth = 0.25,
    boundary = 0,
    closed = "left",
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Upstream" = "turquoise3",
      "Impoundment" = "olivedrab3"
    ),
    name = expression(CH[4]~(mu*M)~Higher~Within)
  ) +
  geom_vline(
    xintercept = 0,
    color = "red",
    linewidth = 2,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = seq(
      floor(min(diffs.ch4, na.rm = TRUE)),
      ceiling(max(diffs.ch4, na.rm = TRUE)),
      by = 0.5
    )
  ) +
  labs(
    x = expression(Dissolved~CH[4]~difference~(mu*M)),
    y = "Count"
  ) +
  annotate(
    "text",
    x = max(diffs.ch4, na.rm = TRUE),   # top-right x
    y = max(hist(diffs.ch4, plot = FALSE)$counts), # top y
    label = "p = 0.0056",
    hjust = 0.5, 
    vjust = 1,
    size = 5
  )+
  theme_classic()


# N2O Differences ---------------------------------------------------------
#make n2o diffs df
paired_data_n2o <- data %>%
  dplyr::select(Site, Date, Location, N2O.nM) %>%
  tidyr::pivot_wider(
    names_from = Location,
    values_from = N2O.nM
  ) %>%
  tidyr::drop_na(Upstream, Downstream)

#calc n2o diffs
diffs_n2o <- paired_data_n2o$Downstream - paired_data_n2o$Upstream

#normality check
shapiro.test(diffs_n2o)
qqnorm(diffs_n2o) #QQ plot to visually confirm non-normality
qqline(diffs_n2o, col = "red")

wilcox.test(
  paired_data_n2o$Upstream,
  paired_data_n2o$Downstream,
  paired = TRUE
)

#make histogram of diffs
ggplot(data.frame(diffs = diffs_n2o), aes(x = diffs)) +
  geom_histogram(
    aes(fill = after_stat(ifelse(x < 0, "Upstream", "Impoundment"))),
    binwidth = 20,
    boundary = 0,
    closed = "left",
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Upstream" = "turquoise3",
      "Impoundment" = "olivedrab3"
    ),
    name = expression(N[2]*O~(nM)~Higher~Within)
  ) +
  geom_vline(
    xintercept = 0,
    color = "red",
    linewidth = 2,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = seq(
      floor(min(diffs_n2o, na.rm = TRUE) / 20) * 20,
      ceiling(max(diffs_n2o, na.rm = TRUE) / 20) * 20,
      by = 20
    )
  ) +
  labs(
    x = expression(Dissolved~N[2]*O~difference~(nM)),
    y = "Count"
  ) +
  scale_y_continuous(breaks = function(x) seq(0, ceiling(max(x)), by = 2))+
  annotate(
    "text",
    x = max(diffs_n2o, na.rm = TRUE),   # top-right x
    y = max(hist(diffs_n2o, plot = FALSE)$counts), # top y
    label = "p = 0.036",
    hjust = 0.5, 
    vjust = 1,
    size = 5
  )+
  theme_classic()




# CO2 Diffs - Spatial -----------------------------------------------------
#1/4 distance across samples
paired_data_CO2.25 <- data %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  dplyr::select(Site, Date, Location, CO2.uM, CO2.uM.25) %>%
  pivot_wider(
    names_from = Location,
    values_from = c(CO2.uM, CO2.uM.25)
  ) %>%
  transmute(
    Site,
    Date,
    Upstream   = CO2.uM_Upstream,
    Downstream = CO2.uM.25_Downstream
  ) %>%
  drop_na(Upstream, Downstream)

#calculate differences
diffs.co2.25 <- paired_data_CO2.25$Downstream - paired_data_CO2.25$Upstream

#normality checks
shapiro.test(diffs.co2.25) #p <0.05 so not normal, use Wilcoxon signed rank
qqnorm(diffs.co2.25) #QQ plot to visually confirm non-normality
qqline(diffs.co2.25, col = "red")

#Wilcoxon signed rank
wilcox.test(
  paired_data_CO2.25$Upstream,
  paired_data_CO2.25$Downstream,
  paired = TRUE
) #p=0.01718

#3/4 distance across samples
paired_data_CO2.75 <- data %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  dplyr::select(Site, Date, Location, CO2.uM, CO2.uM.75) %>%
  pivot_wider(
    names_from = Location,
    values_from = c(CO2.uM, CO2.uM.75)
  ) %>%
  transmute(
    Site,
    Date,
    Upstream   = CO2.uM_Upstream,
    Downstream = CO2.uM.75_Downstream
  ) %>%
  drop_na(Upstream, Downstream)

#calculate differences
diffs.co2.75 <- paired_data_CO2.75$Downstream - paired_data_CO2.75$Upstream

#normality checks
shapiro.test(diffs.co2.75) #p = 0.1591 so technically normal and can use t-test
qqnorm(diffs.co2.75) #QQ plot to visually confirm normality
qqline(diffs.co2.75, col = "red") #doesnt look great, but choosing to accept 

#t-test & wilcoxon signed rank
t.test(
  paired_data_CO2.75$Upstream,
  paired_data_CO2.75$Downstream,
  paired = TRUE
) #p=0.01442

wilcox.test(
  paired_data_CO2.75$Upstream,
  paired_data_CO2.75$Downstream,
  paired = TRUE
) #p=0.02395

# boxplot of 1/4, 1/2, and 3/4 co2 gas
box_df_co2 <- data.frame(
  value = c(diffs.co2.25, diffs_co2, diffs.co2.75),
  group = factor(
    c(
      rep("CO2.25", length(diffs.co2.25)),
      rep("CO2.5",  length(diffs_co2)),
      rep("CO2.75", length(diffs.co2.75))
    ),
    levels = c("CO2.25", "CO2.5", "CO2.75")
  )
)

ggplot(box_df_co2, aes(x = group, y = value, fill = group)) +
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA,   # hide default outliers
    alpha = 0.7
  ) +
  geom_jitter(
    aes(color = group),
    width = 0.12,
    height = 0,
    size = 2,
    alpha = 0.6
  ) +
  scale_y_continuous(
    breaks = function(x) seq(
      floor(min(x) / 20) * 20,
      ceiling(max(x) / 20) * 20,
      by = 20
    )
  )+
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.6,
    linetype = "dotted",
    color = "black"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(
    values = c(
      "CO2.25" = "purple2",
      "CO2.5"  = "yellow2",
      "CO2.75" = "sienna2"
    )
  ) +
  scale_color_manual(
    values = c(
      "CO2.25" = "purple4",
      "CO2.5"  = "yellow4",
      "CO2.75" = "sienna4"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "CO2.25" = "First Quarter",
      "CO2.5"  = "Center",
      "CO2.75" = "Third Quarter"
    )
  ) +
  labs(
    x = "Location Across Stream Channel",
    y = expression(Dissolved~CO[2]~difference~(mu*M))
  ) +
  theme_classic() +
  theme(legend.position = "none")

#extract statistics from boxplot for each section
summary_stats_co2 <- box_df_co2 %>%
  group_by(group) %>%
  summarise(
    n        = sum(!is.na(value)),
    mean     = mean(value, na.rm = TRUE),
    median   = median(value, na.rm = TRUE),
    sd       = sd(value, na.rm = TRUE),
    IQR      = IQR(value, na.rm = TRUE),
    min      = min(value, na.rm = TRUE),
    max      = max(value, na.rm = TRUE)
  )
summary_stats_co2

#proportion of sites where [gas] is higher downstream than upstream
mean(diffs.co2.25 > 0, na.rm = TRUE) # 70% of sites had higher co2 in the impoundment 1/4 across
mean(diffs_co2    > 0, na.rm = TRUE) # 70% of sites had higher co2 in the impoundment 1/2 across
mean(diffs.co2.75 > 0, na.rm = TRUE) # 65% of sites had higher co2 in the impoundment 3/4 across


# CH4 Diffs - Spatial -----------------------------------------------------
#1/4 distance across samples
paired_data_CH4.25 <- data %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  dplyr::select(Site, Date, Location, CH4.uM, CH4.uM.25) %>%
  pivot_wider(
    names_from = Location,
    values_from = c(CH4.uM, CH4.uM.25)
  ) %>%
  transmute(
    Site,
    Date,
    Upstream   = CH4.uM_Upstream,
    Downstream = CH4.uM.25_Downstream
  ) %>%
  drop_na(Upstream, Downstream)

#calculate differences
diffs.ch4.25 <- paired_data_CH4.25$Downstream - paired_data_CH4.25$Upstream

#normality checks
shapiro.test(diffs.ch4.25) #p <0.05 so not normal, use Wilcoxon signed rank
qqnorm(diffs.ch4.25) #QQ plot to visually confirm non-normality
qqline(diffs.ch4.25, col = "red")

#Wilcoxon signed rank
wilcox.test(
  paired_data_CH4.25$Upstream,
  paired_data_CH4.25$Downstream,
  paired = TRUE
) #p=0.001824

#3/4 distance across samples
paired_data_CH4.75 <- data %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  dplyr::select(Site, Date, Location, CH4.uM, CH4.uM.75) %>%
  pivot_wider(
    names_from = Location,
    values_from = c(CH4.uM, CH4.uM.75)
  ) %>%
  transmute(
    Site,
    Date,
    Upstream   = CH4.uM_Upstream,
    Downstream = CH4.uM.75_Downstream
  ) %>%
  drop_na(Upstream, Downstream)

#calculate differences
diffs.ch4.75 <- paired_data_CH4.75$Downstream - paired_data_CH4.75$Upstream

#normality checks
shapiro.test(diffs.ch4.75) #p < 0.05 so not normal
qqnorm(diffs.ch4.75) #QQ plot to visually confirm normality
qqline(diffs.ch4.75, col = "red")

#Wilcoxon signed rank
wilcox.test(
  paired_data_CH4.75$Upstream,
  paired_data_CH4.75$Downstream,
  paired = TRUE
) #p=0.0005856

# boxplot of 1/4, 1/2, and 3/4 ch4 gas
box_df_ch4 <- data.frame(
  value = c(diffs.ch4.25, diffs.ch4, diffs.ch4.75),
  group = factor(
    c(
      rep("CH4.25", length(diffs.ch4.25)),
      rep("CH4.5",  length(diffs.ch4)),
      rep("CH4.75", length(diffs.ch4.75))
    ),
    levels = c("CH4.25", "CH4.5", "CH4.75")
  )
)

ggplot(box_df_ch4, aes(x = group, y = value, fill = group)) +
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA,   # hide default outliers
    alpha = 0.7
  ) +
  geom_jitter(
    aes(color = group),
    width = 0.12,
    height = 0,
    size = 2,
    alpha = 0.4
  ) +
  scale_y_continuous(
    breaks = function(x) seq(
      floor(min(x) / 0.25) * 0.25,
      ceiling(max(x) / 0.25) * 0.25,
      by = 0.25
    )
  )+
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.6,
    linetype = "dotted",
    color = "black"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(
    values = c(
      "CH4.25" = "purple2",
      "CH4.5"  = "yellow2",
      "CH4.75" = "sienna2"
    )
  ) +
  scale_color_manual(
    values = c(
      "CH4.25" = "purple4",
      "CH4.5"  = "yellow4",
      "CH4.75" = "sienna4"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "CH4.25" = "First Quarter",
      "CH4.5"  = "Center",
      "CH4.75" = "Third Quarter"
    )
  ) +
  labs(
    x = "Location Across Stream Channel",
    y = expression(Dissolved~CH[4]~difference~(mu*M))
  ) +
  theme_classic() +
  theme(legend.position = "none")

#extract statistics from boxplot for each section
summary_stats_ch4 <- box_df_ch4 %>%
  group_by(group) %>%
  summarise(
    n        = sum(!is.na(value)),
    mean     = mean(value, na.rm = TRUE),
    median   = median(value, na.rm = TRUE),
    sd       = sd(value, na.rm = TRUE),
    IQR      = IQR(value, na.rm = TRUE),
    min      = min(value, na.rm = TRUE),
    max      = max(value, na.rm = TRUE)
  )
summary_stats_ch4

#proportion of sites where [gas] is higher downstream than upstream
mean(diffs.ch4.25 > 0, na.rm = TRUE) # 75% of sites had higher ch4 in the impoundment 1/4 across
mean(diffs.ch4    > 0, na.rm = TRUE) # 70% of sites had higher ch4 in the impoundment 1/2 across
mean(diffs.ch4.75 > 0, na.rm = TRUE) # 80% of sites had higher ch4 in the impoundment 3/4 across





# N2O Diffs - Spatial -----------------------------------------------------
#1/4 distance across samples
paired_data_n2o.25 <- data %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  dplyr::select(Site, Date, Location, N2O.nM, N2O.nM.25) %>%
  pivot_wider(
    names_from = Location,
    values_from = c(N2O.nM, N2O.nM.25)
  ) %>%
  transmute(
    Site,
    Date,
    Upstream   = N2O.nM_Upstream,
    Downstream = N2O.nM.25_Downstream
  ) %>%
  drop_na(Upstream, Downstream)

#calculate differences
diffs.n2o.25 <- paired_data_n2o.25$Downstream - paired_data_n2o.25$Upstream

#normality checks
shapiro.test(diffs.n2o.25) #p <0.05 so not normal, use Wilcoxon signed rank
qqnorm(diffs.n2o.25) #QQ plot to visually confirm non-normality
qqline(diffs.n2o.25, col = "red")

#Wilcoxon signed rank
wilcox.test(
  paired_data_n2o.25$Upstream,
  paired_data_n2o.25$Downstream,
  paired = TRUE
) #p=0.01362

#3/4 distance across samples
paired_data_n2o.75 <- data %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  dplyr::select(Site, Date, Location, N2O.nM, N2O.nM.75) %>%
  pivot_wider(
    names_from = Location,
    values_from = c(N2O.nM, N2O.nM.75)
  ) %>%
  transmute(
    Site,
    Date,
    Upstream   = N2O.nM_Upstream,
    Downstream = N2O.nM.75_Downstream
  ) %>%
  drop_na(Upstream, Downstream)

#calculate differences
diffs.n2o.75 <- paired_data_n2o.75$Downstream - paired_data_n2o.75$Upstream

#normality checks
shapiro.test(diffs.n2o.75) #p < 0.05 so not normal
qqnorm(diffs.n2o.75) #QQ plot to visually confirm normality
qqline(diffs.n2o.75, col = "red")

#Wilcoxon signed rank
wilcox.test(
  paired_data_n2o.75$Upstream,
  paired_data_n2o.75$Downstream,
  paired = TRUE
) #p=0.02395

# boxplot of 1/4, 1/2, and 3/4 n2o gas
box_df_n2o <- data.frame(
  value = c(diffs.n2o.25, diffs_n2o, diffs.n2o.75),
  group = factor(
    c(
      rep("N2O.25", length(diffs.n2o.25)),
      rep("N2O.5",  length(diffs_n2o)),
      rep("N2O.75", length(diffs.n2o.75))
    ),
    levels = c("N2O.25", "N2O.5", "N2O.75")
  )
)

ggplot(box_df_n2o, aes(x = group, y = value, fill = group)) +
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA,   # hide default outliers
    alpha = 0.7
  ) +
  geom_jitter(
    aes(color = group),
    width = 0.12,
    height = 0,
    size = 2,
    alpha = 0.4
  ) +
  scale_y_continuous(
    breaks = function(x) seq(
      floor(min(x) / 10) * 10,
      ceiling(max(x) / 10) * 10,
      by = 10
    )
  )+
  stat_summary(
    fun = mean,
    geom = "crossbar",
    width = 0.6,
    linetype = "dotted",
    color = "black"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(
    values = c(
      "N2O.25" = "purple2",
      "N2O.5"  = "yellow2",
      "N2O.75" = "sienna2"
    )
  ) +
  scale_color_manual(
    values = c(
      "N2O.25" = "purple4",
      "N2O.5"  = "yellow4",
      "N2O.75" = "sienna4"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "N2O.25" = "First Quarter",
      "N2O.5"  = "Center",
      "N2O.75" = "Third Quarter"
    )
  ) +
  labs(
    x = "Location Across Stream Channel",
    y = expression(Dissolved~N[2]*O~difference~(nM))
  ) +
  theme_classic() +
  theme(legend.position = "none")
#

#extract statistics from boxplot for each section
summary_stats_n2o <- box_df_n2o %>%
  group_by(group) %>%
  summarise(
    n        = sum(!is.na(value)),
    mean     = mean(value, na.rm = TRUE),
    median   = median(value, na.rm = TRUE),
    sd       = sd(value, na.rm = TRUE),
    IQR      = IQR(value, na.rm = TRUE),
    min      = min(value, na.rm = TRUE),
    max      = max(value, na.rm = TRUE)
  )
summary_stats_n2o

#proportion of sites where [gas] is higher downstream than upstream
mean(diffs.n2o.25 > 0, na.rm = TRUE) # 70% of sites had higher n2o in the impoundment 1/4 across
mean(diffs_n2o    > 0, na.rm = TRUE) # 75% of sites had higher n2o in the impoundment 1/2 across
mean(diffs.n2o.75 > 0, na.rm = TRUE) # 75% of sites had higher n2o in the impoundment 3/4 across




# CO2 Time Series -------------------------------------

data_pb <- data %>%
  filter(Site == "Pheasant Branch")

#co2 1/4 across
ts.co2.pb.25 <- data_pb %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  mutate(
    Date=as.Date(Date, format="%d-%b-%y"),
    CO2_plot = case_when(
      Location == "Upstream"   ~ CO2.uM,
      Location == "Downstream" ~ CO2.uM.25
    )
  ) %>%
  dplyr::select(Date, Location, CO2_plot) %>%
  arrange(Date)

#co2 1/2 across
ts.co2.pb.5 <- data_pb %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  mutate(
    Date=as.Date(Date, format="%d-%b-%y"),
    CO2_plot = case_when(
      Location == "Upstream"   ~ CO2.uM,
      Location == "Downstream" ~ CO2.uM
    )
  ) %>%
  dplyr::select(Date, Location, CO2_plot) %>%
  arrange(Date)

#co2 3/4 across
ts.co2.pb.75 <- data_pb %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  mutate(
    Date=as.Date(Date, format="%d-%b-%y"),
    CO2_plot = case_when(
      Location == "Upstream"   ~ CO2.uM,
      Location == "Downstream" ~ CO2.uM.75
    )
  ) %>%
  dplyr::select(Date, Location, CO2_plot) %>%
  arrange(Date)

ggplot(ts.co2.pb.5, aes(x = Date, y = CO2_plot, color = Location)) +
  geom_line(aes(group=Location)) +
  geom_point(size = 1) +
  scale_color_manual(
    values = c(
      "Upstream" = "turquoise3",
      "Downstream" = "olivedrab3"
    )
  ) +
  labs(
    x = "Date",
    y = expression(Dissolved~CO[2]~(mu*M)),
    color = "Location"
  ) +
  theme_classic()

ts_all_co2_pb <- bind_rows(
  ts.co2.pb.25 %>% mutate(Position = "First Quarter"),
  ts.co2.pb.5  %>% mutate(Position = "Center"),
  ts.co2.pb.75 %>% mutate(Position = "Third Quarter")
) %>%
  mutate(
  Position = factor(
    Position,
    levels = c("First Quarter", "Center", "Third Quarter")
  )
)

ggplot(ts_all_co2_pb, aes(x = Date, y = CO2_plot, color = Location)) +
  geom_line(aes(group = Location), linewidth=1) +
  geom_point(size = 2) +
  geom_smooth(aes(group = Location), method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.8) +  # can remove trendline
  scale_color_manual(
    values = c(
      "Upstream" = "turquoise3",
      "Downstream" = "olivedrab3"
    )
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  facet_wrap(~Position, nrow = 1) +
  labs(
    x = "Date",
    y = expression(Dissolved~CO[2]~(mu*M)),
    color = "Location"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",                                      # nicer legend placement
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold")
  )

#extract coefficients if using lm
lm_trends <- ts_all_co2_pb %>%
  group_by(Location) %>%
  do(
    model = lm(CO2_plot ~ Date, data = .)
  )
lm_trends <- lm_trends %>%
  rowwise() %>%
  mutate(
    intercept = coef(model)[1],
    slope = coef(model)[2]
  ) %>%
  dplyr::select(Location, intercept, slope)
lm_trends




# CH4 Time Series ---------------------------------------------------------

#ch4 1/4 across
ts.ch4.pb.25 <- data_pb %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  mutate(
    Date=as.Date(Date, format="%d-%b-%y"),
    CH4_plot = case_when(
      Location == "Upstream"   ~ CH4.uM,
      Location == "Downstream" ~ CH4.uM.25
    )
  ) %>%
  dplyr::select(Date, Location, CH4_plot) %>%
  arrange(Date)

#ch4 1/2 across
ts.ch4.pb.5 <- data_pb %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  mutate(
    Date=as.Date(Date, format="%d-%b-%y"),
    CH4_plot = case_when(
      Location == "Upstream"   ~ CH4.uM,
      Location == "Downstream" ~ CH4.uM
    )
  ) %>%
  dplyr::select(Date, Location, CH4_plot) %>%
  arrange(Date)

#ch4 3/4 across
ts.ch4.pb.75 <- data_pb %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  mutate(
    Date=as.Date(Date, format="%d-%b-%y"),
    CH4_plot = case_when(
      Location == "Upstream"   ~ CH4.uM,
      Location == "Downstream" ~ CH4.uM.75
    )
  ) %>%
  dplyr::select(Date, Location, CH4_plot) %>%
  arrange(Date)

#time series of ch4 at center
ggplot(ts.ch4.pb.5, aes(x = Date, y = CH4_plot, color = Location)) +
  geom_line(aes(group=Location)) +
  geom_point(size = 1) +
  scale_color_manual(
    values = c(
      "Upstream" = "turquoise3",
      "Downstream" = "olivedrab3"
    )
  ) +
  labs(
    x = "Date",
    y = expression(Dissolved~CH[4]~(mu*M)),
    color = "Location"
  ) +
  theme_classic()

#bind to position
ts_all_ch4_pb <- bind_rows(
  ts.ch4.pb.25 %>% mutate(Position = "First Quarter"),
  ts.ch4.pb.5  %>% mutate(Position = "Center"),
  ts.ch4.pb.75 %>% mutate(Position = "Third Quarter")
) %>%
  mutate(
    Position = factor(
      Position,
      levels = c("First Quarter", "Center", "Third Quarter")
    )
  )

#plot all spatial time series onto one plot and add lm line
ggplot(ts_all_ch4_pb, aes(x = Date, y = CH4_plot, color = Location)) +
  geom_line(aes(group = Location), linewidth = 1) +                # main lines
  geom_point(size = 2) +                                          # points
  geom_smooth(aes(group = Location), method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.8) +  # can remove trendline
  scale_color_manual(
    values = c(
      "Upstream" = "turquoise3",
      "Downstream" = "olivedrab3"
    )
  ) +
  scale_x_date(
    date_breaks = "1 month",                                  
    date_labels = "%b"                                      
  ) +
  facet_wrap(~Position, nrow = 1) + #facets for 1/4, 1/2, 3/4 channel
  labs(
    x = "Date",
    y = expression(Dissolved~CH[4]~(mu*M)),
    color = "Location"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",                                      # nicer legend placement
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold")
  )

#extract coefficients if using lm
lm_trends <- ts_all_ch4_pb %>%
  group_by(Location) %>%
  do(
    model = lm(CH4_plot ~ Date, data = .)
  )
lm_trends <- lm_trends %>%
  rowwise() %>%
  mutate(
    intercept = coef(model)[1],
    slope = coef(model)[2]
  ) %>%
  dplyr::select(Location, intercept, slope)
lm_trends


# N2O Time Series ---------------------------------------------------------
#n2o 1/4 across
ts.n2o.pb.25 <- data_pb %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  mutate(
    Date=as.Date(Date, format="%d-%b-%y"),
    N2O_plot = case_when(
      Location == "Upstream"   ~ N2O.nM,
      Location == "Downstream" ~ N2O.nM.25
    )
  ) %>%
  dplyr::select(Date, Location, N2O_plot) %>%
  arrange(Date)

#n2o 1/2 across
ts.n2o.pb.5 <- data_pb %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  mutate(
    Date=as.Date(Date, format="%d-%b-%y"),
    N2O_plot = case_when(
      Location == "Upstream"   ~ N2O.nM,
      Location == "Downstream" ~ N2O.nM
    )
  ) %>%
  dplyr::select(Date, Location, N2O_plot) %>%
  arrange(Date)

#n2o 3/4 across
ts.n2o.pb.75 <- data_pb %>%
  filter(Location %in% c("Upstream", "Downstream")) %>%
  mutate(
    Date=as.Date(Date, format="%d-%b-%y"),
    N2O_plot = case_when(
      Location == "Upstream"   ~ N2O.nM,
      Location == "Downstream" ~ N2O.nM.75
    )
  ) %>%
  dplyr::select(Date, Location, N2O_plot) %>%
  arrange(Date)

#time series of n2o at center
ggplot(ts.n2o.pb.5, aes(x = Date, y = N2O_plot, color = Location)) +
  geom_line(aes(group=Location)) +
  geom_point(size = 1) +
  scale_color_manual(
    values = c(
      "Upstream" = "turquoise3",
      "Downstream" = "olivedrab3"
    )
  ) +
  labs(
    x = "Date",
    y = expression(Dissolved~N[2]*O~(nM)),
    color = "Location"
  ) +
  theme_classic()

#bind to position
ts_all_n2o_pb <- bind_rows(
  ts.n2o.pb.25 %>% mutate(Position = "First Quarter"),
  ts.n2o.pb.5  %>% mutate(Position = "Center"),
  ts.n2o.pb.75 %>% mutate(Position = "Third Quarter")
) %>%
  mutate(
    Position = factor(
      Position,
      levels = c("First Quarter", "Center", "Third Quarter")
    )
  )

#plot all spatial time series onto one plot and add lm line
ggplot(ts_all_n2o_pb, aes(x = Date, y = N2O_plot, color = Location)) +
  geom_line(aes(group = Location), linewidth = 1) +               
  geom_point(size = 2) +                                      
  geom_smooth(aes(group = Location), method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.8) +  # can remove trendline
  scale_color_manual(
    values = c(
      "Upstream" = "turquoise3",
      "Downstream" = "olivedrab3"
    )
  ) +
  scale_x_date(
    date_breaks = "1 month",                                  
    date_labels = "%b"                                      
  ) +
  facet_wrap(~Position, nrow = 1) + #facets for 1/4, 1/2, 3/4 channel
  labs(
    x = "Date",
    y = expression(Dissolved~N[2]*O~(nM)),
    color = "Location"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",                                      # nicer legend placement
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold")
  )

#extract coefficients if using lm
lm_trends <- ts_all_n2o_pb %>%
  group_by(Location) %>%
  do(
    model = lm(N2O_plot ~ Date, data = .)
  )
lm_trends <- lm_trends %>%
  rowwise() %>%
  mutate(
    intercept = coef(model)[1],
    slope = coef(model)[2]
  ) %>%
  dplyr::select(Location, intercept, slope)
lm_trends

# Facet Wrapping QQ Plots -------------------------------------------------
#co2
qq_df_co2 <- data.frame(
  value = c(diffs.co2.25, diffs_co2, diffs.co2.75),
  Position = factor(
    c(
      rep("First Quarter", length(diffs.co2.25)),
      rep("Center", length(diffs_co2)),
      rep("Third Quarter", length(diffs.co2.75))
    ),
    levels = c("First Quarter", "Center", "Third Quarter")
  )
)
ggplot(qq_df_co2, aes(sample = value)) +
  stat_qq(size = 2) +
  stat_qq_line(color = "red", linewidth = 1) +
  facet_wrap(~ Position, nrow = 1) +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
  ) +
  theme_bw()

#ch4
qq_df_ch4 <- data.frame(
  value = c(diffs.ch4.25, diffs.ch4, diffs.ch4.75),
  Position = factor(
    c(
      rep("First Quarter", length(diffs.ch4.25)),
      rep("Center", length(diffs.ch4)),
      rep("Third Quarter", length(diffs.ch4.75))
    ),
    levels = c("First Quarter", "Center", "Third Quarter")
  )
)
ggplot(qq_df_ch4, aes(sample = value)) +
  stat_qq(size = 2) +
  stat_qq_line(color = "red", linewidth = 1) +
  facet_wrap(~ Position, nrow = 1) +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
  ) +
  theme_bw()

#n2o
qq_df_n2o <- data.frame(
  value = c(diffs.n2o.25, diffs_n2o, diffs.n2o.75),
  Position = factor(
    c(
      rep("First Quarter", length(diffs.n2o.25)),
      rep("Center", length(diffs_n2o)),
      rep("Third Quarter", length(diffs.n2o.75))
    ),
    levels = c("First Quarter", "Center", "Third Quarter")
  )
)
ggplot(qq_df_n2o, aes(sample = value)) +
  stat_qq(size = 2) +
  stat_qq_line(color = "red", linewidth = 1) +
  facet_wrap(~ Position, nrow = 1) +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles",
  ) +
  theme_bw()
