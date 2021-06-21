## Start ----
## Script name: Price Dispersion Analysis
##
## Purpose of the script: Explore pricing data of Xbox360 products
##                        as set by certain stores from 2012 - 2020
##                        Home Assignment;
##                        Data Analysis and Visualization course;
##                        Dalarna University
##
## Authors: Saumya Gupta, M.M. Usman Zahid
##
## Date Created: 2021-02-28
##
## Copyright (c) 2021 Saumya Gupta
##
## Email: h20saugu@du.se, v21miaza@du.se


## before proceeding, we recommend Windows Rstudio user to use Ctrl+Shift+O
## to see document outline


## set working directory.
# setwd(
#   'C:/Users/gupta/OneDrive/Documents/MS-DS/AMI23A/Part3/HomeAssignment/PriceDispersionAnalysis/'
# )


## load up required packages
library(ggplot2)
library(ggdendro)
library(ggpubr)
library(gridExtra)
library(data.table)
library(dplyr)
library(tidyverse)
library(dtw)
library(NbClust)


## create DTW Suite suggested method to help create dissimilarity matrix out of
## time series of different lengths
dtwOmitNA <- function (x, y)
{
  a <- na.omit(x)
  b <- na.omit(y)
  return(dtw(a, b, distance.only = TRUE)$normalizedDistance)
}

## create new entry in registry with two aliases
pr_DB$set_entry(FUN = dtwOmitNA, names = c("dtwOmitNA"))


## create method to cluster stores and produce cluster visualizations
findDTWHCluster <- function(id, product_ts, k_val = 3) {
  ## convert to data frame for ease in operations
  product_ts <- as.data.frame(product_ts)
  
  ## get store IDs as row names
  row.names(product_ts) <- product_ts$store_id
  product_ts$store_id <- NULL
  
  ## get distance matrix using DTW distance measure for unequal time series
  d <- dist(product_ts, method = "dtwOmitNA")
  
  ## cluster hierarchically using 'complete' agglomeration method
  hc <- stats::hclust(d, method = "complete")
  
  ## cut tree using asked number of groups
  hclus <- cutree(hc, k = k_val) %>%
    as.data.frame(.) %>%
    rename(., cluster_group = .) %>%
    rownames_to_column("store_id")
  
  ## show cluster hierarchy using dendrogram
  hcdata <- dendro_data(hc)
  
  # cols <- c("#a9a9a9", "#1f77b4")
  
  p1 <- hcdata %>%
    ggdendrogram(., rotate = TRUE, leaf_labels = FALSE) +
    geom_hline(yintercept = 1, color = "red") +
    annotate(
      "text",
      x = 15.5,
      y = 0.85,
      label = "Cluster 2",
      size = 2
    ) +
    annotate(
      "text",
      x = 8,
      y = 0.85,
      label = "Cluster 1",
      size = 2
    ) +
    theme(text = element_text(size = 8),
          plot.margin = margin(0, 0,-0.3, 0, "cm"))
  
  p1 <- annotate_figure(p1,
                        bottom = text_grob("Height", size = 8))
  
  ## get store IDs as row names in clustered stores data
  row.names(hclus) <- hclus$store_id
  hclus$store_id <- NULL
  
  ## combine non clustered time series with assigned cluster groups
  stores_clustered_wide <-
    merge(product_ts, hclus, by = 0) %>% rename("store_id" = "Row.names")
  
  ## reshape data to facilitate plotting
  stores_clustered_long <-
    stores_clustered_wide %>% pivot_longer(
      starts_with("201"),
      names_to = "date",
      values_to = "normalized_adjusted_price",
      values_drop_na = TRUE
    ) %>%
    mutate(date = as.Date(date)) %>%
    as.data.frame()
  
  ## get real prices for plotting
  product_real_price <- data_Xbox360 %>%
    filter(product_id == id) %>%
    select(store_id, date, cpi_adjusted_price)
  
  ## plot un-clustered stores first, for comparison
  ggplot(product_real_price,
         aes(date, cpi_adjusted_price, color = store_id)) +
    geom_step(size = 1) +
    ggtitle(paste("Product #", product, "Non clustered Stores")) +
    xlab("Time") +
    ylab("CPI-Adjusted Price (in SEK)") +
    theme_minimal()
  
  ## merge clustered data with real prices
  stores_clustered_long <-
    merge(stores_clustered_long,
          product_real_price,
          by = c("store_id", "date")) %>%
    mutate(Cluster = as.factor(cluster_group),
           date = as.Date(date))
  
  ## get cluster statistics on cluster groups
  clustered_stores <- stores_clustered_long %>%
    group_by(Cluster, date) %>%
    summarise(
      mean_price = mean(cpi_adjusted_price),
      q.25 = quantile(cpi_adjusted_price, 0.25),
      q.75 = quantile(cpi_adjusted_price, 0.75),
      min_price = min(cpi_adjusted_price),
      max_price = max(cpi_adjusted_price)
    )
  
  ## produce cluster result
  cluster_fig <- ggplot(clustered_stores,
                        aes(date,
                            mean_price,
                            group = Cluster,
                            colour = Cluster)) +
    geom_step(size = 1) +
    geom_ribbon(aes(
      ymin = q.25,
      ymax = q.75,
      fill = Cluster
    ),
    alpha = 0.25,
    color = 'transparent') +
    ggtitle(paste("Product #", id)) +
    xlab("Time") +
    ylab("CPI-Adjusted Price (in SEK)") +
    theme_minimal() +
    theme(text = element_text(size = 8), legend.position = "none")
  
  grid.arrange(cluster_fig, p1, ncol = 2)
  
  ## un-comment below to just get time series w/o dendrogram
  # print(cluster_fig)
}


## create cluster validation function using NbClust to
## validate clusters and returns optimal partitioning information
bestNbClust <- function(id, product_ts) {
  product_data_NbClust <- as.data.frame(product_ts)
  # Get store IDs as row names
  row.names(product_data_NbClust) <- product_data_NbClust$store_id
  product_data_NbClust$store_id <- NULL
  # Get distance matrix using dtw distance measure for unequal time series
  d_NbClust <- dist(product_data_NbClust, method = "dtwOmitNA")
  res <-
    NbClust(
      diss = d_NbClust,
      distance = NULL,
      min.nc = 2,
      max.nc = 6,
      method = "ward.D2",
      index = "mcclain"
    )
  res$Best.nc
  #  return (res)
}


## Data Read ----

## load up data containing all product categories
data_total <- fread("../home_assignment_data_pricing.csv")

## filter data needed for exploration
data_Xbox360 <- data_total %>% filter(category == 'Xbox 360')

## encode variables to factor variables to help plot better figures
data_Xbox360$product_id = as.factor(data_Xbox360$product_id)
data_Xbox360$store_id = as.factor(data_Xbox360$store_id)


## Result 1 ----

### Fig. 1 ----
## show data distribution over time
ggplot(data_Xbox360,
       aes(date)) +
  geom_histogram(bins = 28,
                 color = 'white') +
  xlab('Time') +
  ylab('Number of data points') +
  ggtitle('Data Distribution') +
  theme_minimal()

## get number of stores and products over time
data_Xbox360[, c('date', 'product_id', 'store_id')] %>%
  group_by(date) %>%
  mutate(products = n_distinct(product_id),
         stores = n_distinct(store_id)) %>%
  select(date, products, stores) %>%
  distinct() %>%
  pivot_longer(
    cols = c("products", "stores"),
    names_to = "entity",
    values_to = "count"
  ) -> everyday_product_store_count

### Fig. 2 (Report Fig. 1)----
## show number of products and stores over time
ggplot(everyday_product_store_count, aes(date, count, color = entity)) +
  geom_line(size = 1) +
  scale_color_manual(
    "",
    labels = c("Products", "Retailers"),
    values = c("blue", "darkorange")
  ) +
  xlab('Time') +
  ylab('Count') +
  ggtitle('Products and Retailers Over Time (2012 - 2017)') +
  theme_minimal() +
  theme(text = element_text(size = 8))

### Fig. 3 ----
## show all prices over time (execution takes time)
# ggplot(data_Xbox360[,
#                     c("date", "store_id", "product_id", "cpi_adjusted_price")],
#        aes(date, cpi_adjusted_price, color = store_id)) +
#   geom_point(size = 0.1, alpha = 0.25) +
#   ylim(0, 1500) +
#   xlab('Time') +
#   ylab('CPI-Adjusted Price') +
#   ggtitle('Prices Over Time (2012 - 2017)') +
#   theme_minimal()
# theme(legend.position = "none")

## get all price aggregates over time
data_Xbox360 %>%
  select(cpi_adjusted_price, date) %>%
  group_by(date) %>%
  summarise(
    # mean = mean(cpi_adjusted_price),
    # median = median(cpi_adjusted_price),
    # standard_deviation = sd(cpi_adjusted_price),
    CoV = (sd(cpi_adjusted_price) / mean(cpi_adjusted_price)) * 1000,
    Min  = min(cpi_adjusted_price),
    Max = max(cpi_adjusted_price)
  ) %>%
  pivot_longer(cols = starts_with(c("M", "CoV", "sta")),
               names_to = "aggregate",
               values_to = "value") -> everyday_price_aggregates

### Fig. 4 ----
## show all price aggregates over time
ggplot(everyday_price_aggregates,
       aes(date, value, color = aggregate)) +
  geom_step() +
  scale_y_continuous(
    "CPI-Adjusted Price (in SEK)",
    sec.axis = sec_axis(trans =  ~ . * 0.1,
                        name = "(*0.1) Zoomed Axis for CoV (%)")
  ) +
  # scale_color_brewer("",
  #                    palette = "Dark2") +
  xlab("Time") +
  ggtitle('Prices (all products)') +
  theme_minimal()

### Fig. 5 ----
## show case of product #1260
data_Xbox360 %>%
  filter(product_id == 2687705,
         store_id == 1260,
         date >= '2015-01-01',
         date <= '2015-12-31') %>%
  ggplot(aes(date, cpi_adjusted_price)) +
  geom_step() +
  xlab("2015") +
  ylab("CPI-Adjusted Price (in SEK)") +
  ggtitle('Price (Product #2687705 - Store #1260) (2015)') +
  theme_minimal()

## get price aggregates excluding weird data points
data_Xbox360 %>%
  select(cpi_adjusted_price, date) %>%
  filter(cpi_adjusted_price < 4000) %>%
  group_by(date) %>%
  summarise(
    # mean = mean(cpi_adjusted_price),
    # median = median(cpi_adjusted_price),
    Min  = min(cpi_adjusted_price),
    Max = max(cpi_adjusted_price),
    # standard_deviation = sd(cpi_adjusted_price),
    CoV = (sd(cpi_adjusted_price) / mean(cpi_adjusted_price)) * 1000
  ) %>%
  pivot_longer(cols = starts_with(c("M", "CoV", "sta")),
               names_to = "aggregate",
               values_to = "value") -> everyday_price_aggregates_wo_outlier

## show all price aggregates over time excluding weird data points
plot1 <- ggplot(everyday_price_aggregates_wo_outlier,
                aes(date, value, color = aggregate)) +
  geom_step() +
  scale_y_continuous("",
                     sec.axis = sec_axis(trans =  ~ . * 0.1)) +
  # scale_color_brewer("",
  #                    palette = "Dark2") +
  xlab("Time") +
  ggtitle('A. Prices (all products)') +
  theme_minimal() +
  theme(text = element_text(size = 8),
        legend.title = element_blank())

## check for single product
data_Xbox360 %>%
  filter(product_id == 3186029) %>%
  select(cpi_adjusted_price, date) %>%
  group_by(date) %>%
  summarise(
    # mean = mean(cpi_adjusted_price),
    # median = median(cpi_adjusted_price),
    Min  = min(cpi_adjusted_price),
    Max = max(cpi_adjusted_price),
    # standard_deviation = sd(cpi_adjusted_price),
    CoV = (sd(cpi_adjusted_price) / mean(cpi_adjusted_price)) * 1000
  ) %>%
  pivot_longer(cols = starts_with(c("M", "CoV", "sta")),
               names_to = "aggregate",
               values_to = "value") -> everyday_price_aggregates_3186029

## show result for single product
plot2 <- ggplot(everyday_price_aggregates_3186029,
                aes(date, value, color = aggregate)) +
  geom_step() +
  scale_y_continuous("",
                     sec.axis = sec_axis(trans =  ~ . * 0.1)) +
  # scale_color_brewer("",
  #                    palette = "Dark2") +
  xlab("Time") +
  ggtitle('B. Prices (product # 3186029)') +
  theme_minimal() +
  theme(
    text = element_text(size = 8),
    plot.margin = margin(0, 0,-0.2, 0, "cm"),
    legend.title = element_blank()
  )

## combine plot 1 & 2
multi_fig <- ggarrange(
  plot1,
  plot2,
  nrow = 2,
  common.legend = TRUE,
  legend = "bottom"
)

### Fig. 6 (Report Fig. 2)----
## annotate some axis text
annotate_figure(
  multi_fig,
  left = text_grob("CPI-Adjusted Price (in SEK)", size = 8, rot = 90),
  right = text_grob(
    "(*0.1) Zoomed Axis for CoV (%)",
    size = 8,
    rot = -90
  )
)

## find summary stats on mean coefficient of variation for all products
product_with_mean_CoV <- data_Xbox360 %>%
  group_by(product_id, date) %>%
  summarise(CoV =
              sd(cpi_adjusted_price) / mean(cpi_adjusted_price)) %>%
  group_by(product_id) %>%
  summarise(mean_CoV = mean(CoV))

summary(product_with_mean_CoV %>% select(mean_CoV))

product_stats <-
  merge(product_with_mean_CoV, product_with_store_count, by = "product_id") %>%
  arrange(desc(mean_CoV))

## get number of stores for each product
data_Xbox360 %>%
  group_by(product_id) %>%
  summarise(store_count = n_distinct(store_id)) %>%
  arrange(desc(store_count)) -> product_with_store_count

## get number of products for each store
data_Xbox360 %>%
  group_by(store_id) %>%
  summarise(product_count = n_distinct(product_id)) %>%
  arrange(desc(product_count)) -> store_with_product_count

## get store ts for all products in file using loop
# for (product in levels(data_Xbox360$product_id)) {
#   my_plot <- ggplot(
#     data_Xbox360 %>% filter(product_id == product),
#     aes(date, cpi_adjusted_price, color = store_id)
#   ) +
#     geom_line() +
#     ggtitle(paste("Product #", product)) +
#     xlab("Time") +
#     ylab("CPI-Adjusted Price (in SEK)")
#   ggsave(paste("product_", product, ".png"), my_plot)
# }

## show price randomizing/undercutting practiced by stores for product # 1769910
plot3 <- ggplot(
  data_Xbox360 %>% filter(product_id == 1769910),
  aes(date, cpi_adjusted_price, color = store_id)
) +
  geom_line() +
  ggtitle(paste("A. Product #", 1769910)) +
  xlab("") +
  ylab("") +
  theme_minimal() +
  theme(text = element_text(size = 8)) +
  theme(legend.position = "none",
        plot.margin = unit(c(0.1, 0.2,-0.1,-0.2), "cm"))

## show price randomizing/undercutting practiced by stores for product # 1769910
plot4 <- ggplot(
  data_Xbox360 %>% filter(product_id == 1341635),
  aes(date, cpi_adjusted_price, color = store_id)
) +
  geom_line() +
  ggtitle(paste("B. Product #", 1341635)) +
  xlab("Time") +
  ylab("") +
  theme_minimal() +
  theme(text = element_text(size = 8)) +
  theme(legend.position = "none",
        plot.margin = unit(c(-0.1, 0.2, 0,-0.2), "cm"))

### Fig. 7 (Report Fig. 3) ----
## combine plot 3 & 4
grid.arrange(
  plot3,
  plot4,
  nrow = 2,
  left = text_grob("CPI-Adjusted Price (in SEK)", size = 8, rot = 90)
)


## Result 2 ----

### Scaling Prices ----
## normalizing cpi_adjusted_prices
data_Xbox360 <- data_Xbox360 %>%
  mutate(normalized_cpi_adjusted_price = scale(cpi_adjusted_price))

### Clustering ----
## get store-wise time series for all products in list using loop
product_ts_list <- list()

for (product in product_with_store_count$product_id[1:30]) {
  product_ts_list[[product]] <- data_Xbox360 %>%
    filter(product_id == product) %>%
    select(store_id, normalized_cpi_adjusted_price, date) %>%
    arrange(date) %>%
    pivot_wider(id_cols = store_id,
                names_from = date,
                values_from = normalized_cpi_adjusted_price) %>%
    as.data.table()
}

#### Fig. 8, 9 (Report Fig. 4) ----
## cluster product # 1341635 (change function a bit to get exact figure)
findDTWHCluster(1341635,
                product_ts_list$`1341635`,
                2)

#### Fig. 10, 11 (Report Fig. 5) ----
## cluster product # 916993
findDTWHCluster(916993,
                product_ts_list$`916993`,
                2)

### Cluster Validation ----
## validate clusters for product # 1341635
bestNbClust(1341635, product_ts_list$`1341635`)

## validate clusters for product # 916993
bestNbClust(916993, product_ts_list$`916993`)


## End ----
