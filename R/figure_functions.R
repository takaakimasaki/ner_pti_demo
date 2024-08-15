map_indicator_adm2 <- function(sf_adm2, sf_adm1, indicator) {
map <- tm_shape(sf_adm2) +
    tm_fill(
      indicator,
      title = "Value",
      style = "quantile",
      palette = "Reds",
      legend.reverse = TRUE
    )  +
    tm_borders(col = "grey", lwd = 0.5) +
    tm_layout(
      legend.outside = FALSE,
      legend.position = c("left", "top"),
      legend.title.size
      = 1.2,
      legend.text.size = 1.2
    ) +
    tm_shape(sf_adm1) +
    tm_borders(col = "black", lwd = 2) +
    tm_text("admin1Name",
            size = 1.5,
            col = "black",
            alpha = 0.8) +
    tmap_options(check.and.fix = TRUE)

tmap_save(tm = map,
          filename = paste0("figures","/",indicator,".png"))
}

map_indicator_adm1 <- function(sf_adm1, indicator) {
  map <- tm_shape(sf_adm1) +
    tm_fill(
      indicator,
      title = "Value",
      style = "quantile",
      palette = "Reds",
      legend.reverse = TRUE
    )  +
    tm_borders(col = "grey", lwd = 0.5) +
    tm_layout(
      legend.outside = FALSE,
      legend.position = c("left", "top"),
      legend.title.size
      = 1.2,
      legend.text.size = 1.2
    ) +
    tmap_options(check.and.fix = TRUE)
  
  tmap_save(tm = map,
            filename = paste0("figures","/",indicator,".png"))
}

map_indicator_adm2_pti <- function(sf_adm2, sf_adm1, indicator) {
  map <- tm_shape(sf_adm2) +
    tm_fill(
      indicator,
      title = "Value",
      style = "quantile",
      n=5,
      legend.reverse = TRUE,
      palette=brewer.pal(5,"BuPu"),
      labels = c("Priority 5",
                 "Priority 4",
                 "Priority 3",
                 "Priority 2",
                 "Priority 1"))  +
    tm_borders(col = "grey", lwd = 0.5) +
    tm_layout(
      legend.outside = FALSE,
      legend.position = c("left", "top"),
      legend.title.size
      = 1.2,
      legend.text.size = 1.2
    ) +
    tm_shape(sf_adm1) +
    tm_borders(col = "black", lwd = 2) +
    tm_text("admin1Name",
            size = 1.5,
            col = "black",
            alpha = 0.8) +
    tmap_options(check.and.fix = TRUE)
  
  tmap_save(tm = map,
            filename = paste0("figures","/",indicator,".png"))
  
}

