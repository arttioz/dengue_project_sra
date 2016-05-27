## plot forecasts on a map
## Nicholas Reich
## September 2014

#'@param forecast_data forecast file dataframe
#'@param cdata cntry.data object with spatial polygons needed for plotting
#'@param biweek_to_plot biweek to plot
#'@param include_legend logical, whether to include legend
#'@param plot_type one of either "incidence" or "outbreak"

plot_forecast_map <- function(forecast_data, cdata, biweek_to_plot, 
                              include_legend=TRUE,
                              plot_type=c("incidence", "outbreak")) {
        require(ggplot2)
        require(dplyr)
        require(rgeos)
        require(mapproj)
        
        data(thai_prov_data)

        ## adjust for end of year biweeks
        if(biweek_to_plot>26)
                biweek_to_plot <- biweek_to_plot - 26
        
        if(!(biweek_to_plot %in% unique(forecasts$biweek)))
                stop("biweek must be in forecast_data.")
        
        ## merge thai_prov_data with forecasts to get population
        forecast_data$FIPS <- forecast_data$pid
        forecast_data_merged <- left_join(forecast_data, thai_prov_data)
        forecast_data_merged <- mutate(forecast_data_merged,
                                       incidence = predicted_count/Population)
                
        ## foritfy polygon info
        thai_locs <- fortify(cdata@loc.info, region="ID_1")
        thai_locs[['region']] <- thai_locs[['id']]
        
        ## store loc.info@data
        loc_info <- cdata@loc.info@data
        
        ## combine polygon info with thai data
        thai_prov_data <- mutate(thai_prov_data, FIPS_ADMIN = FIPS) ## create common column name
        loc_info <- select(loc_info, -ISO) ## create common column name
        data_to_plot <- left_join(loc_info, thai_prov_data) %>%
                mutate(id = ID_1,
                       pid = as.character(FIPS))
        
      
        ## plotting choices based on type
        if(plot_type=="incidence") {
                fill_var <- "log10(incidence)"
                plot_lims <- range(forecast_data_merged$incidence)
                plot_lims <- c(floor(log10(plot_lims)[1]),
                               ceiling(log10(plot_lims)[2]))
                plot_breaks <- seq(plot_lims[1], plot_lims[2])
                plot_midpoint <- mean(plot_lims)
                legend_title <- "incidence"
                plot_labels <- paste0("1e", plot_breaks)
                
        } else {
                fill_var <- "outbreak_prob"
                plot_lims <- c(0,1)
                plot_midpoint <- .5
                plot_breaks <- seq(0, 1, by=.2)
                plot_labels <- plot_breaks
                legend_title <- "outbreak probability"
        }
        
        ## set legend position, if any
        legend_pos <- ifelse(include_legend, "right", "none")
                
        ## text for map label
        forecast_data_subset <- subset(forecast_data_merged, biweek == biweek_to_plot)
        map_date <- format(as.Date(biweek_to_date(biweek_to_plot, forecast_data_subset$year[1])), "%d %b %Y")
        
        ## merge forecast data with data_to_plot
        data_to_plot <- left_join(data_to_plot, forecast_data_subset)
        
        sp_map <- ggplot(data_to_plot, aes(map_id=id)) + 
                geom_map(aes_string(fill=fill_var), map=thai_locs) + 
                expand_limits(x = thai_locs$long, y = thai_locs$lat) +
                ## use color blind friendly colors
                scale_fill_gradient2(low = "#053061", mid="#CCCCCC", high = "#FF2C19", 
                                     name=legend_title,
                                     limits=plot_lims, 
                                     midpoint=plot_midpoint, 
                                     breaks=plot_breaks,
                                     labels=plot_labels) +
                theme_bw() +
                theme(axis.ticks = element_blank(), 
                      axis.text = element_blank(),
                      panel.background = element_rect(fill = "transparent",colour = NA),
                      legend.position = legend_pos) + # or element_blank()
                coord_map(projection="mercator") + ## to keep scaling right
                annotate("text", x = 103.5, y = 20.1, label = map_date, size=4) +
                xlab("") + ylab("")        
        print(sp_map)
        
}


new_plot_forecast_map <- function(forecast_data,
                                  cdata,
                                  biweek_to_plot, 
                                  include_legend=TRUE,
                                  plot_type=c("incidence", "outbreak")) {
  require(ggplot2)
  require(dplyr)
  require(rgeos)
  require(mapproj)
    require(maptools)
  
  data(thai_prov_data)
  
  ## adjust for end of year biweeks
  if(biweek_to_plot>26)
    biweek_to_plot <- biweek_to_plot - 26
  
  if(!(biweek_to_plot %in% unique(forecasts$date_sick_biweek)))
    stop("date_sick_biweek must be in forecast_data.")
  
  ## merge thai_prov_data with forecasts to get population
  forecast_data_merged <- left_join(forecast_data, thai_prov_data, by = c("pid" = "FIPS")) %>%
    mutate(incidence = predicted_count/Population)
  
  ## foritfy polygon info
  cdata@loc.info <- cdata@loc.info[which(cdata@loc.info@data$gns_adm1!="TH81"),]
  cdata@loc.info$ID_1 <- dense_rank(cdata@loc.info$woe_name)
  thai_locs <- fortify(cdata@loc.info, region="ID_1")
  thai_locs[['region']] <- thai_locs[['id']]
  
  ## store loc.info@data
  loc_info <- cdata@loc.info@data
  
  ## combine polygon info with thai data
  data_to_plot <- left_join(loc_info, thai_prov_data, by = c("gns_adm1" = "FIPS")) %>%
    mutate(id = ID_1,
           pid = as.character(gns_adm1))
  
  
  ## plotting choices based on type
  if(plot_type=="incidence") {
    fill_var <- "log10(incidence)"
    plot_lims <- range(forecast_data_merged$incidence)
    plot_lims <- c(floor(log10(plot_lims)[1]),
                   ceiling(log10(plot_lims)[2]))
    plot_breaks <- seq(plot_lims[1], plot_lims[2])
    plot_midpoint <- mean(plot_lims)
    legend_title <- "incidence"
    plot_labels <- paste0("1e", plot_breaks)
    
  } else {
    fill_var <- "outbreak_prob"
    plot_lims <- c(0,100)
    plot_midpoint <- 50
    plot_breaks <- seq(0, 100, by=20)
    plot_labels <- plot_breaks
    legend_title <- "outbreak probability (%)"
  }
  
  ## set legend position, if any
  legend_pos <- ifelse(include_legend, "right", "none")
  
  ## text for map label
  forecast_data_subset <- subset(forecast_data_merged, date_sick_biweek == biweek_to_plot)
  map_date <- format(as.Date(biweek_to_date(biweek_to_plot, forecast_data_subset$date_sick_year[1])), "%d %b %Y")
  
  ## merge forecast data with data_to_plot
  new_dp <- left_join(data_to_plot, forecast_data_subset)
  
  sp_map <- ggplot(new_dp, aes(map_id=id)) + 
    geom_map(aes_string(fill=fill_var), map=thai_locs) + 
    expand_limits(x = thai_locs$long, y = thai_locs$lat) +
    ## use color blind friendly colors
    scale_fill_gradient2(low = "#053061", mid="#CCCCCC", high = "#FF2C19", 
                         name=legend_title,
                         limits=plot_lims, 
                         midpoint=plot_midpoint, 
                         breaks=plot_breaks,
                         labels=plot_labels) +
    theme_bw() +
    theme(axis.ticks = element_blank(), 
          axis.text = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.position = legend_pos) + # or element_blank()
    coord_map(projection="mercator") + ## to keep scaling right
    annotate("text", x = 103.5, y = 20.1, label = map_date, size=4) +
    xlab("") + ylab("")        
  print(sp_map)
  
}