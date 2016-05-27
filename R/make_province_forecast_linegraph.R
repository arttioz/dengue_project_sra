## make province prediction graph
## Nicholas Reich
## September 2014

#'@param forecasts object with the forecasts
get_forecast_prov_data <- function(forecasts) {
    require(dplyr)
    require(lubridate)
    data(thai_prov_data)
    forecasts_prov <- forecasts %>% 
        mutate(time = year + (biweek-1)/26,
               date_sick = biweek_to_date(biweek, year),
               FIPS = pid)
    forecasts_prov <- left_join(forecasts_prov, thai_prov_data)
    return(forecasts_prov)
}

#'@param forecasts object with the forecasts
new_get_forecast_prov_data <- function(forecasts) {
    require(dplyr)
    require(lubridate)
    data(thai_prov_data)
    forecasts_prov <- forecasts %>% 
        mutate(time = date_sick_year + (date_sick_biweek-1)/26,
               date_sick = biweek_to_date(date_sick_biweek, date_sick_year),
               FIPS = pid)
    forecasts_prov <- left_join(forecasts_prov, thai_prov_data)
    return(forecasts_prov)
}

#'@param forecasts_prov object with the forecasts by province, output from get_forecast_prov_data
#'@param counts object with count data
#'@param min_plot_date the earliest date desired in the plots
get_count_prov_data <- function(counts, forecasts_prov, min_plot_date=as.Date('2012-04-01')) {
    require(dplyr)
    require(lubridate)
    
    data(thai_prov_data)
    
    ## get forecast times to categorize counts
    forecast_times <- (forecasts_prov$year + (forecasts_prov$biweek-1)/26)
    
    ## set counts
    counts_prov <- counts %>% 
        group_by(date_sick_year, date_sick_biweek, pid) %>%
        summarize(prov_count = sum(count)) %>%
        mutate(time = date_sick_year + (date_sick_biweek-1)/26,
               date_sick = biweek_to_date(date_sick_biweek, date_sick_year),
               forecast_biweek = time %in% forecast_times) %>%
        filter(date_sick >= min_plot_date) %>%
        ungroup()
    
    ## ready the province data
    thai_prov_data <- mutate(thai_prov_data, 
                             province=ISO,
                             pid=FIPS,
                             pname=reorder(Province, Population, FUN=mean))
    counts_prov <- left_join(counts_prov, thai_prov_data)
    
    return(counts_prov)
}

#'@param forecasts_prov object with the forecasts by province, output from get_forecast_prov_data
#'@param counts object with count data
#'@param min_plot_date the earliest date desired in the plots
new_get_count_prov_data <- function(counts,
                                    forecasts_prov,
                                    expanded_counts) {
    require(dplyr)
    require(lubridate)
    
    data(thai_prov_data)
    
    ## get forecast times to categorize counts
    forecast_times <- (forecasts_prov$date_sick_year + (forecasts_prov$date_sick_biweek-1)/26)
    
    ## set counts
    counts_prov <- counts %>% 
        group_by(date_sick_year, date_sick_biweek, pid) %>%
        summarize(prov_count = sum(count)) %>%
        mutate(time = date_sick_year + (date_sick_biweek-1)/26,
               date_sick = biweek_to_date(date_sick_biweek, date_sick_year),
               forecast_biweek = time %in% forecast_times,
               expanded = 0) %>%
        # filter(date_sick >= min_plot_date) %>%
        ungroup()
    
    ## add in expanded counts, if necessary
    if(!missing(expanded_counts)){
        expanded_prov <- expanded_counts %>%
            mutate(pid = thai_prov_data$FIPS[match(Province,
                                                   thai_prov_data$Province)]) %>%
            group_by(date_sick_year, date_sick_biweek, pid) %>%
            summarize(prov_count = sum(expanded_cases)) %>%
            mutate(time = date_sick_year + (date_sick_biweek-1)/26,
                   date_sick = biweek_to_date(date_sick_biweek, date_sick_year),
                   forecast_biweek = time %in% forecast_times,
                   expanded=1) %>%
            filter(date_sick >= min_plot_date) %>%
            select(date_sick_year, date_sick_biweek, pid, prov_count,
                   time, date_sick, forecast_biweek, expanded)
        counts_prov <- bind_rows(counts_prov, expanded_prov)
    }
    
    ## ready the province data
    thai_prov_data <- mutate(thai_prov_data, 
                             pid=FIPS,
                             pname=reorder(Province, Population, FUN=mean))
    counts_prov <- left_join(counts_prov, thai_prov_data)
    
    return(counts_prov)
}

#'@param forecasts_prov object containing processed forecasts
#'@param counts_prov object containing aggregated counts
#'@param region MOPH region to plot
#'@param show_unused_cases if true, shows the counts unused in the forecasts

make_province_prediction_line_graph <- function(forecasts_prov, 
                                                counts_prov, 
                                                region=1,
                                                show_unused_counts=TRUE,
                                                min_plot_date,
                                                ub_multiplier=1.1) {
    require(dplyr)
    require(lubridate)
    require(scales)
    
    ## subset based on region
    forecasts_prov <- filter(forecasts_prov, MOPH_Admin_Code == region)
    counts_prov <- filter(counts_prov, MOPH_Admin_Code == region)
    counts_ub <- counts_prov %>%
        group_by(date_sick_year, date_sick_biweek, date_sick, pid) %>%
        summarise(count = sum(prov_count))
    max_counts <- max(counts_ub$count * ub_multiplier)
    counts_prov <- filter(counts_prov, date_sick >= min_plot_date)
    counts_ub <- filter(counts_ub, date_sick >= min_plot_date)
    chart_ub <- ifelse(max(forecasts_prov$ub) * ub_multiplier > max_counts,
                       max(max_counts,
                           forecasts_prov$predicted_count * ub_multiplier),
                       max(c(forecasts_prov$ub,
                             counts_ub$count)) * ub_multiplier)
    
    ## make plot
    plot_title <- ifelse(region==0,
                         paste("Observed and predicted DHF case counts for Bangkok"),
                         paste("Observed and predicted DHF case counts for MOPH Region", region))
    
    ## T/F variable of whether there are expanded counts
    expanded_counts <- sum(counts_prov$expanded)>0
    
    ## start graphing
    p <- ggplot() + theme_bw() + 
        theme(legend.position="bottom", #legend.justification=c(1,1),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5),
              panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
              panel.grid.major =  element_blank(),
              panel.grid.minor =  element_blank(),
              plot.background = element_rect(fill = "transparent",colour = NA))
    if(show_unused_counts){
        if(expanded_counts){
            counts_prov$bar_color <- counts_prov$expanded+
                2*counts_prov$forecast_biweek
            
            ## using gray bars for unused cases and blue bars for expanded cases
            p <- p + geom_bar(data=counts_prov, 
                              aes(x=date_sick,
                                  y=prov_count,
                                  fill=as.factor(bar_color)),
                              color="black", 
                              stat="identity") + 
                scale_fill_manual(values=c("black", "gray", "white"),
                                  name="",
                                  labels=c("observed cases",
                                           "estimated unreported cases",
                                           "observed cases, unused"))
        } else{
            ## using gray bars for unused cases
            p <- p +
                geom_bar(data=counts_prov, 
                         aes(x=date_sick, y=prov_count, fill=forecast_biweek),
                         color="black",
                         stat="identity") + 
                scale_fill_manual(values=c("black", "white"),
                                  name="",
                                  labels=c("used by forecast model",
                                           "not used by forecast model"))
        }
    } else {
        ## no unused cases
        if(expanded_counts){
            ## using blue bars for expanded cases
            p <- p + geom_bar(data=counts_prov, 
                              aes(x=date_sick,
                                  y=prov_count,
                                  fill=as.factor(expanded)),
                              color="black",
                              stat="identity") + 
                scale_fill_manual(values=c("black", "gray"),
                                  name="",
                                  labels=c("observed cases",
                                           "estimated unreported cases"))
        } 
        p <- p +
            geom_bar(data=filter(counts_prov, forecast_biweek==FALSE), 
                     aes(x=date_sick, y=prov_count), 
                     stat="identity")
    }
    p <- p +
        ## add forecasts
        geom_line(data=forecasts_prov, aes(x=date_sick, y=predicted_count)) +
        geom_point(data=forecasts_prov, aes(x=date_sick, y=predicted_count)) +
        geom_ribbon(data=forecasts_prov, aes(x=date_sick,  ymin=lb, ymax=ub),
                    alpha=I(.3)) +
        facet_grid(pname~.) +
        # air-brushing
        scale_x_date(date_breaks = "3 months",
                     labels = date_format("%d %b %Y")) +
        coord_cartesian(ylim=c(0, chart_ub)) +
        xlab(NULL) + ylab(NULL) + #ylim(0, 1000) +
        ggtitle(plot_title)
    p
    
}

