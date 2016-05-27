## make country prediction graph
## Nicholas Reich
## September 2014

#'@param forecast_file file containing forecasts
#'@param counts_file file containing counts
#'

make_country_prediction_line_graph <- function(forecasts, counts, ylim_scale=1, min_plot_date=as.Date("2012-04-01"), show_unused_counts=TRUE) {
    require(dplyr)
    require(lubridate)
    require(scales)
    forecasts <- tbl_df(forecasts)
    counts <- tbl_df(counts)
    
    ## aggregate to country-level
    forecasts_cntry <- forecasts %>% group_by(biweek, year) %>% 
        summarize(predicted_cntry_count = sum(predicted_count),
                  predicted_ub = sum(ub),
                  predicted_lb = sum(lb)) %>%
        mutate(time = year + (biweek-1)/26,
               date_sick = biweek_to_date(biweek, year))
    forecast_times <- (forecasts_cntry$year + (forecasts_cntry$biweek-1)/26)
    
    counts_cntry <- counts %>% 
        group_by(date_sick_year, date_sick_biweek) %>%
        summarize(cntry_count = sum(count)) %>%
        mutate(time = date_sick_year + (date_sick_biweek-1)/26,
               date_sick = biweek_to_date(date_sick_biweek, date_sick_year),
               forecast_biweek = time %in% forecast_times) %>%
        filter(date_sick >= min_plot_date)
    
    ## add column in counts_cntry indicating which biweeks were left out of the fit
    
    ## make plot
    p <- ggplot() + theme_bw() + 
        theme(legend.position="bottom",
              axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5),
              panel.background = element_rect(fill = "transparent",colour = NA), # or element_blank()
              panel.grid.major =  element_blank(),
              panel.grid.minor =  element_blank(),
              plot.background = element_rect(fill = "transparent",colour = NA)) 
    
    if(show_unused_counts){
        ## using gray bars for unused cases
        p <- p + geom_bar(data=counts_cntry, 
                          aes(x=date_sick, y=cntry_count, fill=forecast_biweek), 
                          stat="identity") + 
            scale_fill_manual(values=c("black", "gray"),
                              name="",
                              labels=c("used by forecast model", "not used by forecast model"))
    } else {
        ## no unused cases
        p <- p + geom_bar(data=filter(counts_cntry, forecast_biweek==FALSE), 
                          aes(x=date_sick, y=cntry_count), 
                          stat="identity")
    }
    p <- p +  
        ## add forecasts
        geom_line(data=forecasts_cntry, 
                  aes(x=date_sick, y=predicted_cntry_count)) +
        geom_point(data=forecasts_cntry, 
                   aes(x=date_sick, y=predicted_cntry_count)) +
        geom_ribbon(data=forecasts_cntry, 
                    aes(x=date_sick, ymin=predicted_lb, ymax=predicted_ub), 
                    alpha=I(.3)) +
        # air-brushing
        scale_x_date(breaks = "3 months",
                     labels = date_format("%d %b %Y"))+
        xlab(NULL) + ylab(NULL) + 
        ylim(0, max(counts_cntry$cntry_count)*ylim_scale) +
        ggtitle("Observed and predicted DHF case counts for all of Thailand")
    p
    
}

new_make_country_prediction_line_graph <- function(forecasts,
                                                   counts,
                                                   ylim_scale=1,
                                                   min_plot_date=as.Date("2012-04-01"),
                                                   show_unused_counts=TRUE,
                                                   expanded_counts) {
    require(dplyr)
    require(lubridate)
    require(scales)
    forecasts <- tbl_df(forecasts)
    counts <- tbl_df(counts)
    
    ## aggregate to country-level
    forecasts_cntry <- forecasts %>%
        group_by(date_sick_biweek, date_sick_year) %>% 
        summarize(predicted_cntry_count = sum(predicted_count),
                  predicted_ub = sum(ub),
                  predicted_lb = sum(lb)) %>%
        mutate(time = date_sick_year + (date_sick_biweek-1)/26,
               date_sick = biweek_to_date(date_sick_biweek, date_sick_year)) %>%
        ungroup()
    forecast_times <- (forecasts_cntry$date_sick_year + (forecasts_cntry$date_sick_biweek-1)/26)
    
    counts_cntry <- counts %>% 
        group_by(date_sick_year, date_sick_biweek) %>%
        summarize(cntry_count = sum(count)) %>%
        mutate(time = date_sick_year + (date_sick_biweek-1)/26,
               date_sick = biweek_to_date(date_sick_biweek, date_sick_year),
               forecast_biweek = time %in% forecast_times,
               expanded = 0) %>%
        filter(date_sick >= min_plot_date) %>%
        ungroup()
    
    ## establish a sane upper bound
    counts_ub <- counts %>%
        group_by(date_sick_year, date_sick_biweek) %>%
        summarise(count = sum(count))
    max_counts <- max(counts_ub$count * ylim_scale)
    chart_ub <- ifelse(max(forecasts_cntry$predicted_ub) * ylim_scale > max_counts,
                       max(max_counts,
                           forecasts_cntry$predicted_count * ylim_scale),
                       max(c(forecasts_cntry$predicted_ub,
                             counts_cntry$cntry_count)) * ylim_scale)
    
    if(!missing(expanded_counts)){
        expanded_ctry <- expanded_counts %>% 
            group_by(date_sick_year, date_sick_biweek) %>%
            summarize(cntry_count = sum(expanded_cases)) %>%
            mutate(time = date_sick_year + (date_sick_biweek-1)/26,
                   date_sick = biweek_to_date(date_sick_biweek, date_sick_year),
                   forecast_biweek = time %in% forecast_times,
                   expanded=1) %>%
            filter(date_sick >= min_plot_date) %>%
            select(date_sick_year, date_sick_biweek, cntry_count,
                   time, date_sick, forecast_biweek, expanded)
        counts_cntry <- bind_rows(counts_cntry, expanded_ctry)
        
    }
    
    ## add column in counts_cntry indicating which biweeks were left out of the fit
    
    ## make plot
    p <- ggplot() + theme_bw() + 
        theme(legend.position="bottom",
              axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5),
              panel.background = element_rect(fill = "transparent",
                                              colour = NA), # or element_blank()
              panel.grid.major =  element_blank(),
              panel.grid.minor =  element_blank(),
              plot.background = element_rect(fill = "transparent",
                                             colour = NA)) 
    
    if(show_unused_counts){
        if(!missing(expanded_counts)){
            counts_cntry$bar_color <- counts_cntry$expanded+
                2*counts_cntry$forecast_biweek
            
            ## using gray bars for unused cases and blue bars for expanded cases
            p <- p + geom_bar(data=counts_cntry, 
                              aes(x=date_sick,
                                  y=cntry_count,
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
            p <- p + geom_bar(data=counts_cntry, 
                              aes(x=date_sick,
                                  y=cntry_count,
                                  fill=forecast_biweek),
                              color="black",
                              stat="identity") + 
                scale_fill_manual(values=c("black", "white"),
                                  name="",
                                  labels=c("used by forecast model",
                                           "not used by forecast model"))
        }
    } else {
        if(!missing(expanded_counts)){
            ## using blue bars for expanded cases
            p <- p + geom_bar(data=counts_cntry, 
                              aes(x=date_sick,
                                  y=cntry_count,
                                  fill=as.factor(expanded)), 
                              color="black",
                              stat="identity") + 
                scale_fill_manual(values=c("black", "gray"),
                                  name="",
                                  labels=c("observed cases",
                                           "estimated unreported cases"))
        } else{
            ## no unused cases
            p <- p + geom_bar(data=filter(counts_cntry, forecast_biweek==FALSE), 
                              aes(x=date_sick, y=cntry_count), 
                              stat="identity")
        }
    }
    p <- p +  
        ## add forecasts
        geom_line(data=forecasts_cntry, 
                  aes(x=date_sick, y=predicted_cntry_count)) +
        geom_point(data=forecasts_cntry, 
                   aes(x=date_sick, y=predicted_cntry_count)) +
        geom_ribbon(data=forecasts_cntry, 
                    aes(x=date_sick, ymin=predicted_lb, ymax=predicted_ub), 
                    alpha=I(.3)) +
        # air-brushing
        scale_x_date(date_breaks = "3 months",
                     labels = date_format("%d %b %Y"))+
        xlab(NULL) + ylab(NULL) + 
        coord_cartesian(ylim=c(0, chart_ub)) +
        ggtitle("Observed and predicted DHF case counts for all of Thailand")
    p
    
}