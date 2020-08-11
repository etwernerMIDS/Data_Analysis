alliance_isaf <- c("Albania",
                    "Belgium",
                    "Bulgaria",
                    "Canada",
                    "Denmark",
                    "France",
                    "Germany",
                    "Hungary",
                    "Italy",
                    "Netherlands",
                    "Norway",
                    "Poland",
                    "Romania",
                    "Spain",
                    "Turkey",
                    "Ukraine",
                    "United Kingdom",
                    "United States of America")

#########################################################

# Alliance data from COW Alliances V 4.1
ally <- read.csv(paste0(here::here(),"/inst/extdata/alliance_v4.1_by_dyad_yearly.csv"), header = TRUE)

##########################################################

polnet <- pull_live_googledoc(ws = "polnet",
                              key = '104s0-psNBbWCZsEw0bqTBog2AUFzNfrp1mkCUGoT3rE')

subsheet_names <- googlesheets::gs_ws_ls(polnet)
sheets_out <- list()
for(i in 1:length(subsheet_names)){
  sheets_out[[i]] <- googlesheets::gs_read(polnet, ws = i)
}

polnet_final <- sheets_out

n <- length(polnet_final[[1]])
polnet_final <- structure(polnet_final, row.names = c(NA, -n), class = "data.frame")

polnet_final <- data.table::rbindlist(polnet_final, fill = TRUE)
polnet_final <- as.data.frame(polnet_final)


##########################################################

ally <- ally[ally$state_name1 %in% alliance_isaf &
               ally$state_name2 %in% alliance_isaf, ]
ally$alliance_strength <- ally$defense + ally$neutrality + ally$nonaggression + ally$entente

ally$isaf <- ifelse(ally$state_name1 %in% polnet_final$country,     polnet_final$isaf_ratio,
                    NA)

vars_ally <- c("ccode1", "ccode2", "year", "alliance_strength","isaf")
ally <- ally[vars_ally]

# for the individual
ally <- ally[ which(ally$year == '2001'), ]
ally$ccode1_name <- countrycode::countrycode(ally$ccode1, 'cown', 'cowc')
ally$ccode2_name <- countrycode::countrycode(ally$ccode2, 'cown', 'cowc')
#ally

abbr <- c(unique(ally$ccode1_name),unique(ally$ccode2_name))
abbr <- unique(abbr)

##########################################################

# CINC scores from COW National Military Capability (NMC) V 5.0
cinc <- read.csv(paste0(here::here(),"/inst/extdata/NMC_5_0.csv"), header = TRUE)

cinc <- cinc[cinc$stateabb %in% abbr, ]
vars_cinc <- c("ccode", "year", "cinc")
cinc <- cinc[vars_cinc]

#for the individual
cinc <- cinc[ which(cinc$year == '2001'), ]
cinc$ccode_name <- countrycode::countrycode(cinc$ccode, 'cown', 'cowc')
#cinc

##########################################################

col_ally <- c("ccode1_name", "ccode2_name")
pair <- ally[col_ally]

values <- c("gray82","darkgray","gray22")
ally$str_color <- values[ally$alliance_strength]

my_igraph <- igraph::graph_from_data_frame(d = pair, vertices = cinc$ccode_name, directed = F)
my_igraph <- igraph::simplify(my_igraph, remove.multiple = T)

deg <- igraph::degree(my_igraph)
igraph::V(my_igraph)$size <- exp(20*cinc$cinc)+8
igraph::V(my_igraph)$shape <- "sphere"
igraph::V(my_igraph)$color <- "cadetblue1"
igraph::E(my_igraph)$width <- (ally$alliance_strength * 0.6) #warning

# Generate edge color variable to plot the path:
ecol <- rep(ally$str_color, igraph::ecount(my_igraph))

# Generate edge width variable to plot the path:
ew <- rep(2, igraph::ecount(my_igraph))

# Generate node color variable to plot the path:
vcol <- rep("cadetblue1", igraph::vcount(my_igraph))


layout <- igraph::layout_with_kk(my_igraph)
layout <- igraph::norm_coords(layout, ymin = -1, ymax = 1, xmin = -1, xmax = 1)

plot(my_igraph, main = "Alliances between Afghanistan war coalition members", sub = "2001",
     layout = layout * 1.2, rescale = FALSE, vertex.label = cinc$ccode_name,
     vertex.color = vcol,
     vertex.label.font = 2, vertex.label.dist = 1.5, edge.color=ecol,
     vertex.label.degree = pi/2, vertex.label.cex = 0.8)


#ratio_spiderplot_scaled_fullportfolio

alliance_iiss <- c("albania",
                   "belgium",
                   "bulgaria",
                   "canada",
                   "denmark",
                   "france",
                   "germany",
                   "hungary",
                   "italy",
                   "netherlands",
                   "norway",
                   "poland",
                   "romania",
                   "spain",
                   "turkey",
                   "ukraine",
                   "united kingdom",
                   "us")

iiss <- readRDS(file = paste0(here::here(), '/data/IISS.rds'))
iiss <- iiss[which(iiss$year==2014),]

complete <- iiss
iiss <- iiss[which(iiss$country %in% alliance_iiss),]

equipment <- c("armoured fighting vehicles","artillery","air defence",
  "engineering & maintenance vehicles","patrol and coastal combatants","mine warfare",
  "logistics and support","helicopters","principal surface combatants","aircraft",
  "unmanned aerial vehicles","missiles","satellites","anti-tank/anti-infrastructure",
  "radars","amphibious","submarines","bombers","strategic missile defence")

percentages <- list()
index <- 1

for(my_equip in equipment){
  ratio <- iiss
  total <- complete

  ratio <- ratio %>% filter(ratio$equipment_type==my_equip)
  ratio$unit_count <- as.integer(ratio$unit_count)

  total <- total %>% filter(total$equipment_type==my_equip)
  total$unit_count <- as.integer(total$unit_count)

  df <- setkey(setDT(ratio),equipment_type)[,list(equip_amount=sum(unit_count, na.rm = T)),
                                            by=list(equipment_type)]

  t <- setkey(setDT(total),equipment_type)[,list(equip_amount=sum(unit_count, na.rm = T)),
                                            by=list(equipment_type)]

  final_ratio <- df$equip_amount/t$equip_amount

  percentages[[index]] <- data.frame(equipment = my_equip, percent = 100*final_ratio)
  index <- index + 1
}

my_df <- percentages[[1]]
for(i in 2:length(percentages)){
  my_df <- rbind(my_df,percentages[[i]])
}
my_df <- equip_domain(my_df)
my_df <- my_df[order(my_df$equip_domain),]
Domain <- my_df$equip_domain

spider <- ggplot2::ggplot(data = my_df, aes(x = my_df$equipment,fill = Domain, y = my_df$percent)) +
  geom_bar(stat = "identity", width = 1, color="black") +
  coord_polar(theta = "x") +
  labs(title=paste0("Distribution of Military Capabilities"),x="", y="Percent of Global Share") + # title and caption
  theme_linedraw() +
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "gray70"),
        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0))) +
  scale_x_discrete(limits=my_df$equipment)
spider

gt <- ggplot_gtable(ggplot_build(spider))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

rs <- arrangeGrob(gt)

#########################

overall <- list()
index <- 1
dfList <- list()
count <- 1

for(my_country in alliance_iiss){
  dfList <- list()
  count <- 1
  for(my_equip in equipment){
    ratioList <- equipment_ratio(complete,2014,my_equip)
    ratioVector <- unlist(ratioList)
    ratio <- list()
    rcount <- 1
    country <- list()
    ccount <- 1

    for(i in 1:length(ratioVector)){
      if((i %% 2) == 0){
        country[[ccount]] <- ratioVector[i]
        ccount <- ccount +1
      }
      else{
        if(ratioVector[i] > 0.0000001){
          ratioVector[i] <- 0
        }
        ratio[[rcount]] <- ratioVector[i]
        rcount <- rcount +1
      }
    }
    ratio

    country <- unlist(country)
    ratio <- unlist(ratio)
    my_data <- cbind(country,ratio)
    my_data <- data.frame(my_data)


    my_data$equipment <- ifelse(!is.na(my_data$country),     my_equip,
                                NA)


    ratio <- as.numeric(ratio)
    my_data$ratio_100 <- ifelse(ratio <= 1,     ratio*100,
                                NA)


    if(!(my_country %in% my_data$country)){
      my_data$country <- my_country
      my_data$ratio <- 0
      my_data$equipment <- my_equip
      my_data$ratio_100 <- 0.0000000
      my_data <- my_data[1,]
    }
    else{
      my_data <- my_data %>% filter(my_data$country == my_country)
    }
    dfList[[count]] <- my_data
    count <- count + 1
  }

  my_df <- dfList[[1]]
  my_df
  for(i in 2:length(dfList)){
    my_df <- rbind(my_df,dfList[[i]])
  }
  my_df

  overall[[index]] <- my_df
  index <- index + 1

}

final <- overall[[1]]
for(i in 2:length(overall)){
  final <- rbind(final,overall[[i]])
}

#final <- unique(final)
#final <- my_df

tol18rainbow=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")


g <- ggplot(data = final, aes(x = final$equipment,y = final$ratio_100,fill = final$country)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_light() +
  theme(axis.text.x = element_text(size = 8,angle = 90),
        axis.text.y = element_text(size = 12)) +
  guides(fill=guide_legend(title="Country")) +
  scale_fill_manual(values=tol18rainbow) +
  labs(title="Overall Military Arsenal",
       subtitle="2014",
       x = "Military Equipment", y = "Global Share")
g



#c('darkred', 'red', 'orange', 'yellow', 'lightgreen', 'green4', '#46f0f0', 'navy', 'plum', 'purple3',
#'lightpink', 'hotpink3', 'indianred3', 'gray', 'royalblue2', 'mediumpurple2', 'mediumorchid', 'cadetblue3')


