nato <- pull_live_googledoc(ws = "NATO_membership",
                            key = '1YlMVTe6IjgxEheRjUzITq7zSMlI3DRUSrcpGK35gKUg')

sheet_nato <- googlesheets::gs_read(nato, ws = "nato_membership")
sheet_pfp <- googlesheets::gs_read(nato, ws = "pfp_membership")

nato <- structure(sheet_nato, class = "data.frame")
nato <- as.data.frame(nato)
pfp <- structure(sheet_pfp, class = "data.frame")
pfp <- as.data.frame(pfp)

head(nato)
head(pfp)

nato <- nato[which(nato$nato==1),]
pfp <- pfp[which(pfp$nato==1),]

nato_states <- sort(unique(nato$state))
pfp_states <- sort(unique(pfp$state))
cur_year <- 2019

ghantt_df <- list()
index <- 1
#values: 0 - nothing, 1 - pfp, 2 - nato
for(country in nato_states){
  if(country %in% pfp_states){
    pfp_temp <- pfp[which(pfp$state==country),]
    nato_temp <- nato[which(nato$state==country),]

    pfp_start <- pfp_temp$join_pfp_year
    pfp_end <- pfp_temp$left_pfp_year

    r <- c(1949:(pfp_start-1))
    for(yr in r){
      ghantt_df[[index]] <- data.frame(country = country, year = yr, value = 0)
      index <- index + 1
    }

    r <- c(pfp_start:(pfp_end-1))
    for(yr in r){
      ghantt_df[[index]] <- data.frame(country = country, year = yr, value = 1)
      index <- index + 1
    }

    start_yr <- nato_temp$join_nato_year
    r <- c(start_yr:cur_year)
    for(yr in r){
      ghantt_df[[index]] <- data.frame(country = country, year = yr, value = 2)
      index <- index + 1
    }
  }
  else{
    nato_temp <- nato[which(nato$state==country),]
    start_yr <- nato_temp$join_nato_year

    if(start_yr != 1949){
      r <- c(1949:(start_yr-1))
      for(yr in r){
        ghantt_df[[index]] <- data.frame(country = country, year = yr, value = 0)
        index <- index + 1
      }
    }

    r <- c(start_yr:cur_year)
    for(yr in r){
      ghantt_df[[index]] <- data.frame(country = country, year = yr, value = 2)
      index <- index + 1
    }
  }
}

g_df <- ghantt_df[[1]]
for(i in 2:length(ghantt_df)){
  g_df <- rbind(g_df,ghantt_df[[i]])
}

ghantt_df <- g_df
head(ghantt_df)

ghantt_df$value <- as.factor(ghantt_df$value)

#vectors for x-axis breaks and labels
breaks <- seq(from = 1945, to = 2021)
labels <- c()
desired_labels <- seq(from = 1945, to = 2020, by = 5)
for(i in breaks){
  if(i %in% desired_labels){
    labels <- c(labels,i)
  }
  else{
    labels <- c(labels,'')
  }
}

ghantt_chart <- ggplot(ghantt_df, aes(ghantt_df$year, y=factor(country,levels=rev(levels(factor(country)))))) +
  geom_tile(aes(fill = ghantt_df$value),height=0.5) +
  scale_fill_manual(values = c("white", "gold", "darkblue"),
                    labels=c("None", "PFP", "NATO")) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 18),
        title = element_text(size = 20),
        axis.line.y = element_line(),
        panel.background = element_rect(fill="white", color="white")) +
  #scale_x_continuous(breaks = seq(), minor_breaks = 1895:1914, expand = c(.05, .05)) +
  expand_limits(x = 1945:2021) +
  scale_x_discrete(breaks = breaks,
                   labels = labels,
                   limits = c(1945:2024)) +
  labs(y="Country",title = "Timeline for Stages of NATO Membership",
       x="Year", fill="Stage")
ghantt_chart
