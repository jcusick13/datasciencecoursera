# pm_emissions.R
library(dplyr)
library(ggplot2)

# Download dataset
temp <- tempfile()
f <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip" 
download.file(f, temp)
unlink(temp)

setwd("/Users/JonathanCusick/Documents/local/datasciencecoursera/exploratory_data_analysis")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
SCC$SCC <- as.character(SCC$SCC)



# 1. Have total emissions from PM2.5 decreased in the U.S.?
total_pm25 <-
    NEI %>%
    group_by(year) %>%
    summarize(emissions = sum(Emissions, na.rm = TRUE))

plot(x = total_pm25$year, y = total_pm25$emissions, xlab = "Year",
     ylab = "Emissions (tons)", 
     main = "Total PM2.5 Emissions in the United States (1999 - 2008)") 
abline(lm(total_pm25$emissions ~ total_pm25$year))


# 2. Have total emissions from PM2.5 decreased in Baltimore City?
bmore_total_pm25 <-
    NEI %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarize(emissions = sum(Emissions, na.rm = TRUE))

plot(x = bmore_total_pm25$year, y = bmore_total_pm25$emissions,
     xlab = "Year", ylab = "Emissions (tons)",
     main = "Total PM2.5 Emissions in Baltimore City, MD (1999 - 2008)")
abline(lm(bmore_total_pm25$emissions ~ bmore_total_pm25$year))


# 3. Emission trends for Baltimore City by type?
bmore_types <-
    NEI %>%
    filter(fips == "24510") %>%
    group_by(year, type) %>%
    summarize(emissions = sum(Emissions, na.rm = TRUE))

ggplot(bmore_types, aes(x = year, y = emissions)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap( ~ type) +
    labs(x = "Year", y = "Emissions (tons)",
         title = "PM2.5 Emissions by Source - Baltimore City, MD") 


# 4. Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008?
coal <-
    SCC %>%
    filter(str_detect(EI.Sector, "^Fuel Comb.*Coal")) %>%
    inner_join(NEI, by = "SCC") %>%
    group_by(year) %>%
    summarize(emissions = sum(Emissions, na.rm = TRUE))

ggplot(coal, aes(x = year, y = emissions)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(x = "Year", y = "Emissions (tons)",
         title = "National PM2.5 Emissions",
         subtitle = "Coal combustion-related sources")


# 5. How have emissions from motor vehicle sources changed 
# from 1999–2008 in Baltimore City?
bmore_mv <-
    SCC %>%
    filter(str_detect(EI.Sector, "^Mobile.*Vehicles")) %>%
    inner_join(NEI, by = "SCC") %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    summarize(bmore.emissions = sum(Emissions, na.rm = TRUE))

ggplot(bmore_mv, aes(x = year, y = bmore.emissions)) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(x = "Year", y = "Emissions (tons)",
         title = "Baltimore City, MD PM2.5 Emissions",
         subtitle = "Motor vehicle sources")


# 6. Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County,
# California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). Which city has seen greater changes over
# time in motor vehicle emissions?
la_mv <-
    SCC %>%
    filter(str_detect(EI.Sector, "^Mobile.*Vehicles")) %>%
    inner_join(NEI, by = "SCC") %>%
    filter(fips == "06037") %>%
    group_by(year) %>%
    summarize(la.emissions = sum(Emissions, na.rm = TRUE))
    
mv_compare <- inner_join(bmore_mv, la_mv, by = "year")

ggplot(mv_compare) +
    geom_point(aes(x = year, y = bmore.emissions)) +
    geom_point(aes(x = year, y = la.emissions))
