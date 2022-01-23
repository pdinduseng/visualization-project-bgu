library(maps)
library(ggplot2)
library(readr)
library(hrbrthemes)
library(zoo)
library(dplyr)
library(lubridate)

data <- read_csv("panel_v3.csv", locale = locale(date_names = "he", encoding = "UTF-8"))
data <-data %>% filter(!is.na(Quarter)) %>% filter(!(is.na()))
data$Group<- data$Group %>% replace(is.na(.) ,"שאר עבירות")
data <- data %>% mutate(Group = replace(Group, Group == "שאר עבירות", "קבוצת כל השאר"))
data <- data %>% mutate(Group = replace(Group, Group == "סעיפי הגדרה", "קבוצת כל השאר"))

data$year_month <- as.Date(as.yearqtr(data %>% select(Quarter)%>% unlist, format = "%Y-Q%q"), frac = 1)
data$Year <- year(data$year_month)
data$Month <- month(data$year_month)
grouped_data_plot <- data %>% group_by(Yeshuv,Group,Year) %>% summarise(Num_Tikim = sum(Tikim),
                                                                        Population = mean(total_population),
                                                                        Jews_Population = mean(jews),
                                                                        Arab_Population = mean(arabs),
                                                                        Jews_and_others_Population = mean(jews_and_others),
                                                                        Salary = mean(avg_salary),
                                                                        Unemployment_rate=mean(unemployed.rate),
                                                                        Women_Population = mean(population_woman),
                                                                        Man_Population = mean(population_man),
                                                                        Population_Density = mean(Population.density.per.km)
)

grouped_data_plot$arabs_population_rate <- grouped_data_plot$Arab_Population/grouped_data_plot$Population %>% round(.,3)
grouped_data_plot$Tikim_by_population <- grouped_data_plot$Num_Tikim/grouped_data_plot$Population

grouped_data_plot$Tikim_by_population_metric <- grouped_data_plot$Tikim_by_population*1000000
grouped_data_plot$Tikim_by_population <- grouped_data_plot$Num_Tikim/grouped_data_plot$Population

grouped_data_plot_2019 <- grouped_data_plot %>% filter(Year ==2019)
grouped_data_plot_2019 %>% select(arabs_population_rate) %>% summary()
grouped_data_plot_2019$arab_population_rate_categories <- cut(grouped_data_plot_2019$arabs_population_rate, 
                                                              breaks = c(0, .25, .5, .75,.9,Inf),
                                                              labels = c("0-25%", "25-50%", "50-75%","75-90%","90+%"),
                                                              right = FALSE)
ggplot(grouped_data_plot_2019 %>% filter(!is.na(arab_population_rate_categories)), aes(x=Group, y=Tikim_by_population, fill = arab_population_rate_categories)) + 
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(trans='log2') + 
  ylab("סקאלה לוגרתמית של יחס תיקים לאוכלוסייה") + 
  xlab("סוג עבירה") +
  ggtitle("יחס אחוז אוכלוסיה ערבית לסוג עבירות") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1),
        axis.ticks.y=element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +  
  scale_fill_manual(values=c( "#4169E1","#FF0000","#DC143C"),name = "אחוז ערבים ליישוב")

grouped_data_plot_year <- data %>% group_by(Year,Month) %>% summarise(Num_Tikim = sum(Tikim),
                                                                      Population = mean(total_population),
                                                                      Jews_Population = mean(jews),
                                                                      Arab_Population = mean(arabs),
                                                                      Jews_and_others_Population = mean(jews_and_others),
                                                                      Salary = mean(avg_salary),
                                                                      Unemployment_rate=mean(unemployed.rate),
                                                                      Women_Population = mean(population_woman),
                                                                      Man_Population = mean(population_man),
                                                                      Population_Density = mean(Population.density.per.km))

data$year_month <- as.Date(as.yearqtr(data %>% filter(!is.na(Quarter)) %>% select(Quarter)%>% unlist, format = "%Y-Q%q"), frac = 1)

grouped_data_plot_yearly <- data %>% group_by(year_month) %>% summarise(Num_Tikim = sum(Tikim))
x <-grouped_data_plot_yearly$year_month

y <- grouped_data_plot_yearly$Num_Tikim
ggplot(data = grouped_data_plot_yearly, aes(x=x, y=y)) +
  geom_line( color ="#4169E1") + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+
  theme_bw() +
  scale_x_date(date_labels = "%Y %b",date_breaks = "3 month")+
  theme(axis.text.x=element_text(angle=60, hjust=1),plot.title = element_text(hjust = 0.5)) +
  ylab("כמות תיקים")+
  ggtitle("מגמת כמות תיקים לרבעון") +
  xlab("")




