#########################################################
# FEUP - Master in Data Science and Engineering
# VPDA
##### Authors #####
# André Afonso - up2007002102@fe.up.pt
# Gabriel Carvalhal - up202103616@fe.up.pt
#########################################################

library(tidyverse)
library(extrafont)
library(readxl)
library(ggplot2)
library(maps)
library(sf)
library(ggtext)


#########################################################
##### Global theme for all the charts in the report #####
#########################################################

report_theme = function () {
  TITLE_COLOR <-"steelblue4"
  SUBTITLE_COLOR <- "steelblue4"
  FONT_FAMILY <- "Lato"
  
  theme_light(base_size = 12,) %+replace%
    theme(
      plot.title = element_text(size = 15,
                                hjust = 0.5,
                                color = TITLE_COLOR,
                                margin=margin(0,0,8,0),
                                face = "bold"),
      plot.subtitle = element_text(size = 12,
                                   hjust = 0.5,
                                   vjust = 1,
                                   margin=margin(0,0,12.5,0),
                                   color = SUBTITLE_COLOR),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key = element_rect(fill = "#E2E2E3", colour = "#E2E2E3"),
      legend.text = element_text(family = FONT_FAMILY, colour = "steelblue4", size = 10),
      axis.text = element_text(family = FONT_FAMILY, colour = "dimgray", face="bold", size = 10),
      axis.title = element_text(family = FONT_FAMILY, colour = "steelblue4", face = "bold", size = 12),
      axis.title.y = element_text(angle=90, margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(angle=0, margin = margin(t = 20, r = 0, b = 0, l = 0)),
      axis.line = element_line(colour = "darkgray"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      strip.text = element_text(family = FONT_FAMILY, colour = "white"),
      plot.caption = element_text(color = "grey23", hjust = 0, face= "italic", size = 10),
      plot.margin = unit(c(1,1,1,1), "cm")
    )
}

#Set working directory to file path
setwd("C:\\Users\\USER\\Documents\\[Universidade]-FEUP\\1_ANO\\Visualização e Preparação de Dados\\project\\data")

################################################################################
##########            Fig.1 - Macroeconomic impact                    ##########
################################################################################

#1 - Happiness
ds_felicidade <- read.csv("felicidade.csv")
ds_felicidade <- ds_felicidade %>%
  select(-Code) %>% 
  filter(Entity=='Portugal' & Year %in% c(2019,2020)) %>%
  rename(happiness = `Life.satisfaction.in.Cantril.Ladder..World.Happiness.Report.2021.`) %>%
  spread(Year, happiness) %>%
  mutate(variavel = 'Felicidade') %>%
  select(-Entity)

#2 GDP
ds_pib <- read_xlsx("PIB.xlsx",range=cell_rows(8:34))
ds_pib <- ds_pib %>%
  select('Anos','PT - Portugal') %>%
  filter(Anos %in% c(2019,2020)) %>%
  rename(PIB = 'PT - Portugal') %>%
  mutate(variavel = 'PIB') %>%
  spread(Anos, PIB)

#3 Employment
ds_emprego <- read_xlsx("Pop_empregada.xlsx",range=cell_rows(8:43))
ds_emprego <- ds_emprego%>%
  select('Anos','PT - Portugal')%>%
  filter( Anos %in% c(2019,2020))%>%
  rename(EMPREGO = 'PT - Portugal')%>%
  mutate(variavel = 'Emprego') %>%  
  spread(Anos, EMPREGO)

#4 Savings
ds_poupanca <- read_xlsx("Taxa_Poupanca_Familias.xlsx",range=cell_rows(8:34))
ds_poupanca <- ds_poupanca %>%
  select('Anos','PT - Portugal') %>%
  filter( Anos %in% c(2019,2020)) %>%
  rename(Poupanca= 'PT - Portugal') %>%
  mutate(variavel = 'Poupança') %>%    
  spread(Anos,Poupanca)


#Percent change
Impacto <- as.data.frame(rbind(ds_poupanca, ds_emprego, ds_pib, ds_felicidade))
Impacto$variavel = factor(Impacto$variavel)
Impacto$Percent <- ((Impacto[,3]-Impacto[,2])/Impacto[,2]) * 100


# Negative/Positive
Impacto$Status[Impacto$Percent >= 0] <- "Positivo"
Impacto$Status[Impacto$Percent < 0] <- "Negativo"

#Base of the plot
plot1 <- ggplot(data=Impacto, aes(x=variavel,y=Percent)) + 
  geom_col(aes(fill=Status), width = 0.039) + 
  geom_point(aes(fill=Status, color=Status, shape=Status), size=5, show.legend = FALSE) + 
  scale_shape_manual(values=c("Positivo"=24, "Negativo"=25)) +
  geom_text(aes(label = paste(round(Percent,2),'%')), vjust = 0.40 ,hjust=-0.40) +
  scale_y_continuous(limits=c(-10, 80), breaks = seq(-10, 80, by = 10)) + 
  geom_hline(yintercept = 0, color = "dimgray", linetype='dashed',size=.5) + 
  #column color
  scale_fill_manual( values = c( "indianred" , "darkgreen" )) +
  scale_color_manual( values = c( "indianred" , "darkgreen" )) +
  #setting the theme
  report_theme() +
  theme(
    legend.position = "none",
    legend.background = element_blank()
  ) + 
  #setting titles on the axis
  labs(x = "Indicadores" , y = "Variação percentual (%)",
       title = "Impacto macroeconómico da pandemia COVID-19 em Portugal",
       subtitle = "(variação % entre 2019 e 2020)",
       caption = "Fonte: PORDATA \n          ourworldindata.org"
  )
#view plot
plot1

################################################################################
##########            Figura 2 - Inflation rate                       ##########
################################################################################


#Read Data
inflacao <- read_xlsx("inflacao.xlsx",range='A9:AI33')

#Change Data
inflacao<-inflacao %>%
  select('Anos','PT - Portugal')%>%
  filter( Anos >=2005)%>%
  rename(Inflacao_Portugal= 'PT - Portugal')

# Interpolation to better fit area chart
interp <- approx(inflacao$Anos, inflacao$Inflacao_Portugal, n=1000)
ds_inf <- data.frame(Year=interp$x, InfInter=interp$y)
ds_inf$status[ds_inf$InfInter >= 0] <- "pos"
ds_inf$status[ds_inf$InfInter < 0] <- "neg"


plot2 <- ggplot(ds_inf, aes(x=Year, y=InfInter)) +
  geom_area(aes(fill=status), lwd=5, alpha=.7) +
  geom_line() +
  geom_hline(yintercept=0, color="dimgrey") +
  scale_fill_manual(values=c("red4","gray80"), guide='none') +
  scale_x_continuous(expand=c(0,0.2),breaks = seq(2005, 2020, by = 1))+
  scale_y_continuous(breaks = seq(-1, 4, by = 0.5)) +
  #choosing a theme
  report_theme() +
  theme(
    legend.background = element_blank(),
    panel.grid.major.y = element_line(colour='white')
  ) +
  #setting titles on the axis
  labs (
    x = "",
    y = "Taxa de Inflação (%)",
    title = "Evolução da taxa de inflação em Portugal" ,
    subtitle = paste( "(entre 2005 e 2020)" ),
    caption = "Fonte: PORDATA"  
  )

#view plot
plot2

################################################################################
##########            Figura 3 - Earnings per sector                  ##########
################################################################################

ds_setores = readxl::read_xlsx("Destaque_E-fatura_PT.xlsx", sheet="Dados 3", range="B4:C40")
colnames(ds_setores) <- c("setor", "valor")
ds_setores$setor <- plyr::mapvalues(ds_setores$setor, 
                                    from = c("Fabricação de têxteis, indústria do vestuário e do couro e dos produtos do couro",
                                             "Fabricação de coque e de produtos petroliferos refinados",
                                             "Fabricação de produtos farmacêuticos de base e de preparação farmacêuticas", 
                                             "Comércio por grosso e a retalho; reparação de veículos automóveis e motociclos",
                                             "Atividades de alojamento",
                                             "Atividades de restauração e similares", 
                                             "Consultoria e atividades relacionadas de programação informática; atividades dos serviços de informação",
                                             "Investigação científica e desenvolvimento",
                                             "Atividades artísticas, de espetáculos, desportistas e recreativas"), 
                                    to = c("Indústria têxtil",
                                           "Combustíveis",
                                           "Produtos Farmacêuticos",
                                           "Comércio a retalho",
                                           "Alojamento",
                                           "Restauração",
                                           "Consultoria Informática",
                                           "Investigação científica",
                                           "Artes, desporto e espetáculos")
)

#Filter only selected economic sectors
ds_setores = ds_setores %>% filter(setor %in% c("Indústria têxtil","Combustíveis","Produtos Farmacêuticos","Construção", "Telecomunicações", "Comércio a retalho","Alojamento","Restauração","Consultoria Informática","Investigação científica","Educação","Artes, desporto e espetáculos")) %>% 
  mutate(diff=ifelse(valor<0,1,2)) %>% 
  arrange(-valor) %>% 
  dplyr::mutate(row = row_number())
ds_setores$diff = factor(ds_setores$diff)

# Create the plot
plot3 <- ggplot(ds_setores, aes(x=reorder(setor,row), y = valor,fill = diff,color = diff)) +
  geom_bar(stat = "identity", position="stack") + 
  coord_flip() +
  ylim(-75,75) +
  geom_text(
    label = paste(ds_setores$valor, "%"),
    size = 3,
    vjust = 0.5,
    hjust=ifelse(ds_setores$diff==1, 1.2, -.2),
    color="black"
  ) +
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "black", size=1) +
  scale_fill_manual(values=c("indianred", "lightgreen")) +
  scale_color_manual(values=c("red4", "green4")) +
  labs(
    title="Variação homóloga do valor de faturação (p.p.)",
    subtitle="(março a dezembro de 2020)",
    x = "Sector de atividade",
    y = "Variação (%)",
    caption = "Fonte: INE"
  ) + 
  report_theme() + 
  theme(
    legend.position = "none",
    axis.line = element_line(colour = "darkgray"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
  )

#view plot
plot3

################################################################################
##########            Figura 4 - Investiment map R&D EU27             ##########
################################################################################


ds = readxl::read_xlsx("rd_gdp_eu27.xlsx", sheet="Sheet 1", range="A14:X40", col_names = F)
ds = ds %>% select(1,24)
colnames(ds) <- c("region", "valor")
ds[ds$region == 'Germany (until 1990 former territory of the FRG)',][1] = 'Germany'

maps = map_data("world", region = ds$region)

lab_data = maps %>% group_by(region) %>% dplyr::summarize(long = mean(long), lat = mean(lat))

ds_countries_plot = left_join(ds, maps, by = "region")

mapa = ggplot( ds_countries_plot , aes(long, lat) )+
  geom_polygon( aes(fill = valor, group = group ), color = "white") +
  geom_text( data = lab_data , aes(label = region), fontface="bold", color="black", size = 3, hjust = 0.5)+
  theme_void() +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen" ) +
  coord_sf () +
  labs(
    title="EU27 - Investimento I&DT",
    subtitle="(% do PIB em 2020)",
    fill = "% do PIB\n ",
    caption = "Fonte: Eurostat"
  ) + 
  report_theme() + 
  theme(
    plot.title = element_text(size = 15,
                              hjust = 0.5,
                              color = "steelblue4",
                              face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(family = "Arial", colour = "black", size = 8),
    legend.title = element_text(family = "Arial", face="bold", colour = "steelblue4", size = 10),
    plot.caption = element_text(color = "grey23", hjust = 0, face= "italic", size = 10),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.line = element_blank(),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_blank(),
  ) + 
  labs(x = NULL, y = NULL)

#view plot
mapa

################################################################################
##########            Fig. 5 - R&D Investment UE27                    ##########
################################################################################


ds = readxl::read_xlsx("rd_gdp_eu27.xlsx", sheet="Sheet 1", range="A14:X40", col_names = F)
ds = ds %>% select(1,24)
colnames(ds) <- c("region", "valor")
ds[ds$region == 'Germany (until 1990 former territory of the FRG)',][1] = 'Germany'
ds[ds$region == 'Czechia',][1] = 'Czech Republic'
ds = ds %>% arrange(valor) %>% 
  dplyr::mutate(row = row_number()) %>%
  mutate(IsPortugal = ifelse(region == "Portugal", "pt", "eu"))

country_flags = readr::read_csv("countries.csv")

ds = left_join(ds, country_flags, by = c("region" = "region"))
ds

image_url = sprintf("%s - <img src='%s' width='11' />", toupper(ds$code),  ds$image_url)


plot5 <- ggplot(ds, aes(x=reorder(region,row), y = valor,)) +
  geom_bar(aes(fill = IsPortugal), width=0.7, stat = "identity") +
  # scale_fill_gradient(low = "lightgreen", high = "seagreen4") +
  scale_fill_manual(values=c("pt"="dodgerblue4", "eu"="lightgreen")) +
  geom_hline(yintercept = mean(ds$valor), linetype="dotted", 
             color = "dodgerblue4", size = 0.5) + 
  geom_text(
    label = paste(ds$valor, "%"),
    size = 3,
    vjust = 0.5,
    hjust=-.2,
    color="black"
  ) +
  geom_label(aes(x = mean(valor), y = 2),
             label = sprintf("Média UE27: %s %%", round(mean(ds$valor),1)),
             color = "dodgerblue4", 
             size = 4, 
             fontface = "italic") +
  scale_x_discrete(
    name = NULL,
    labels = image_url
  ) +
  coord_flip() +
  ylim(0, 4) +
  labs(
    title="UE27 - Investimento em I&DT",
    subtitle = "(% do PIB em 2020)",
    x = "",
    y = "",
    caption = "Fonte: Eurostat"
  ) + 
  report_theme() + 
  theme(
    axis.text.y = element_markdown(color = "lightsteelblue4", size = 8),
    legend.position = "none"
  )

#view plot
plot5

################################################################################
##########      Fig. 6 - Tech job openings vs salary EU27             ##########
################################################################################

#Read earnings dataset
earn <- read_xlsx("earn_nt_net_page_spreadsheet.xlsx",sheet = 'Sheet 1', range='A12:K49', col_names = F)
earn

#select country and years
earn <- earn %>% select(1,10,11)
#rename columns
colnames(earn) <- c("region", "2019", "2020")
#change countries names
earn[earn$region == 'Germany (until 1990 former territory of the FRG)',][1] = 'Germany'
earn[earn$region == 'Czechia',][1] = 'Czech Republic'
#transform to dataframe
earn <- as.data.frame(earn)
#gather by year
earn <- earn %>% gather(year,value_earn,2:3)

#read employment dataset
work <- read_xlsx("hrst_st_ncat_page_spreadsheet.xlsx", sheet = 'Sheet 1', range='A13:T51', col_names = F)
#select country and years
work<-work %>% select(1,16,18)
#rename columns
colnames(work) <- c("region", '2019','2020')
#change countries names
work[work$region == 'Germany (until 1990 former territory of the FRG)',][1] = 'Germany'
work[work$region == 'Czechia',][1] = 'Czech Republic'

#transform to dataframe
work <- as.data.frame(work)

#gather the year
work <- work %>% gather(year,value_work,2:3)

#Join two dataframes
ds <- inner_join(earn, work, by = c("region" = "region", "year" = "year"))


country_list = c("Portugal", "Germany", "Switzerland", "Italy", "Ireland", "Spain", "France")

ds <- ds %>%
  filter(!str_detect(ds$region, 'Euro|United') & year %in% c('2019', '2020') & region %in% country_list) %>%
  mutate(color = case_when(
    region %in% country_list ~ "Colorize",
    TRUE ~ "Other Countries"
  )
  ) %>%
  mutate(plotname = ifelse(region %in% country_list & year==2019, region, ""))


#Build a scatter plot
plot6 <- ggplot(data=ds) + 
  geom_point(aes(x= as.numeric(value_earn),y=value_work, colour = year, fill=year), shape=21 , size=5, alpha=.7) +
  geom_text(aes(x= as.numeric(value_earn),y=value_work, label = plotname), size = 3, hjust = 1.5, vjust=-1.4, nudge_y = -0.25) +
  scale_fill_manual(values=c("gray", "deepskyblue4")) +
  scale_color_manual(values=c("gray", "deepskyblue4")) +
  scale_x_continuous(limits=c(0, 70000), breaks = seq(0, 80000, by = 10000)) +
  ylim(0, 2000) +
  labs(
    title="UE27 - Oportunidades de emprego vs Salário em ITC",
    subtitle = "(2019 vs 2020)",
    x =  "Salário Médio (???)",
    y ="Nº oportunidades de emprego (mil.)",
    caption = "Fonte: Eurostat"
  ) +
  report_theme() + 
  theme(
    # legend.position = "none",
    legend.background = element_blank(),
    legend.key=element_blank()
  )

#view plot
plot6
