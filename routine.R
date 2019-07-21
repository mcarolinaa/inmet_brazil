#--------
## Part 1: Explore/define weather stations of interest

library(tidyverse)
library(rvest)

# Web scraping at BDMEP weather stations list
cod.estacoes <- read_html("http://www.inmet.gov.br/projetos/rede/pesquisa/lista_estacao.php")

# Organize stations into one table
tabela_estacoes <-
    cod.estacoes %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table()

colnames(tabela_estacoes)

# tidying columns: city (CIDADE), state (ESTADO)
# deleting line 1, no data
tabela_estacoes <-
    tabela_estacoes %>%
    separate("X2", c("CIDADE", "ESTADO"), sep = "-")

tabela_estacoes <-
    tabela_estacoes %>%
    select(ESTACAO = CIDADE, ESTADO = ESTADO, CODIGO_OMM = X3) %>%
    slice(-1)
    
# Tidying station and state columns 
tabela_estacoes$ESTACAO <- str_trim(tabela_estacoes$ESTACAO)
tabela_estacoes$ESTADO <- str_trim(tabela_estacoes$ESTADO)

# checking
head(tabela_estacoes)
glimpse(tabela_estacoes)

# Getting stations of interest
estacoes_mt <-
    tabela_estacoes %>%
    filter(ESTADO == "MT")
    
    
 #--------   
 ## Part 2: Login BDMEP and getting data
 
 # Obs: in this part, I used code and information obtained from here:
 # http://r-br.2285057.n4.nabble.com/R-br-r-baixando-dados-inmet-td4660459.html
 # http://r-br.2285057.n4.nabble.com/R-br-RCurl-td4659610.html
 
library(RCurl)
library(bitops)

# Acessing your BDMEP account
myParams=list(
    mCod="seuemail", # change!
    mSenha="suasenha", # change!
    btnProcesso = " Acessar ")

# Login INMET
url_login <- "http://www.inmet.gov.br/projetos/rede/pesquisa/inicio.php"

# Adjust links accoridng to weather station code
# Cáceres (Mato Grosso state) (mtca)
url_mtca <- "http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao=83405&btnProcesso=serie&mRelDtInicio=01/01/1940&mRelDtFim=30/07/2017&mAtributos=,,1,1,,,,,,1,1,,1,1,1,1,"

# Diamantino (Mato Grosso state) (mtdi)
url_mtdi <- "http://www.inmet.gov.br/projetos/rede/pesquisa/gera_serie_txt.php?&mRelEstacao=83309&btnProcesso=serie&mRelDtInicio=01/01/1940&mRelDtFim=30/07/2017&mAtributos=,,1,1,,,,,,1,1,,1,1,1,1,"

# Getting data and putting them into a list
myCurl <- getCurlHandle()
curlSetOpt(cookiejar="cookies.txt", useragent="Mozilla/67.0.4" , followlocation=TRUE, curl=myCurl)

login <- postForm(url_login, .params=myParams, curl=myCurl)

lista_dados <- mapply(getURLContent, list(url_mtca, url_mtdi), MoreArgs = list(curl = myCurl))
edit(lista_dados)

# [Optional]: working with 1 element from list
# AND/OR exporting raw data to work on later
mtca <- unlist(lista_dados[1])
mtdi <- unlist(lista_dados[2])

write.table(mtca, "mtca.txt")
write.table(mtdi, "mtdi.txt")
 
 
#-------
# Part 3: Tidying data
# (Assuming you exported)
 
# Importing: 2 options:

# 1-Importing 1 by 1
estacao_inmet <- read.table("mtca.txt", header = TRUE, skip = 48, sep = ";")

# More interactively
estacao_inmet <- read.table(file.choose(), header = TRUE, skip = 48, sep = ";")
head(estacao_inmet)

# 2-Importing several: list
folder <- "seudiretorio"  
file_list <- list.files(path=folder, pattern="*.txt") 

estacoes_inmet <- lapply(seq(1,2), function(i)  # Change "seq" with your number of stations!
    read.delim(paste(folder, file_list[i], sep = ""),
          skip = 48, quote = "", sep = ";"))
glimpse(estacoes_inmet) 

# Organizing using data from a list as example
# (If using only 1 station, don't use lapply structure)  

# Working with dates
library(lubridate)

inmet.list <- lapply(estacoes_inmet, function(x) {
    x[,"Data"] <- dmy(x[,"Data"]) ; x}) ; str(inmet.list)

colnames(inmet.list[[1]]) # check columns we have

# I have a 12 column with no data
inmet.list <- lapply(inmet.list, "[",  c(1:11))

# Select an rename columns according to your workflow 
inmet.list <- lapply(inmet.list, function (x) {
    colnames(x) <- c("STATION", "DATE", "TIME", "RAIN", "TMAX", "TMIN",
                     "INSO", "EVAP", "TAVG", "RHUM", "WIND"); x})

# Putting daily observations into one row/day
inmet.list <-
    lapply (inmet.list, function(x,...) { # concatenate both daily values into one row
        x %>% group_by_("DATE") %>% summarise (STATION = paste(STATION, collapse = ","),
                                               DATES = paste(DATE, collapse = ","),
                                               TIME = paste(TIME, collapse = ","),
                                               RAIN = paste(RAIN, collapse = ","),
                                               TMAX = paste(TMAX, collapse = ","),
                                               TMIN = paste(TMIN, collapse = ","),
                                               INSO = paste(INSO, collapse = ","),
                                               EVAP = paste(EVAP, collapse = ","),
                                               TAVG = paste(TAVG, collapse = ","),
                                               RHUM = paste(RHUM, collapse = ","),
                                               WIND = paste(WIND, collapse = ","))
    }) 

# Separate, select columns that contain data and rename again
inmet.list <- 
    lapply (inmet.list, function(x,...) {
        x %>% splitstackshape::cSplit(c("TIME","RAIN", "TMAX", "TMIN", "INSO", "EVAP", "TAVG","RHUM", "WIND")) %>%  
            select(DATE, RAIN_2, TMAX_1, TMIN_2, INSO_1, EVAP_1, TAVG_1, RHUM_1, WIND_1) %>% 
            set_names(c("DATE", "RAIN", "TMAX", "TMIN", "INSO", "EVAP", "TAVG", "RHUM", "WIND")) 
    })

# [Optional]: If you want to export station with a name 
inmet.list <- setNames(inmet.list, c("mtca", "mtdi"))

# Exporting stations (.csv)

for (i in seq_along(inmet.list)) {
    filename <- paste(names(inmet.list[i]), ".csv")
    write.csv(inmet.list[[i]], filename, row.names = FALSE)
}
