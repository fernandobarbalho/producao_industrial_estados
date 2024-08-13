library(sidrar)
library(tidyverse)
library(lubridate)

info_sidra(8888, wb = TRUE)


# Mapeia os nomes dos meses para números
meses_para_numeros <- c(janeiro = "01", fevereiro = "02", março = "03", abril = "04",
                        maio = "05", junho = "06", julho = "07", agosto = "08",
                        setembro = "09", outubro = "10", novembro = "11", dezembro = "12")

# Função para converter o nome do mês e ano em uma data
converter_para_data <- function(data_string) {
  # Separa o mês e o ano
  partes <- strsplit(data_string, " ")[[1]]
  mes <- partes[1]
  ano <- partes[2]
  
  # Mapeia o mês para número
  mes_numero <- meses_para_numeros[[mes]]
  
  # Formata como data no primeiro dia do mês
  
  data_formatada <- paste(ano, mes_numero, "01", sep="-")
  
  return(data_formatada)
}




producao_fisica_estados<-  
  get_sidra(x = 8888,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            period = c("202301-202406"),
            #period = c("last" = 12),
            geo = "State",
            #geo.filter = "RS",
            classific = "C544",
            category =  list(c(129314 )), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

producao_fisica_estados$posicao<- as.Date(map_chr( producao_fisica_estados$D2N, converter_para_data))

producao_fisica_estados<-
  producao_fisica_estados %>%
  select(V, D1C, D1N, D2N, D3N, posicao)



names(producao_fisica_estados) <- c("valor", "codigo_ibge", "estado", "data_referencia", "tipo_dado", "posicao")



saveRDS(producao_fisica_estados, "producao_fisica_estados.rds")



