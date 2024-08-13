producao_fisica_estados <- readRDS("~/github/producao_industrial_estados/producao_fisica_estados.rds")


#Estados com variação positiva
producao_fisica_estados %>%
  filter(tipo_dado == "PIMPF - Variação mês/mês imediatamente anterior, com ajuste sazonal (M/M-1)",
         posicao == "2024-06-01",
         valor>0) %>%
  arrange(desc(valor))


producao_fisica_estados %>%
  filter(tipo_dado == "PIMPF - Variação mês/mesmo mês do ano anterior (M/M-12)",
         posicao == "2024-06-01",
         valor>0) %>%
  arrange(desc(valor))


#Exportação de dados para arquivo Excel
producao_fisica_estados %>%
  filter(tipo_dado == "PIMPF - Variação mês/mês imediatamente anterior, com ajuste sazonal (M/M-1)",
         posicao == "2024-06-01") %>%
  arrange(desc(valor)) %>%
  writexl::write_xlsx("variacao_industria_estados_junho_maio_2024.xlsx")


producao_fisica_estados %>%
  filter(tipo_dado == "PIMPF - Variação mês/mesmo mês do ano anterior (M/M-12)",
         posicao == "2024-06-01") %>%
  arrange(desc(valor))%>%
  writexl::write_xlsx("variacao_industria_estados_junho_2024_maio_2023.xlsx")


producao_fisica_estados %>%
  #filter(D1N %in% c("Rio Grande do Sul","São Paulo", "Rio de Janeiro", "Minas Gerais")) %>%
  filter(D1N %in% c("Rio Grande do Sul")) %>%
  ggplot(aes(x=posicao, y= V)) +
  geom_col() +
  facet_wrap(D1N~.)




dados_grafico<-
  producao_fisica_estados %>%
  filter(D1N %in% c("Rio Grande do Sul")) %>%
  mutate(posicao = as.character(posicao))

vjust_grafico <- ifelse(dados_grafico$V > 0 , -0.5, 0.9)


dados_grafico %>%
  ggplot(aes(x=posicao, y=V)) +
  geom_col() +
  geom_text(aes(label = paste0(V,"%")), vjust = vjust_grafico, size= 3) +
  # geom_text(aes(label=V, y=ifelse(V > 0, V + 2, V - 2)), 
  #           position=position_dodge(width=0.9), vjust=ifelse(V > 0, -0.5, 1.5)) +
  facet_wrap(D1N~.) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  labs(x="",
       y="",
       title = "Variação da produção industrial em relação ao mês anterior")


producao_fisica_estados %>%
  #filter(D1N %in% c("Rio Grande do Sul","São Paulo", "Rio de Janeiro", "Minas Gerais")) %>%
  filter(D1N %in% c("Rio Grande do Sul")) %>%
  ggplot(aes(x=posicao, y= V)) +
  geom_line(aes(color = D1N, group = D1N), show.legend = FALSE) +
  geom_text()
facet_wrap(D1N~.)