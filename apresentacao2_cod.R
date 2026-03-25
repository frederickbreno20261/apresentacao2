require(openxlsx)            # Leitura de base de dados
require(dplyr)               # Manipulação de base de dados
require(gtsummary)           # Tabelas automáticas
require(gt)                  # Tabelas automáticas
require(rstatix)             # Coeficiente de Cramer
require(broom)


theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",") # formatação em português (vírgula pra decimais e ponto para milhares)

Dados = read.xlsx("DadosAviao.xlsx")


DadosQuali = Dados %>% select(Servico_Online, Genero, Cliente, Idade,
                              Tipo_Viagem, Classe, Distancia,
                              Atraso_Partida, Atraso_Chegada)

tbl_summary(data = DadosQuali)

tbl_summary(data = DadosQuali,
            by = Servico_Online)

tbl_summary(data = DadosQuali,
            by = Servico_Online,
            percent = "row")

chisq.test(Dados$Genero,Dados$Servico_Online)$expected
chisq.test(Dados$Cliente,Dados$Servico_Online)$expected
chisq.test(Dados$Idade,Dados$Servico_Online)$expected
chisq.test(Dados$Tipo_Viagem,Dados$Servico_Online)$expected
chisq.test(Dados$Classe,Dados$Servico_Online)$expected
chisq.test(Dados$Distancia,Dados$Servico_Online)$expected
chisq.test(Dados$Atraso_Partida,Dados$Servico_Online)$expected
chisq.test(Dados$Atraso_Chegada,Dados$Servico_Online)$expected


tbl_summary(data = DadosQuali,
            by = Servico_Online,
            percent = "row")%>%
  add_p()


chisq.test(Dados$Cliente, Dados$Servico_Online)$stdres

chisq.test(Dados$Idade, Dados$Servico_Online)$stdres

chisq.test(Dados$Classe, Dados$Servico_Online)$stdres


# Função que calcula o coeficiente de Cramer
cramer_fun <- function(data, variable, by, ...) {
  tab <- table(data[[variable]], data[[by]])
  v <- cramer_v(tab)
  tibble::tibble(`**Cramér**` = round(v, 3))
}

# Código da tabela
tbl_summary(data = DadosQuali,
            by = Servico_Online,
            percent = "row",
            label = list(
              Genero ~ "Gênero<sup>Q</sup>",
              Cliente ~ "Tipo de Cliente<sup>Q</sup>",
              Idade ~ "Idade<sup>Q</sup>",
              Tipo_Viagem ~"Tipo de Viagem<sup>Q</sup>",
              Classe ~ "Classe<sup>Q</sup>",
              Distancia ~ "Distância<sup>Q</sup>",
              Atraso_Partida ~"Atraso na Partida<sup>Q</sup>",
              Atraso_Chegada ~"Atraso na Chegada<sup>Q</sup>"
            )
)%>%
  add_p(pvalue_fun = label_style_pvalue(digits = 3)) %>%
  bold_p(t = 0.05) %>%
  add_stat(fns = everything() ~ cramer_fun)%>%
  modify_spanning_header(all_stat_cols() ~ "**Satisfação com o Serviço Online**") %>%
  modify_header(label ~ "**Variáveis**") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)")%>%
  modify_bold(
    columns = stat_1,  # primeira coluna
    rows = (variable == "Cliente" & label == "Cliente Fiel"| label=="Cliente Nao Fiel") | 
      (variable == "Classe" & label == "Economica" | label=="Executiva") |
      (variable == "Idade" & label == "<30 anos" | label=="30-50 anos")
  )%>%
  modify_bold(
    columns = stat_2, # segunda coluna
    rows = (variable == "Classe" & label == "Economica" | label == "Executiva")
  )%>%
  modify_bold(
    columns = stat_3,  # terceira coluna
    rows = (variable == "Classe" & label == "Economica"| label=="Executiva") |
      (variable == "Idade" & label == "<30 anos") |
      (variable == "Cliente" & label == "Cliente Fiel" | label == "Cliente Nao Fiel"))%>%
  modify_footnote(everything() ~ NA)%>%
  as_gt() %>%                   
  fmt_markdown(columns = label) %>%
  tab_options(
    table.font.size = "20px",    
    heading.title.font.size = "26px",
    column_labels.font.size = "22px"
  )
