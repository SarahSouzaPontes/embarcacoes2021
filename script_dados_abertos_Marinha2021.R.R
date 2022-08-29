#Fonte dos dados: https://dados.gov.br/dataset/embarcacoes
library(dplyr)
library(ggplot2)#Carregar Pacote

setwd("/Users/sarahpontes/Desktop/embarcacoes")#Diretório de Trabalho

read.csv2("/Users/sarahpontes/Desktop/dt2021.csv",sep= ",", encoding = "utf-8")->dt2021 #carregando dados

View(dt2021)#visualizando dados

variable.names(dt2021)#verificando quais variáveis consta no banco

unique(dt2021$tipo) #visualizando quais tipos de embarcações existem em valores únicos

dt2021 %>% #selecionando e renomeando variáveis
  select(c(DN, tipo, Agencia_Capitania,UF, N_Tipo))%>% #selecionando variáveis de interesse
  filter(tipo =="Veleiro"|tipo =="Lancha"|tipo =="Escuna"|tipo =="Multicasco (Catamarã, Trimarã, Tetramarã, etc)"|tipo =="Jet Boat") -> dt2021 #Filtrando por embarcações mais utilizadas em passeios com base em pesquisas

dt2021 %>% 
  filter(UF == "BA")-> BA_2021 #Filtrando dados pela UF = Bahia

BA_2021 %>% 
  dplyr::group_by(tipo) %>% #agrupando por tipo de embarcação
  dplyr::summarise(numTipo=sum(N_Tipo))-> data_numTipo #somando o quantitativo de embarcações

ggplot(data_numTipo,aes(x=numTipo, y= tipo, fill = tipo))+#Plotando gráfico dos tipos de embarcações em 2021
  geom_bar(stat = "identity")+ #posição um ao lado do outro
  scale_fill_discrete() + #Preechimento para dados discretos
  theme(legend.position="none")+#Sem legenda
  xlab("")+ylab("")+#Declarando que os labels não possuem nada escrito
  coord_flip() # Direcionamento horizontal das barras

