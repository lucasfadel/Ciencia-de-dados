# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Versão 4.0 2018
# Observação: Esta nova versão foi testada no ambiente MacOsx.
#
# Esta versão gera ("plota") os gráficos mas não os mostra na janela "Plots" abaixo à direita do IDE RStudio.
# Os gráficos são gerados e gravados nos arquivos com a extensão .PNG.
# Para tanto, foi criada uma pasta (folder), abaixo da pasta principal com a denominacao de "gráficos". 
#
## Análise Exploratória dos Dados realizada na planilha modificada localizada na pasta dados
## Neste script R o nome da planilha ficou sendo "umses_graduacao_2018.xlsx"
#
# Carregando a biblioteca para leitura e gravação de planilhas Excel formato 'xlsx'
if (!require(readxl)) install.packages('readxl')
library(readxl)
#
# Lendo a planilha já com o nome modificado
# A opção "sheet = "dados" especifica o nome da área de trabalho que será lida da planilha
#
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
#
# A função "head(df, 3)" mostra as três primeiras linhas trazidas para o data.frame de nome "df".
# O objetivo é o de fazer uma inspeção visual das colunas e verificar a importação correta dos dados.
#
head(df, 3)
#
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# Iniciando a geração e gravação dos gráficos
#
# Seção I.Dados censitários
#
# Gênero
sexo=table(df$genero)
sexo
# Exibição do conteúdo para simples conferência.
#  2  3 
# 37 24 
# Fatores, onde:
# 1.Prefiro não declarar
# 2.Masculino e 
# 3.Feminino
#
# A variável "lbls" é utilizada neste e nos próximos gráficos para definir os rótulos mostrados na figura
# Observar que a opção "1.Prefiro não declarar" não existe nesta amostra de dados!
lbls <- c("Masculino", "Feminino")
# A variável "pct" é utilizada neste e nos outros gráficos para definir os valores exibidos em porcentagem
# O valor está arredondado para uma casa decimal
pct <- round(sexo/sum(sexo)*100, digits=1)
pct
# Exibição do resultado do cálculo. Apenas para conferência.
#    2    3 
# 60.7 39.3  
#
# O primeiro gráfico a ser elaborado, será do tipo pizza (pie) para a variável "sexo".
# A variável "lbls" concatena os elementos (nomes) e os valores (em porcentagem)
lbls <- paste0(pct,"% ", lbls) # add percents to labels
lbls
#[1] "60.7% Masculino" "39.3% Feminino"
# A chamada da funcao png abaixo serve para plotar o grafico em um arquivo, 
# ao inves de plotar na janela "Plots" no RStudio. 
# Poderiam ser utilizados outros formatos de arquivo, tais como: 'bmp', 'pdf', 'jpeg' ou 'tiff'.
#
# Obs: lembrar de criar um diretório, abaixo do diretório de trabalho atual, com o nome "gráficos"!!
#
png(filename="gráficos/aed_survey_sexo_tidy.png", width = 800, height = 500, pointsize = 16)
#
# Definindo a cor de fundo do gráfico a ser exibido: azul clara
par(bg = "light blue")
#
# Geração do gráfico tipo pizza (pie)
pie(sexo, labels = lbls, edges = 100, angle = 45, col = c("blue","purple", "green3"), border = NULL,
    lty = NULL, main = "Gênero dos respondentes")
#
# Não se esquecer de chamar a funcao "dev.off()" para fechar o arquivo gravado!
# Este comando "dev.off()" é utilzado em complemento com a funcao png (ou outras funcoes para gerar arquivos graficos)
dev.off()
#
# Outra forma de visualização da variável "sexo", utilizando gráfico de barras
# 
png(filename="gráficos/aed_survey_barra_sexo_tidy.png", width = 800, height = 500, pointsize = 16)
# Gráfico do tipo "barplot"
# Observar que a opção "1.Prefiro não declarar" não existe nesta amostra de dados!
lbls <- c("Masc.", "Fem.")
# Definindo a cor de fundo branca (default)
par(bg = "white")
# Geração do gráfico tipo barra (barplot)
my_bar=barplot(sexo, border=F, names.arg = NA, las=2,
               col = c("lavender", "cornsilk","black"),
               ylab = "Quantidade",
               xlab = " ",
               ylim = c(0,130),
               main = "Gênero dos respondentes")
# Adicionando o texto a ser exibido 
text(my_bar, sexo+4, paste("n = ",sexo,sep=""),cex=1) 

#Agora adicionando a legenda ao gráfico
legend("topleft", legend = c("Masculino","Feminino"),
       col = c("lavender", "cornsilk","black"),
       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, 
       horiz = FALSE, inset = c(0.05, 0.05))
dev.off()
# Não se esquecer de chamar a funcao "dev.off()" para fechar o arquivo gravado!
#
# Fim do programa !!
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
