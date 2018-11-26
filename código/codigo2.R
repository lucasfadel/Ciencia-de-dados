#Grafico de pizza para idade

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
idade=table(df$idade)
idade
lbls <- c("Entre 16 e 20 anos","Entre 21 e 25 anos", "Entre 26 e 30 anos", "Entre 30 e 35 anos", "Entre 36 e 40 anos", "Acima de 40 anos")
pct <- round(idade/sum(idade)*100, digits = 1)
pct
lbls <- paste0(pct, "% ", lbls)
lbls
png(filename = "gráficos/grafico_idade.png", width = 800, height = 500, pointsize = 16)
par(bg = "light blue")
pie(idade, labels = lbls, edges = 100, angle = 45, col = c("blue","purple","green","red","yellow","orange"), border = NULL, lty = NULL, main = "Idade dos correspondentes")
dev.off()


#Grafico de Barras para idade

png(filename="grÃ¡ficos/aed_survey_barra_sexo_tidy.png", width = 800, height = 500, pointsize = 16)
lbls <- c("Masc.", "Fem.")
par(bg = "white")
my_bar=barplot(sexo, border=F, names.arg = NA, las=2,
               col = c("lavender", "cornsilk","black"),
               ylab = "Quantidade",
               xlab = " ",
               ylim = c(0,130),
               main = "GÃªnero dos respondentes")
text(my_bar, sexo+4, paste("n = ",sexo,sep=""),cex=1) 
legend("topleft", legend = c("Masculino","Feminino"),
       col = c("lavender", "cornsilk","black"),
       bty = "n", pch=20 , pt.cex = 2, cex = 0.8, 
       horiz = FALSE, inset = c(0.05, 0.05))
dev.off()


#Grafico de Barra - Plataformas de Redes Sociais - Questionario B-I

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
B1 = matrix(data = c(df$facebook, df$twitter, df$whatsapp, df$linkedin, df$youtube, df$instagram, df$snapchat, df$tumblr,df$pinterest),nrow = 61, ncol = 9)
B1
B1.facebook = table(df$facebook, exclude = 0)
B1.twitter = table(df$twitter, exclude = 0)
B1.whatsapp = table(df$whatsapp, exclude = 0)
B1.linkedin = table(df$linkedin, exclude = 0)
B1.youtube = table(df$youtube, exclude = 0)
B1.instagram = table(df$instagram, exclude = 0)
B1.snapchat = table(df$snapchat, exclude = 0)
B1.tumblr = table(df$tumblr, exclude = 0)
B1.pinterest = table(df$pinterest, exclude = 0)
png(filename="gráficos/plataformas_mais_usadas.png", width = 800, height = 500, pointsize = 16)
par(bg = "white")
my_bar=barplot(B1, border=F, col = "red",names.arg = expression(Facebook, Twitter,Whatsapp, Linkedin,Youtube,Instagram,Snapchat,Tumblr,Pinterest),
               ylab = "Quantidade",
               xlab = " ",
               ylim = c(0,130),
               main = "Plataformas de Redes Sociais Mais Usadas")
text(1,B1.facebook+4,cex=1,paste("n = ",B1.facebook,sep=""),pos = 2, offset = 0.20)
text(2,B1.twitter+4,cex=1,paste("n = ",B1.twitter,sep=""))
text(3,B1.whatsapp+4,cex=1,paste("n = ",B1.whatsapp,sep=""))
text(4,B1.linkedin+4,cex=1,paste("n = ",B1.linkedin,sep=""),pos = 4, offset = 0.01)
text(5,B1.youtube+4,cex=1,paste("n = ",B1.youtube,sep=""),pos = 4, offset = 0.25)
text(6,B1.instagram+4,cex=1,paste("n = ",B1.instagram,sep=""),pos = 4, offset = 0.6)
text(7,B1.snapchat+4,cex=1,paste("n = ",B1.snapchat,sep=""),pos = 4, offset = 1.4)
text(8,B1.tumblr+4,cex=1,paste("n = ",B1.tumblr,sep=""),pos = 4, offset = 2)
text(9,B1.pinterest+4,cex=1,paste("n = ",B1.pinterest,sep=""),pos = 4, offset = 2.5)
dev.off()


#Grafico de Barras - Principais motivos de uso - Questionario B-II

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
B1 = matrix(data = c(df$contato, df$atualizado, df$preencher, df$encontrar, df$compopiniao, df$compfoto, df$amigosja, df$profnetwork, df$novaamizade, df$compdetalhe),nrow = 61, ncol = 10)
B1
png(filename="gráficos/principais_motivos.png", width = 1700, height = 500, pointsize = 16)
par(bg = "white")
my_bar=barplot(B1, border=F, col = "red",names.arg = expression(Manter_contato, Manter_atualizado, Tempo_livre, 
                                                                Conteudo_interessante,Compart_opinioes, Compart_fotos, Amigos_estao,
                                                                Network, Conhecer_pessoas, Assuntos_trabalho),
               ylab = "Quantidade",
               xlab = " ",
               ylim = c(0,130),
               main = "Principais motivos do uso de redes sociais")
dev.off()


#Grafico de Pizza - para Uso das mídias sociais - Tempo Gasto - Questionario B-III-3

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
tempogasto=table(df$tempogasto)
tempogasto
lbls <- c("Nenhum","de 5 a 10 minutes", "de 10 a 30 minutes", "de 30 minutos até 1 hora", "de 1 a 2 horas", "de 2 a 3 horas","de 3 a 4 horas", "de 4 a 5 horas", "mais de 5 horas")
pct <- round(tempogasto/sum(tempogasto)*100, digits = 1)
pct
lbls <- paste0(pct, "% ", lbls)
lbls
png(filename = "gráficos/grafico_tempo_gasto.png", width = 500, height = 300, pointsize = 10)
par(bg = "light blue")
pie(tempogasto, labels = lbls, edges = 100, angle = 45, border = NULL, lty = NULL, main = "Tempo Gasto em Redes Sociais")
dev.off()


#Grafico de pizza - Deve ser utilizada pelos professores - Questionario B-III-4

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
usoacademico=table(df$usoacademico)
usoacademico
lbls <- c("Não","Sim", "Sim, porém com restrições", "Não sei / Não tenho opinião")
pct <- round(usoacademico/sum(usoacademico)*100, digits = 1)
pct
lbls <- paste0(pct, "% ", lbls)
lbls
png(filename = "gráficos/grafico_utilizada_professores.png", width = 500, height = 300, pointsize = 10)
par(bg = "light blue")
pie(usoacademico, labels = lbls, edges = 100, angle = 45, border = NULL, lty = NULL, main = "Midia social deve ser utilizada pelos Professores?")
dev.off()


#Grafico de pizza - melhor forma de aproximação - Questionario B-III-5

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
profchegaal=table(df$profchegaal)
profchegaal
lbls <- c("Não","Sim","Não sei / Não tenho opinião")
pct <- round(profchegaal/sum(profchegaal)*100, digits = 1)
pct
lbls <- paste0(pct, "% ", lbls)
lbls
png(filename = "gráficos/grafico_aproximacao_professor.png", width = 800, height = 500, pointsize = 16)
par(bg = "light blue")
pie(profchegaal, labels = lbls, edges = 100, angle = 45, border = NULL, lty = NULL, main = "Midia social deve ser utilizada pelos Professores?")
dev.off()


#Grafico de Pizza - Melhores Resultados - Questionario B-III-6

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
melhoraresul=table(df$melhoraresul)
melhoraresul
lbls <- c("Não","Sim","Não sei / Não tenho opinião")
pct <- round(melhoraresul/sum(melhoraresul)*100, digits = 1)
pct
lbls <- paste0(pct, "% ", lbls)
lbls
png(filename = "gráficos/grafico_melhores_resultados.png", width = 500, height = 300, pointsize = 10)
par(bg = "light blue")
pie(melhoraresul, labels = lbls, edges = 100, angle = 45, border = NULL, lty = NULL, main = "Melhores resultados se as mídias sociais estiverem integradas às aulas?")
dev.off()


#Grafico de barras - Dificuldades midias sociais - Questionario B-III-7

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
B1 = matrix(data = c(df$distracao, df$usoindev, df$prejintera, df$bulling, df$continadeq),nrow = 61, ncol = 5)
B1
png(filename="gráficos/dificuldades_redes_sociais.png", width = 800, height = 500, pointsize = 16)
par(bg = "white")
my_bar=barplot(B1, border=F, col = "red",names.arg = expression(distracao, uso_indevido, prejudica_interacao, cyberbullying, conteudo_inadequado),
               ylab = "Quantidade",
               xlab = " ",
               ylim = c(0,130),
               main = "Principais dificuldades do uso das mídias sociais em um ambiente educacional")
dev.off()


#Graficos de Pizza - Avaliacao de Recursos - Questionario B-IV

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
evioinfo=table(df$evioinfo)
evioinfo
lbls <- c("Excelente","Bom","Indiferente","Pobre", "Muito Pobre")
pct <- round(evioinfo/sum(evioinfo)*100, digits = 1)
pct
lbls <- paste0(pct, "% ", lbls)
lbls
png(filename = "gráficos/envio_de_informacoes_pais.png", width = 800, height = 500, pointsize = 16)
par(bg = "light blue")
pie(evioinfo, labels = lbls, edges = 100, angle = 45, border = NULL, lty = NULL, main = "Envio de informações da escola para os pais.")
dev.off()

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
grandeuso=table(df$grandeuso)
grandeuso
lbls <- c("Excelente","Bom","Indiferente","Pobre", "Muito Pobre")
pct <- round(grandeuso/sum(grandeuso)*100, digits = 1)
pct
lbls <- paste0(pct, "% ", lbls)
lbls
png(filename = "gráficos/uso_promocional.png", width = 800, height = 500, pointsize = 16)
par(bg = "light blue")
pie(grandeuso, labels = lbls, edges = 100, angle = 45, border = NULL, lty = NULL, main = "Motivos promocionais.")
dev.off()

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
facegrupo=table(df$facegrupo)
facegrupo
lbls <- c("Excelente","Bom","Indiferente","Pobre", "Muito Pobre")
pct <- round(facegrupo/sum(facegrupo)*100, digits = 1)
pct
lbls <- paste0(pct, "% ", lbls)
lbls
png(filename = "gráficos/grupo_facebook.png", width = 800, height = 500, pointsize = 16)
par(bg = "light blue")
pie(facegrupo, labels = lbls, edges = 100, angle = 45, border = NULL, lty = NULL, main = "Grupos no Facebook para se comunicar com os alunos")
dev.off()

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
trocainfo=table(df$trocainfo)
trocainfo
lbls <- c("Excelente","Bom","Indiferente","Pobre", "Muito Pobre")
pct <- round(trocainfo/sum(trocainfo)*100, digits = 1)
pct
lbls <- paste0(pct, "% ", lbls)
lbls
png(filename = "gráficos/troca_informacao.png", width = 800, height = 500, pointsize = 16)
par(bg = "light blue")
pie(trocainfo, labels = lbls, edges = 100, angle = 45, border = NULL, lty = NULL, main = "Uso para troca de informações")
dev.off()

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
compinfopal=table(df$compinfopal)
compinfopal
lbls <- c("Excelente","Bom","Indiferente","Pobre", "Muito Pobre")
pct <- round(compinfopal/sum(compinfopal)*100, digits = 1)
pct
lbls <- paste0(pct, "% ", lbls)
lbls
png(filename = "gráficos/troca_informacao_professor.png", width = 800, height = 500, pointsize = 16)
par(bg = "light blue")
pie(compinfopal, labels = lbls, edges = 100, angle = 45, border = NULL, lty = NULL, main = "Estudantes e professores podem compartilhar informações entre si")
dev.off()

if (!require(readxl)) install.packages('readxl')
library(readxl)
df <- read_xlsx("dados/umses_graduacao_2018_vtidy.xlsx", sheet="dados")
quadrovirtual=table(df$quadrovirtual)
quadrovirtual
lbls <- c("Excelente","Bom","Indiferente","Pobre", "Muito Pobre")
pct <- round(quadrovirtual/sum(quadrovirtual)*100, digits = 1)
pct
lbls <- paste0(pct, "% ", lbls)
lbls
png(filename = "gráficos/pinterest.png", width = 800, height = 500, pointsize = 16)
par(bg = "light blue")
pie(quadrovirtual, labels = lbls, edges = 100, angle = 45, border = NULL, lty = NULL, main = "Pinterest")
dev.off()