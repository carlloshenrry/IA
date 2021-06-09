#1) Realizar a leitura do dataset em um dataframe.
dataset = read.csv("C:/Users/carlo/Documents/Faculdade 2021/IA/Projeto 2/leaf.csv", header = FALSE)
View(dataset)

#-------------------------------Plots e Boxplots---------------------------
matriz2 <- matrix(data = 1:100, nrow = 1, ncol = 100) 

#Se quiser mudar o nome das colunas, usar :
#         names(dataset) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", 
#                        "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")

plot(dataset, col = as.factor(dataset$V1)) 
plot(dataset)

boxplot(data = dataset, dataset$V2~V1)
boxplot(data = dataset, dataset$V3~V1)
boxplot(data = dataset, dataset$V4~V1)
boxplot(data = dataset, dataset$V5~V1)
boxplot(data = dataset, dataset$V6~V1)
boxplot(data = dataset, dataset$V7~V1)
boxplot(data = dataset, dataset$V8~V1)
boxplot(data = dataset, dataset$V9~V1)
boxplot(data = dataset, dataset$V10~V1)
boxplot(data = dataset, dataset$V11~V1)
boxplot(data = dataset, dataset$V12~V1)
boxplot(data = dataset, dataset$V13~V1)
boxplot(data = dataset, dataset$V14~V1)
boxplot(data = dataset, dataset$V15~V1)
boxplot(data = dataset, dataset$V16~V1)


#-------------matriz correlcaoooooo----------------------------------------
library(RColorBrewer)
library(corrplot)
matriz_cor <- cor(dataset)
matriz_cor

library(corrplot)
#matriz de correlação com cores
corrplot(matriz_cor, type="full", order="hclust",
         col=brewer.pal(n=11, name="RdYlBu"))

#matriz de correlação com cores
corrplot(matriz_cor, type="full", order="hclust",
         tl.col = "black", tl.srt = 45)

# Heatmap
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = matriz_cor, col = col, symm = TRUE)


#for (i in 1:100){
#---------------------DATAS TREINO E TESTE-----------------------------------------
library(class)
set.seed(123)
indiceTreino<-sample(1:nrow(dataset),0.7*nrow(dataset))

#separando dataset de treino com 80% do dataset original
dataTreino<-dataset[indiceTreino,]
View(dataTreino)

#separando 20% restante do anterior
dataTeste<-dataset[-indiceTreino,]
View(dataTeste)

View(table(dataTreino$V1))
View(table(dataTeste$V1))





#----------------------------------Para KNN -----------------------------------
#separando coluna de classificado
treinoClasse<-dataTreino[,1]
dataTreino<-dataTreino[,-1]
dataTreino<-dataTreino[,-1]
#View(dataTreino)

testeClasse<-dataTeste[,1]
dataTeste<-dataTeste[,-1]
dataTeste<-dataTeste[,-1]
#View(dataTeste)

#aplicacao do algoritmo knn, com k=1
modelo<-knn(train = dataTreino, test = dataTeste, cl = as.factor(treinoClasse), k =1)

table(modelo)#resumo dos classificados baseado no knn
table(testeClasse)#mostra o resumo de todos os classificados baseado no dataframe

matriz = table(modelo,testeClasse)

diag(matriz)#pegando diagonal 

acerto = sum(diag(matriz))
total = sum(matriz)
acerto/total
#calcula media
#matriz2[,i]<-mean(modelo == testeClasse)
mean(modelo == testeClasse)
#}

#----------------------------------Arvore de Decisão ---------------------------
library(rpart)
library(rpart.plot)

modelo<-rpart(V1~V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16, dataTreino, method = "class", control = rpart.control(minsplit = 1))
#modelo = rpart(formula = V1 ~ ., data = dataTreino)


#Plotando arvore de decisão
rpart.plot(modelo, type = 3)

#visualizandoData Treino
#table(dataTreino[,1])

#Preprando dataClasse
testeClasse<-dataTeste[,1]
dataTeste<-dataTeste[,-1]
dataTeste<-dataTeste[,-1]
#testeClasse<- unlist(testeClasse)

pred<-predict(modelo, dataTeste, type="class")


matriz<-table(pred,testeClasse)
table(pred,testeClasse)

diag(matriz)

acerto = sum(diag(matriz))
total = sum(matriz)
acerto/total
#calcula media
mean(pred == testeClasse)
