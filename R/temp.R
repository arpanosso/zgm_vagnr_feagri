
# Validação de dados  
## Análise de estatística descritiva  

```{r}
# criando função para estatística descritiva
estat_desc <- function(x){
  x<-na.omit(x)
  media <- mean(x)
  mediana <- median(x)
  dp <- sd(x)
  cv <- 100*dp/media
  MIN <- min(x)
  MAX <- max(x)
  q1 <- quantile(x,0.25)
  q3 <- quantile(x,0.75)
  assi <- agricolae::skewness(x)
  curt <- agricolae::kurtosis(x)
  c(Mínimo = MIN, Q1=q1, Média=media, Med=mediana, Q3=q3, Máximo=MAX,
    DesvPad = dp, CV = cv,Assimetria=assi,Curtose=curt)
}
apply(dados[7:(length(dados)-1)], 2, estat_desc)
```

# Gráficos de histograma

```{r}
dados %>% 
  filter(!is.na(UMID)) %>% 
  group_by(season) %>% 
  ggplot(aes(x=UMID)) +
  geom_histogram(bins=30,color="black",fill="gray")+
  facet_wrap(~season + Profundidade, nrow = 3, scales = "free")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))
levels(as.factor(dados$Tratamento))
```


# Dados de Macrofauna

```{r}
macrofauna <- read.table("https://raw.githubusercontent.com/arpanosso/zgm_vagnr_feagri/main/dados/Macrofauna_edafica_dados_gerais_analises.txt", h=TRUE,
                         sep="\t")
macrofauna <- macrofauna %>%  mutate(Estacao = ifelse(Estacao=="Verão","Verao",Estacao),
                                     Tratamento = ifelse(Tratamento=="Pastagem","Pasto",Tratamento),
                                     Estacao = case_when(
                                       Coleta == 1 ~ "Inverno",
                                       Coleta == 2 ~ "Outono",
                                       Coleta == 3 ~ "Primavera",
                                       Coleta == 4 ~ "Verao"
                                     ),
                                     season = ifelse(Coleta <=2, "Dry","Rainy") )
glimpse(macrofauna)
```

# Gráficos de contagem
```{r,message=FALSE}
macrofauna %>% 
  group_by(season,Tratamento,Ordem) %>% 
  summarise(contagem =n()) %>% 
  mutate(perc = contagem/sum(contagem)) %>% 
  ggplot(aes(x=Tratamento, y=perc, fill=Ordem))+
  geom_col(position = "dodge") + 
  facet_wrap(~season, scale="free",nrow=1) +
  theme_minimal()+
  coord_flip()
```

```{r}
macrofauna %>% 
  group_by(season,Ordem) %>% 
  summarise(count =n()) %>% 
  ggplot(aes(x=season, y=count,fill=Ordem))+
  geom_col(position = "dodge") + 
  theme_minimal()
```


## Análise para todos os dados


### Análise de correlação  

```{r}
dados  %>%   
  ungroup() %>% 
  select(-Coleta, -Estacao, - Tratamento, - Ponto, -Pontos, - Profundidade, - season) %>% 
  cor() %>% 
  corrplot::corrplot(method="ellipse",type="upper",mar=c(0,0,1,0))
```

## Análise de agrupamento hierárquico  
```{r}
### Padronizando as variáveis 
rotulos <- dados  %>%   
  pull(Tratamento)

da_pad<-dados  %>%   
  ungroup() %>% 
  select(-Coleta, -Estacao, - Tratamento, - Ponto, -Pontos, - Profundidade, - season) %>% 
  decostand(method = "standardize",na.rm=TRUE)
da_pad_euc<-vegdist(da_pad,"euclidean")
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")

# Plotando do dendrograma
plot(da_pad_euc_ward, ylab="Distância Euclidiana",
     xlab="Acessos", hang=-1, col="blue",
     las=1, cex=.6, labels = rotulos,
     lwd=1.5);box()

# Plotando o Heatmap
dend<-as.dendrogram(da_pad_euc_ward)
heatmap(as.matrix(da_pad_euc),Rowv=dend,
        symm = TRUE,margin=c(3,3),labRow =rotulos,
        labCol=rotulos)
```

### Análise de Componentes principais (PCA)

```{r}
rotulos <- dados  %>%   pull(Tratamento)
da<-dados  %>%   
  ungroup() %>% 
  select(-Coleta, -Estacao, - Tratamento, - Ponto, -Pontos, - Profundidade, - season)
pca <-  prcomp(da,scale.=T)
print("Autovalores ---")
eig<-pca$sdev^2
print(round(eig,6))
print("% da variância explicada ---")
ve<-eig/sum(eig)
print(round(ve,6)*100)
print("% da variância acumulada ---")
print(cumsum(ve)*100)
mcor<-cor(da,pca$x) 
mcor %>% 
  corrplot::corrplot(mar=c(0,0,1,0))
screeplot(pca);abline(h=1,lty=2)
```


```{r}
pc1V<-cor(da,pca$x)[,1]/sd(cor(da,pca$x)[,1])
pc2V<-cor(da,pca$x)[,2]/sd(cor(da,pca$x)[,2])
pc1c<-pca$x[,1]/sd(pca$x[,1])
pc2c<-pca$x[,2]/sd(pca$x[,2])
Agrupamento<-as.factor(rotulos)
bip<-data.frame(pc1c,pc2c,Agrupamento)
texto <- data.frame(
  x = pc1V,
  y = pc2V,
  label = names(da))
```


```{r}
p<-bip %>% ggplot(aes(x=pc1c,y=pc2c,color=Agrupamento))+
  geom_point(aes(shape =Agrupamento, color = Agrupamento), size = 2) + 
  theme_minimal() +
  geom_vline(aes(xintercept=0),
             color="black", size=1) +
  geom_hline(aes(yintercept=0),
             color="black", size=1) +
  annotate(geom="segment",
           x=rep(0,length(da)),
           xend=texto$x,
           y=rep(0,length(da)),
           yend=texto$y,color="black",lwd=.5) +
  geom_label(data=texto,aes(x=x,y=y,label=label),
             color="black",angle=0,fontface="bold",size=4,fill="white") +
  labs(x=paste("CP1 (",round(100*ve[1],2),"%)",sep=""),
       y=paste("CP2 (",round(100*ve[2],2),"%)",sep="")) +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5))
print(p)
```


```{r}
print( "Tabela das correlações com as PCs---")
ck<-sum(pca$sdev^2>=1)
tabelapca<-vector()
for( l in 1:ck) tabelapca<-cbind(tabelapca,mcor[,l]) 
colnames(tabelapca)<-paste(rep(c("PC"),ck),1:ck,sep="")
pcat<-round(tabelapca,3)
tabelapca<-tabelapca[order(abs(tabelapca[,1])),]
print(tabelapca)
cat("\n")
```

## Análise por Estação e por Profundidade 

### Análise de correlação  

```{r}
# Definir a estação e a profundidade
Est = "Rainy"
Prof = "0.00-0.10"
dados  %>%   
  filter(season==Est, Profundidade == Prof) %>% 
  ungroup() %>% 
  select(-Coleta, -Estacao, - Tratamento, - Ponto, -Pontos, - Profundidade, - season, - Tar, - RH, - WBT) %>% 
  cor() %>% 
  corrplot::corrplot(method="ellipse",type="upper",mar=c(0,0,1,0),
                     title = paste0(Est,"; ", Prof,sep=""))
```

## Análise de agrupamento hierárquico  
```{r}
rotulos <- dados  %>%   
  filter(season==Est, Profundidade == Prof) %>% 
  pull(Tratamento)

da_pad<-dados  %>%   
  filter(season==Est, Profundidade == Prof) %>% 
  ungroup() %>% 
  select(-Coleta, -Estacao, - Tratamento, - Ponto, -Pontos, - Profundidade, - season, - Tar, - RH, - WBT) %>% 
  decostand(method = "standardize",na.rm=TRUE)
da_pad_euc<-vegdist(da_pad,"euclidean")
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
```


```{r}
# Plotando do dendrograma
plot(da_pad_euc_ward,ylab="Distância Euclidiana",xlab="Acessos",main=paste0(Est,"; ", Prof,sep=""),
     hang=-1,col="blue",las=1,cex=.6,labels = rotulos,lwd=1.5);box()
```


```{r}
# Plotando o Heatmap
dend<-as.dendrogram(da_pad_euc_ward)
heatmap(as.matrix(da_pad_euc),
        Rowv=dend,
        symm = TRUE,main=paste0(Est,"; ", Prof,sep=""),
        margin=c(3,3),
        labRow =rotulos,
        labCol=rotulos)
```

### Análise de Componentes principais (PCA)

```{r}
rotulos <- dados  %>%   
  filter(season==Est, Profundidade == Prof) %>% 
  pull(Tratamento)

da<-dados  %>%   
  filter(season==Est, Profundidade == Prof) %>% 
  ungroup() %>% 
  select(-Coleta, -Estacao, - Tratamento, - Ponto, -Pontos, - Profundidade, - season,  - Tar, - RH, - WBT)

pca <-  prcomp(da,scale.=T)
print("Autovalores ---")
eig<-pca$sdev^2
print(round(eig,6))
cat("\n")
print("% da variância explicada ---")
ve<-eig/sum(eig)
print(round(ve,6)*100)
cat("\n")
print("% da variância acumulada ---")
print(cumsum(ve)*100)
```


```{r}
mcor<-cor(da,pca$x) 
mcor %>% 
  corrplot::corrplot(mar=c(0,0,1,0))
screeplot(pca);abline(h=1,lty=2)
```


```{r}
# Construção do biplot usando as funções básicas
pc1V<-cor(da,pca$x)[,1]/sd(cor(da,pca$x)[,1])
pc2V<-cor(da,pca$x)[,2]/sd(cor(da,pca$x)[,2])
pc1c<-pca$x[,1]/sd(pca$x[,1])
pc2c<-pca$x[,2]/sd(pca$x[,2])
Agrupamento<-as.factor(rotulos)
bip<-data.frame(pc1c,pc2c,Agrupamento)
texto <- data.frame(x = pc1V,y = pc2V,label = names(da))

p<-bip %>% ggplot(aes(x=pc1c,y=pc2c,color=Agrupamento))+
  geom_point(aes(shape =Agrupamento, color = Agrupamento), size = 2)+ 
  theme_minimal()+
  geom_vline(aes(xintercept=0),
             color="black", size=1)+
  geom_hline(aes(yintercept=0),
             color="black", size=1)+
  annotate(geom="segment",
           x=rep(0,length(da)),
           xend=texto$x,
           y=rep(0,length(da)),
           yend=texto$y,color="black",lwd=.5)+
  geom_label(data=texto,aes(x=x,y=y,label=label),
             color="black",angle=0,fontface="bold",size=4,fill="white")+
  labs(x=paste("CP1 (",round(100*ve[1],2),"%)",sep=""),
       y=paste("CP2 (",round(100*ve[2],2),"%)",sep=""),
       title =  paste0(Est,"; ", Prof,sep="") )+
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5))
print(p)
```


```{r}
print( "Tabela das correlações com as PCs---")
ck<-sum(pca$sdev^2>=1)
tabelapca<-vector()
for( l in 1:ck) tabelapca<-cbind(tabelapca,mcor[,l]) 
colnames(tabelapca)<-paste(rep(c("PC"),ck),1:ck,sep="")
pcat<-round(tabelapca,3)
tabelapca<-tabelapca[order(abs(tabelapca[,1])),]
print(tabelapca)
cat("\n")
```
