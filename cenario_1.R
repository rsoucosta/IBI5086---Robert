##################################################
# Dados de microbiota pré e pós exposição ao antibiótico ATB1329A e placebo.
# P1 = Primeira análise populacional de microbiota
# P2 = Segunda análise populacional de microbiota (3 meses após a primeira analise)
##################################################

# Cenário 1 - Comparação Temporal - Geral

# Reprodutibilidade
set.seed(129) 

# --- Tamanho amostral ---
n <- 92 

# --- Gerar o P1 --- # Pré-tratamento
media1 <- 1000       
sd1 <- 150           
P1 <- rnorm(n, mean = media1, sd = sd1)

# --- Gerar o P2 --- # Pós-tratamento
b0 <- 100 
b1 <- 0.8        
sd2 <- 80        
P2 <- b0 + b1*P1 + rnorm(n, mean = 0, sd = sd2)

# --- Gerar o Data frame ---
dados <- data.frame(P1, P2)
head(dados)
tail(dados)
str(dados)
names(dados)
summary(dados[,1:2])

# --- Realizar a estatística descritiva ---
library(psych)
describe(dados$P1)
describe(dados$P2)

# --- Visualização da distribuição ---
par(mfrow=c(1,2))
hist(dados$P1, 
     main="Histograma (P1) - Pré-tratamento",
     xlab="P1", col="lightblue", border="black")

hist(dados$P2, 
     main="Histograma (P2) - Pós-tratamento",
     xlab="P2", col="lightpink", border="black")

par(mfrow=c(1,2))
qqnorm(dados$P1, main="QQ-plot de P1 (Pré)")
qqline(dados$P1, col="red")

qqnorm(dados$P2, main="QQ-plot de P2 (Pós)")
qqline(dados$P2, col="red")

# --- Avaliar a normalidade ---
shapiro.test(dados$P1)  # Teste de Shapiro-Wilk para normalidade P1
shapiro.test(dados$P2)  # Teste de Shapiro-Wilk para normalidade P2

# p > 0.05 sugere normalidade

# --- Realizar o teste paramétrico (t de Student pareado) ---
t.test(dados$P2, dados$P1, paired = TRUE)

# --- Ajustar o modelo de regressão ---
modelo <- lm(P2 ~ P1, data = dados)
summary(modelo)

# --- Gerar o Boxplot ---
boxplot(P1, P2,
        names = c("P1 (Pré-tratamento)", "P2 (Pós-tratamento)"),
        col = c("lightblue", "lightpink"),
        main="Comparação Pré-tratamento x Pós-tratamento")

# --- Gerar o Scatter plot ---
plot(dados$P1, dados$P2,
     xlab="Pré-tratamento",
     ylab="Pós-tratamento",
     main="Correlação Pré-tratamento x Pós-tratamento",
     pch=19, col="blue")
abline(lm(P2 ~ P1, data=dados), col="red", lwd=2)
