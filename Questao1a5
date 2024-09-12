 library(readxl)
  
 atividade <- read_excel("C:/Users/beatr/OneDrive/Área de Trabalho/atividade estatistica/teste.xlsx")
 
 library(ggplot2)
 
 
 ggplot(atividade, aes(x = renda , y = despesas)) +
   geom_point(color = "black") +       
   geom_smooth(method = "lm", col = "purple", se = FALSE) +  
   labs(title = "Relação entre Renda e Despesa com Alimentação",
        x = "Renda",
        y = "Despesa com Alimentação") +
   theme_minimal() 

modelo <- lm(despesas ~ renda, data = atividade)
 summary(modelo)
 
 set.seed(123)
 dados_simulados <- data.frame(
   renda = rnorm(100, mean = 5000, sd = 1500),
   despesas = rnorm(100, mean = 1000, sd = 300)
 )
 
 modelo <- lm(despesas ~ renda, data = dados_simulados)
 
 summary(modelo)

 resumo_modelo <- summary(modelo)
 
 r_quadrado <- resumo_modelo$r.squared
 r_quadrado
 
 correlacao <- sqrt(r_quadrado)
 
 if (coef(modelo)[2] < 0) {
   correlacao <- -correlacao
 }
 
 correlacao
 
 print(paste("R:", correlacao))
 print(paste("R:", round(correlacao, 4)))

 residuos <- residuals(modelo)
 variancia_residuos <- sum(residuos^2) / (length(residuos) - length(coef(modelo)))
 
 residuos_padronizados <- residuos / sqrt(variancia_residuos)
 
 atividade$residuos_padronizados <- residuos_padronizados
 
 tabela_residuos <- data.frame(
   renda = atividade$renda,
   despesas = atividade$despesas,
   residuos_padronizados = atividade$residuos_padronizados
 )
 
 print(tabela_residuos)
 
 soma_residuos_padronizados <- sum(residuos_padronizados)
 print(paste("Soma dos resíduos padronizados:", round(soma_residuos_padronizados, 4)))


intercepto <- coef(modelo)[1]
coeficiente_renda <- coef(modelo)[2]

renda_nova <- 800

estimativa_despesa <- intercepto + coeficiente_renda * renda_nova


print(paste("Intercepto (β0):", round(intercepto, 4)))
print(paste("Coeficiente da Renda (β1):", round(coeficiente_renda, 4)))
print(paste("Estimativa da Despesa com Alimentação para uma Renda de R$ 800,00:", round(estimativa_despesa, 2)))

