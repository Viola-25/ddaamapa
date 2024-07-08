# Carregar as bibliotecas necessárias
library(ggplot2)

# Declarar o dataframe 'data' e suas colunas com os valores fornecidos
data <- data.frame(
  Municipio = c("Amapa", "Ferreira Gomes", "Tartarugalzinho", "Santana", "Porto Grande", "Laranjal do Jari", 
                "Pedra Branca do Amapari", "Calcoene", "Mazagao", "Serra do Navio", "Cutias", 
                "Oiapoque", "Itaubal", "Macapa", "Vitoria do Jari", "Pracuuba"),
  Incidencia_de_Doencas_Diarreicas_Agudas = c(33.72, 29.56, 28.95, 27.87, 25.25, 23.1, 
                                              22.12, 21.99, 18.73, 17.58, 16.9, 
                                              16.35, 14.72, 9.22, 7.02, 3.59),
  populacao_sem_agua = c(76.23, 71.45, 87.18, 58.16, 97, 70.56, 
                         96.74, 87.64, 84.91, 66.52, 55.70, 
                         93.25, 72.29, 63.40, 55.34, 79.22),
  populacao_sem_esgoto = c(95.50, NA, NA, 98.87, NA, 
                           NA, NA, NA, 98.57, 67.28, 
                           NA, 99, NA, 89.45, NA, NA),
  populacao_sem_lixo = c(0, 14.12, 34.13, 7.02, 35.70, 
                         0, 30.00, 38.74, 51.43, 28.28, 
                         10.46, 32.46, 58.88, 0, 17.93, NA)
)

# Scatter plots para visualizar correlações
plot1 <- ggplot(data, aes(x = populacao_sem_agua, y = Incidencia_de_Doencas_Diarreicas_Agudas)) +
  geom_point() +
  geom_smooth(method = 'lm', col = 'blue') +
  ggtitle('Incidencia de Doencas Diarreicas Agudas vs Pop sem Água')

plot2 <- ggplot(data, aes(x = populacao_sem_esgoto, y = Incidencia_de_Doencas_Diarreicas_Agudas)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = 'lm', col = 'blue', na.rm = TRUE) +
  ggtitle('Incidencia de Doencas Diarreicas Agudas vs Pop sem Esgoto')

plot3 <- ggplot(data, aes(x = populacao_sem_lixo, y = Incidencia_de_Doencas_Diarreicas_Agudas)) +
  geom_point() +
  geom_smooth(method = 'lm', col = 'blue') +
  ggtitle('Incidencia de Doencas Diarreicas Agudas vs Pop sem Lixo')

# Imprimir plots
print(plot1)
print(plot2)
print(plot3)

# Verificar correlação entre as variáveis
correlation1 <- cor(data$populacao_sem_agua, data$Incidencia_de_Doencas_Diarreicas_Agudas)
correlation2 <- cor(data$populacao_sem_esgoto, data$Incidencia_de_Doencas_Diarreicas_Agudas, use = "complete.obs")
correlation3 <- cor(data$populacao_sem_lixo, data$Incidencia_de_Doencas_Diarreicas_Agudas)

cat("Correlação entre Pop sem Água e Incidência de Doenças Diarreicas Agudas:", correlation1, "\n")
cat("Correlação entre Pop sem Esgoto e Incidência de Doenças Diarreicas Agudas:", correlation2, "\n")
cat("Correlação entre Pop sem Lixo e Incidência de Doenças Diarreicas Agudas:", correlation3, "\n")

# Realizo o teste t
t_test1 <- t.test(data$populacao_sem_agua, data$Incidencia_de_Doencas_Diarreicas_Agudas)
t_test2 <- t.test(data$populacao_sem_esgoto, data$Incidencia_de_Doencas_Diarreicas_Agudas, na.rm = TRUE)
t_test3 <- t.test(data$populacao_sem_lixo, data$Incidencia_de_Doencas_Diarreicas_Agudas)

# Imprimo resultados dos testes t
cat("\nTeste t entre Pop sem Água e Incidência de Doenças Diarreicas Agudas:\n")
print(t_test1)
cat("\nTeste t entre Pop sem Esgoto e Incidência de Doenças Diarreicas Agudas:\n")
print(t_test2)
cat("\nTeste t entre Pop sem Lixo e Incidência de Doenças Diarreicas Agudas:\n")
print(t_test3)

# Verifica se e normal
s_test1 <- shapiro.test(data$populacao_sem_agua)
s_test2 <- shapiro.test(data$populacao_sem_esgoto[!is.na(data$populacao_sem_esgoto)])
s_test3 <- shapiro.test(data$populacao_sem_lixo)
s_test4 <- shapiro.test(data$Incidencia_de_Doencas_Diarreicas_Agudas)

# Imprimir resultados dos testes para verificar se esta em distribuicao normal
cat("\nTeste S entre Pop sem Água:\n")
print(s_test1)
cat("\nTeste S entre Pop sem Esgoto:\n")
print(s_test2)
cat("\nTeste S entre Pop sem Lixo:\n")
print(s_test3)
cat("\nTeste S entre Incidencia:\n")
print(s_test4)
