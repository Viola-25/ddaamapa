# Carregar as bibliotecas necessárias
library(ggplot2)

# Declarar o dataframe 'data' e suas colunas com os valores fornecidos
data <- data.frame(
  Municipio = c("Amapa", "Tartarugalzinho", "Santana", "Porto Grande", "Laranjal do Jari", 
                "Pedra Branca do Amapari", "Calcoene", "Mazagao", "Serra do Navio", "Cutias", 
                "Oiapoque", "Itaubal", "Macapa", "Vitoria do Jari", "Pracuuba"),
  Incidencia_de_Doencas_Diarreicas_Agudas = c(33.72, 28.95, 27.87, 25.25, 23.1, 
                                              22.12, 21.99, 18.73, 17.58, 16.9, 
                                              16.35, 14.72, 9.22, 7.02, 3.59),
  Acesso_a_agua_potavel = c(6.51, 87.18, 14.49, 10.38, 0.43, 
                            96.74, 1.12, 6.51, 5.63, 13.89, 
                            11.54, 11.54, 3.49, 2.31, 0.0),
  Coleta_de_esgoto = c(41.86, 65.87, 68.75, 43.75, 0.43, 
                       93.0, 50.75, 46.88, 46.88, 42.86, 
                       43.75, 37.5, 31.25, 25.0, 87.5),
  Coleta_de_lixo = c(15.19, 42.86, 24.68, 21.05, 3.92, 
                     37.5, 36.21, 29.76, 31.25, 37.5, 
                     34.38, 29.76, 12.5, 17.39, 66.67)
)

# Scatter plots para visualizar correlações
plot1 <- ggplot(data, aes(x = Acesso_a_agua_potavel, y = Incidencia_de_Doencas_Diarreicas_Agudas)) +
  geom_point() +
  geom_smooth(method = 'lm', col = 'blue') +
  ggtitle('Incidencia de Doencas Diarreicas Agudas vs Acesso a agua potavel')

plot2 <- ggplot(data, aes(x = Coleta_de_esgoto, y = Incidencia_de_Doencas_Diarreicas_Agudas)) +
  geom_point() +
  geom_smooth(method = 'lm', col = 'blue') +
  ggtitle('Incidencia de Doencas Diarreicas Agudas vs Coleta de esgoto')

plot3 <- ggplot(data, aes(x = Coleta_de_lixo, y = Incidencia_de_Doencas_Diarreicas_Agudas)) +
  geom_point() +
  geom_smooth(method = 'lm', col = 'blue') +
  ggtitle('Incidencia de Doencas Diarreicas Agudas vs Coleta de lixo')

# Imprimir plots
print(plot1)
print(plot2)
print(plot3)

# Verificar correlação entre as variáveis
correlation1 <- cor(data$Acesso_a_agua_potavel, data$Incidencia_de_Doencas_Diarreicas_Agudas)
correlation2 <- cor(data$Coleta_de_esgoto, data$Incidencia_de_Doencas_Diarreicas_Agudas)
correlation3 <- cor(data$Coleta_de_lixo, data$Incidencia_de_Doencas_Diarreicas_Agudas)

cat("Correlação entre Acesso a água potável e Incidência de Doenças Diarreicas Agudas:", correlation1, "\n")
cat("Correlação entre Coleta de esgoto e Incidência de Doenças Diarreicas Agudas:", correlation2, "\n")
cat("Correlação entre Coleta de lixo e Incidência de Doenças Diarreicas Agudas:", correlation3, "\n")

# Realizo o teste t
t_test1 <- t.test(data$Acesso_a_agua_potavel, data$Incidencia_de_Doencas_Diarreicas_Agudas)
t_test2 <- t.test(data$Coleta_de_esgoto, data$Incidencia_de_Doencas_Diarreicas_Agudas)
t_test3 <- t.test(data$Coleta_de_lixo, data$Incidencia_de_Doencas_Diarreicas_Agudas)

# Imprimo resultados dos testes t
cat("\nTeste t entre Acesso a água potável e Incidência de Doenças Diarreicas Agudas:\n")
print(t_test1)
cat("\nTeste t entre Coleta de esgoto e Incidência de Doenças Diarreicas Agudas:\n")
print(t_test2)
cat("\nTeste t entre Coleta de lixo e Incidência de Doenças Diarreicas Agudas:\n")
print(t_test3)

# Verifica se e normal
s_test1 <- shapiro.test(data$Acesso_a_agua_potavel)
s_test2 <- shapiro.test(data$Coleta_de_esgoto)
s_test3 <- shapiro.test(data$Coleta_de_lixo)
s_test4 <- shapiro.test(data$Incidencia_de_Doencas_Diarreicas_Agudas)

# Imprimir resultados dos testes para verificar se esta em distribuicao normal
cat("\nTeste S entre Acesso a água potável:\n")
print(s_test1)
cat("\nTeste S entre Coleta de esgoto:\n")
print(s_test2)
cat("\nTeste S entre Coleta de lixo:\n")
print(s_test3)
cat("\nTeste S entre Incidencia:\n")
print(s_test4)