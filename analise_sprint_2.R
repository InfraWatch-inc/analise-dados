captura_servidor_2 <- read.csv("captura_servidor_2.csv", header = TRUE, sep = ";")
captura_servidor_2
# Tratando dados 
captura_servidor_2$cpu1_uso_porcentagem <- as.numeric(captura_servidor_2$cpu1_uso_porcentagem)
# alterar uso da ram em % para usar outra medida
captura_servidor_2$ram1_uso_byte <- as.numeric(captura_servidor_2$ram1_uso_byte)
captura_servidor_2$gpu1_temperatura <- as.numeric(captura_servidor_2$gpu1_temperatura)
captura_servidor_2$gpu1_uso_porcentagem <- as.numeric(captura_servidor_2$gpu1_uso_porcentagem)

# alterar de byte para gb
captura_servidor_2$ram1_uso_gb <- round(captura_servidor_2$ram1_uso_byte / 1024 ^ 3, 2)

uso_cpu <- captura_servidor_2$cpu1_uso_porcentagem
uso_ram <- captura_servidor_2$ram1_uso_gb
temperatura_gpu <- captura_servidor_2$gpu1_temperatura
uso_gpu <- captura_servidor_2$gpu1_uso_porcentagem

install.packages("cowplot")
install.packages("tidyverse")
install.packages("patchwork")

library(patchwork)
library(ggplot2)
library(patchwork)

plot_ram_gpu <- ggplot(mapping = aes(uso_ram, uso_gpu)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")+
  xlab("Uso da RAM (GB)") +
  ylab("Uso da GPU (%)") +
  ggtitle("Regressão Uso RAM x Uso GPU")

plot_cpu_ram <- ggplot(mapping = aes(uso_cpu, uso_ram)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm") +
  ylab("Uso da RAM (GB)") +
  xlab("Uso da CPU (%)") +
  ggtitle("Regressão Uso CPU x Uso RAM")

plot_cpu_ram + plot_ram_gpu
# plot_grid(plot_ram_gpu, plot_cpu_ram, labels = "AUTO")

# comparar USO CPU x USO RAM
correlacao_cpu_ram <- cor(uso_cpu, uso_ram)
correlacao_cpu_ram

lm(uso_ram ~ uso_cpu)

summary(lm(uso_ram ~ uso_cpu))
# correlação forte e positiva de ambas variáveis, porém o gráfico de regressão torna ainda mais difícil de encontrar uma relação, pois há muita dispersão dos dados e outliers bem fora do padrão
# coeficiente linear = 53 | coeficiente angular = 0.44
# e ainda por cima o resultado do R2 foi de 23.8%, assim reprovando eficiencia da análise e predição entre as duas varáveis 

# USO RAM x USO GPU
correlacao_ram_usoGpu <- cor(uso_gpu, uso_ram)
correlacao_ram_usoGpu

lm(uso_ram ~ uso_gpu)

summary(lm(uso_ram ~ uso_gpu))
# correlação bem forte e positiva de ambas variáveis. O gráfico de regressao teve um comportamento bem mais diferente desta vez, com dois comportamentos diferentes da dispersao, um comportamento constante de uso cpu 0 e uso da ram vairando até 60, depois do 60 a cpu começou a trablahr de forma mais ativa, com uma grande dispersao, mas ainda assim, mais proxima da linha do que os outros gráficos
# coeficiente linear = 49.6 | coeficiente angular = 0.77
# resultado do R2 foi de 76%, mostrando que há uma chance de 76% de acertar um valor para uma possível predição



