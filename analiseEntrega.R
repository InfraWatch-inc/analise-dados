#Intregantes do grupo 2
#Miguel, Ranier, Grigor, Kaio, Vitória, Pedro.

install.packages(c("ggplot2", "babynames", "dplyr", "hrbrthemes", "viridis"))

library(ggplot2)
library(babynames)
library(dplyr)
library(hrbrthemes)
library(viridis)

data_frame <- read.csv("C:/Users/grigo/OneDrive/Documentos/infrawatch/analise-dados/captura_servidor_2.csv", header = TRUE, sep = ";")
data_frame_maq1 <- read.csv("C:/Users/grigo/OneDrive/Documentos/infrawatch/analise-dados/captura_servidor_1.csv", header = TRUE, sep = ";")

data_frame_maq1 <- head(data_frame_maq1 , nrow(data_frame))
data_frame_maq1$dtHora <- data_frame$dtHora

#Maquina 2
cpu_freq_str <- data_frame$cpu1_frequencia
cpu_freq_str
cpu_freq <- as.numeric(cpu_freq_str)
cpu_freq
media_cpu_freq <- (sum(cpu_freq)) / 2000
media_cpu_freq

cpu_uso_str <- data_frame$cpu1_uso_porcentagem
cpu_uso_str
cpu_uso <- as.numeric(cpu_uso_str)
cpu_uso
media_cpu_uso <- (sum(cpu_uso)) / 2000
media_cpu_uso

ram_uso_str <- data_frame$ram1_uso_byte
ram_uso_str
ram_uso_byte <- as.numeric(ram_uso_str)
ram_uso_byte
media_uso_byte <- (sum(ram_uso_byte)) / 2000
media_uso_byte

ram_percent_str <- data_frame$ram1_uso_porcentagem
ram_percent_str
ram_percent <- as.numeric(ram_percent_str)
ram_percent
media_ram_percent <- (sum(ram_percent)) / 2000
media_ram_percent


disco_byte_str <- data_frame$disco2_uso_byte
disco_byte_str
disco_byte <- as.numeric(disco_byte_str)
disco_byte
media_disco_byte <- (sum(disco_byte)) / 2000
media_disco_byte


disco_percent_str <- data_frame$disco2_uso_porcentagem
disco_percent_str
disco_percent <- as.numeric(disco_percent_str)
disco_percent
media_disco_percent <- (sum(disco_percent)) / 2000
media_disco_percent


total_cpu_freq = sum(cpu_freq)
total_cpu_uso = sum(cpu_uso)
total_ram_percent = sum(ram_percent)
total_ram_uso_byte = sum(ram_uso_byte)
total_disco_byte = sum(disco_byte)
total_disco_percent = sum(disco_percent)


#Maquina 1

maq1_cpu_freq_str = data_frame_maq1$cpu1_frequencia
maq1_cpu_freq <- as.numeric(maq1_cpu_freq_str)

maq1_cpu_uso_str <- data_frame_maq1$cpu1_uso_porcentagem
maq1_cpu_uso <- as.numeric(maq1_cpu_uso_str)

maq1_ram_uso_str <- data_frame_maq1$ram1_uso_porcentagem
maq1_ram_uso <- as.numeric(maq1_ram_uso_str)

maq1_disco_uso_str <- data_frame_maq1$disco2_uso_porcentagem
maq1_disco_uso <- as.numeric(maq1_disco_uso_str)


#distribuição uso percent

par(mfrow = c(1,2))

hist(maq1_cpu_uso,
     main = "Comparação entre máquinas",
     ylab = "frequencia",
     xlab = "maquinas 1")

hist(cpu_uso,
     main = "Comparação entre máquinas",
     ylab = "frequencia",
     xlab = "maquinas 2")

par(mfrow = c(1,1))

#distribuição ram uso percent


par(mfrow = c(1,2))

hist(maq1_ram_uso,
     main = "Comparação entre máquinas",
     ylab = "frequencia",
     xlab = "maquinas 1")

hist(ram_percent,
     main = "Comparação entre máquinas",
     ylab = "frequencia",
     xlab = "maquinas 2")

par(mfrow = c(1,1))

#Distribuição disco uso percent

par(mfrow = c(1,2))

hist(maq1_disco_uso,
     main = "Comparação entre máquinas",
     ylab = "frequencia",
     xlab = "maquinas 1")

hist(disco_percent,
     main = "Comparação entre máquinas",
     ylab = "frequencia",
     xlab = "maquinas 2")

par(mfrow = c(1,1))


#gráfico de linhas comparando CPU
df1 <- data_frame %>% mutate(server = "Servidor 2")
df2 <- data_frame_maq1 %>% mutate(server = "Servidor 1")

df_comparado <- bind_rows(df1, df2)

ggplot(df_comparado, aes(x=dtHora, y = cpu1_uso_porcentagem, group = server, color = server)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Comparação do Uso de CPU entre Servidores") +
  theme_ipsum() +
  ylab("Uso da CPU (%)") +
  xlab("Tempo")

#gráfico de linhas comparando RAM
ggplot(df_comparado, aes(x=dtHora, y=ram1_uso_porcentagem, group = server, color = server)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Comparação do Uso de RAM entre Servidores") +
  theme_ipsum() +
  ylab("Uso da RAM (%)") +
  xlab("Tempo")

#gráfico de linhas comparando uso DISCO
ggplot(df_comparado, aes(x=dtHora, y=disco2_uso_porcentagem, group = server, color = server)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Comparação do Uso de Disco entre Servidores") +
  theme_ipsum() +
  ylab("Uso do Disco (%)") +
  xlab("Tempo")


#gráfico de linhas comparando uso GPU
ggplot(df_comparado, aes(x=dtHora, y = gpu1_uso_porcentagem, group = server, color = server)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Comparação do Uso de GPU entre Servidores") +
  theme_ipsum() +
  ylab("Uso da GPU (%)") +
  xlab("Tempo")

#gráfico de linhas comparando temperatura GPU
ggplot(df_comparado, aes(x=dtHora, y = gpu1_temperatura, group = server, color = server)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Comparação do Temperatura da GPU entre Servidores") +
  theme_ipsum() +
  ylab("Temperatura da GPU (°C)") +
  xlab("Tempo")




