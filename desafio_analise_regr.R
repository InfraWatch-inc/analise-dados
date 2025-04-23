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

# ALTERAÇÕES PRINCIPAIS
#  - retirei a comparação de uso cpu com uso gpu (ambas são %)
#  - complementei algumas idéias de comportamento do dado

# comparar USO CPU x USO RAM
library(ggplot2)
correlacao_cpu_ram <- cor(uso_cpu, uso_ram)
correlacao_cpu_ram

lm(uso_ram ~ uso_cpu)
ggplot(mapping = aes(uso_cpu, uso_ram)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

summary(lm(uso_ram ~ uso_cpu))
# correlação forte e positiva de ambas variáveis, porém o gráfico de regressão torna ainda mais difícil de encontrar uma relação, pois há muita dispersão dos dados e outliers bem fora do padrão
# coeficiente linear = 53 | coeficiente angular = 0.44
# e ainda por cima o resultado do R2 foi de 23.8%, assim reprovando eficiencia da análise e predição entre as duas varáveis 

# comparar USO CPU x Temp GPU
correlacao_cpu_temGpu <- cor(uso_cpu, temperatura_gpu)
correlacao_cpu_temGpu

lm(temperatura_gpu ~ uso_cpu)
ggplot(mapping = aes(temperatura_gpu, uso_cpu)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

summary(lm(temperatura_gpu ~ uso_cpu))
# correlação forte e positiva de ambas variáveis, porém o gráfico de regressão tem bastante dispersão em relação a linha, porém dá pra visualizar a tendencia de aumento 
# coeficiente linear = 55 | coeficiente angular = 0.4
# resultado do R2 foi de 61.2%, assim evidenciando que pode-se fazer uso destas variáveis para métricas preditivas

# RAM x USO GPU
correlacao_ram_usoGpu <- cor(uso_gpu, uso_ram)
correlacao_ram_usoGpu

lm(uso_ram ~ uso_gpu)
ggplot(mapping = aes(uso_ram, uso_gpu)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

summary(lm(uso_ram ~ uso_gpu))
# correlação bem forte e positiva de ambas variáveis. O gráfico de regressao teve um comportamento bem mais diferente desta vez, com dois comportamentos diferentes da dispersao, um comportamento constante de uso cpu 0 e uso da ram vairando até 60, depois do 60 a cpu começou a trablahr de forma mais ativa, com uma grande dispersao, mas ainda assim, mais proxima da linha do que os outros gráficos
# coeficiente linear = 49.6 | coeficiente angular = 0.77
# resultado do R2 foi de 76%, mostrando que há uma chance de 76% de acertar um valor para uma possível predição

# RAM x TEMP GPU
correlacao_ram_tempGpu <- cor(temperatura_gpu, uso_ram)
correlacao_ram_tempGpu

lm(temperatura_gpu ~ uso_ram)
ggplot(mapping = aes(temperatura_gpu, uso_ram)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm") +
  geom_hline(yintercept = mean(dfoo$altura))

summary(lm(temperatura_gpu ~ uso_ram))
# correlação forte e positiva de ambas variáveis. O gráfico de regressao está com os dados menos dispersos e parece estar bem mais próximo da reta, porém parece que em alguns momentos, mesmo se a temperatura subisse, a ram em muitos momentos não acompanhou
# coeficiente linear = 40.2 | coeficiente angular = 0.37
# resultado do R2 foi de 42.8%, mostrando que em uma predição, esses dados seriam incossistentes por si só

# USO GPU x TEMP GPU
correlacao_usoGpu_tempGpu <- cor(temperatura_gpu, uso_gpu)
correlacao_usoGpu_tempGpu

lm(temperatura_gpu ~ uso_gpu)
ggplot(mapping = aes(temperatura_gpu, uso_gpu)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

summary(lm(temperatura_gpu ~ uso_gpu))
# correlação forte e positiva de ambas variáveis. O gráfico de regressao parece ter um comportamento bem similar ao de cima, com valores imutaveis mesmo com alguma mudança da temperatura da gpu
# coeficiente linear = 58.8 | coeficiente angular = 0.29
# resultado do R2 foi de 32.2%, mostrando que mesmo sendo valores de um mesmo componentes, não necessariamente tem uma correlação quase perfeita e muito menos uma possível relação para predição, mesmo sendo métricas diferentes, porém não fogem muito do padrão de irem de 0 até 100