# Visualização de Dados no {ggplot2}
## Carregar o banco de dados geomorfologia
geomorfologia <- read.table("data/geomorfologia.txt",h=TRUE)

## Filtrar os dados da superfície I
filtro <- geomorfologia$SUP == "I"
geo_aux <- geomorfologia[filtro,]

## PIPE (magrittr) %>% - opera data.frames e resuta em data.frames
library(tidyverse)
geomorfologia %>% # CONTROL + SHIFT + M 
  filter(SUP == "I")

# retirando uma coluna com o PIPE
geomorfologia$SUP
# ou
geomorfologia %>% 
  pull(SUP)

# Pegar da superfície I um somente areia grossa e argila
geomorfologia %>% 
  filter(SUP == "I") %>% # 1º verbo
  select(AG, ARGILA) # 2º verbo

# Pegar da superfície I um somente areia grossa e argila
geomorfologia %>% 
  filter(SUP == "I") %>% 
  select(AG, ARGILA) %>% 
  arrange(desc(ARGILA)) # decrescente 3º verbo

# Amédia para ARGILA por tipo de solo
geomorfologia %>% 
  group_by(Solo) %>% # 4º verbo
  summarise(Media_ARGILA = mean(ARGILA,na.rm=TRUE)) # 5º verbo

## Criando gráficos
meu_plot <- geomorfologia %>% 
  ggplot(aes(x=X, y=ARGILA)) +
  geom_point(color = "red",fill="green",size=4,shape=21) +
  geom_line(color = "blue", linetype=2, lwd = .5)

# Mudando os temas
meu_plot + theme_dark()
meu_plot + theme_bw()
meu_plot + theme_classic()


# Gráfico de Barras
geomorfologia %>% 
  ggplot(aes(x=X, y=ARGILA)) + 
  geom_col(color = "black", fill="gray") +
  theme_bw() +
#  coord_flip()  # para rotacionar o gráfico
  facet_wrap(~SUP, scales = "free", nrow = 3)


# Gráfico de Barras
geomorfologia %>% 
  ggplot(aes(x=X, y=ARGILA, fill=Solo)) + 
  geom_col(color="black") +
  theme_bw() +
  facet_wrap(~SUP, scales = "free", nrow = 3)

geomorfologia %>% 
  group_by(SUP, Solo) %>% 
  summarise(media_argila = mean(ARGILA, na.rm=TRUE)) %>% 
  ggplot(aes(x=Solo, y=media_argila,fill=Solo)) +
  geom_col(color="black") +
  facet_wrap(~SUP, scales="free", nrow = 1) +
  labs(x = "Tipo de Solo", y = "Teor de Argila",
       title = "Média por superfície", fill = "Legenda")+
  theme_bw()
  
# Gráfico de Linhas e Barras no mesmo Layout
# Gráfico do Teor de Argila ao longo do perfil (linhas)
geomorfologia %>% 
  ggplot(aes(x=X)) +
  geom_col(aes(y=AG), color="black", fill="lightgray") +
  geom_line(aes(y=ARGILA), lwd=.5, color="red") +
  theme_classic() +
#  facet_wrap(~SUP, scales="free")
  annotate("text", x = 500, y = 10, label = "SUP I") + # texto
  geom_vline(xintercept = c(1000,2000), 
                            color=c("blue","darkgreen"), 
             linetype=2)+
  scale_y_continuous(name="Areia",
                     sec.axis = sec_axis(~.*1,name="Argila"))



# Gráfico do Teor de Argila ao longo do perfil (linhas)
geomorfologia %>% 
  ggplot(aes(x=X)) +
  geom_col(aes(y=AG, fill=SUP)) +
  geom_line(aes(y=ARGILA), lwd=.5, color="red") +
  theme_classic() +
  #  facet_wrap(~SUP, scales="free")
  annotate("text", x = 200, y = 10, label = "SUP I") + # texto
  geom_vline(xintercept = c(400,2000), 
             color=c("blue","darkgreen"), 
             linetype=2)+
  scale_y_continuous(name="Areia",
                     sec.axis = sec_axis(~.*1,name="Argila"))


## Controle de thema
meu_plot + 
  labs(title = "Gráfico de Linhas e Pontos") +
  theme(
    plot.title = element_text(hjust = 0.5, color="blue"),
    panel.background = element_rect(color="black",
                                    fill = "gray86"),
    axis.text = element_text(color = "red"),
    panel.grid.major = element_line(color="black",linetype=2),
    panel.grid.minor = element_line(color = "darkgray"),
    axis.text.x = element_text(color = "blue"),
    axis.title.x = element_text(color = "blue"),
    axis.title.y = element_text(color = "red")
  )+
  coord_cartesian(xlim=c(0, 3000), ylim = c(0,30)) +
  scale_x_continuous(breaks = seq(0,3000,500))

## Vamos fazer alguns ajustes
## gráfico de dispersão argila em função de X, 
library(ggpubr)
geomorfologia %>% 
  ggplot(aes(x=X, y=ARGILA)) +
  geom_point() +
  geom_smooth( method="lm", color="red") +
  stat_regline_equation(aes(
    label =  paste(..eq.label.., 
                   ..rr.label.., 
                   sep = "*plain(\",\")~~")))


formula <- y ~ poly(x,3,raw=TRUE) #definindo a fórmula polinomial
geomorfologia %>% 
  ggplot(aes(x=X, y=ARGILA)) +
  geom_point() +
  geom_smooth(color="red", method="lm", formula=formula) +
  stat_regline_equation(aes(
    label =  paste(..eq.label.., 
                   ..rr.label.., 
                   sep = "*plain(\",\")~~")),
    formula=formula) # adicionar a mesma fórmula


formula <- y ~ poly(x,3,raw=TRUE) #definindo a fórmula polinomial
geomorfologia %>% 
  ggplot(aes(x=X, y=ARGILA, color = SUP)) +
  geom_point() +
  geom_smooth(method="lm", formula=formula) +
  stat_regline_equation(aes(
    label =  paste(..eq.label.., 
                   ..rr.label.., ..AIC.label..,
                   sep = "*plain(\",\")~~")),
    formula=formula) +# adicionar a mesma fórmula
  facet_wrap(~SUP, scales="free", nrow=3)


# Gráficos de Barras, um dos eixos categóricos
geomorfologia %>% 
  ggplot(aes(x=SUP, fill= SUP)) +
  geom_bar()


bar_plot <- geomorfologia %>% 
  ggplot(aes(x=SUP, fill= Solo)) +
  geom_bar(color="black", position = "dodge")

# Gráficos na mesma saída
figure <- ggarrange(meu_plot, bar_plot,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure

## ggplot extensions gallery
# https://exts.ggplot2.tidyverse.org/gallery/
library(tidyverse)

#######
library(ggridges)
library(hrbrthemes)
# densidades dispostas no eixo Y ou X, ao invés do facet
dense_plot <- geomorfologia %>% 
  mutate(
    Solo = fct_reorder(Solo, ARGILA, .fun = median, na.rm=TRUE)
  ) %>% 
  ggplot(aes(x=ARGILA, y=Solo, fill=Solo)) +
  geom_density_ridges(color="transparent", alpha = 0.6) +
  scale_fill_viridis_d(option = "magma") + 
  theme_minimal() +
  theme(
    legend.position = "none"
  ) + theme_ft_rc() 


### 
# Definindo tema para todos os gráficos do ggplot
theme_set(theme_minimal())


# patchwork
dense_plot <- dense_plot + theme(legend.position = "none")
bar_plot <- bar_plot + theme(legend.position = "none")
library(patchwork)
meu_plot + bar_plot
(meu_plot / bar_plot) | dense_plot

((meu_plot / bar_plot)  | dense_plot & theme_classic())  +
  plot_annotation(
    title = "Área para o Título",
    subtitle = "descrição do gráfico, a frase pode ser mais longa",
    caption = "Fonte do Gráfico")

# patchwork
library(patchwork)
densi_plot <- geomorfologia %>% 
  mutate(
    Solo = fct_reorder(Solo, ARGILA, .fun = median, na.rm=TRUE)
  ) %>% 
  ggplot(aes(x=ARGILA, y=Solo, fill=Solo)) +
  geom_density_ridges(color="transparent", alpha = 0.6) +
  scale_fill_viridis_d(option = "magma") + 
  theme_minimal() +
  theme(
    legend.position = "none"
  ) 
dense_plot <- dense_plot + theme(legend.position = "none")
bar_plot <- bar_plot + theme(legend.position = "none")

# Usando a gramática do patchwork para combinar os gráficos
# | - barra vertical, um ao lado do outro
meu_plot | bar_plot | densi_plot

# + usado para empilhar em gradeado
meu_plot + bar_plot + densi_plot + meu_plot + bar_plot + densi_plot

# / para empilhar
meu_plot / bar_plot / densi_plot

# Agora podemos mesclar
(meu_plot / bar_plot) | densi_plot

# podemos inserir anotações
((meu_plot / bar_plot) | densi_plot) + 
  plot_annotation(
    title = "Área para o Título",
    subtitle = "descrição do gráfico, a frase pode ser mais longa",
    caption = "Fonte do Gráfico")

# Utilizando o gghighlight
library(gghighlight)
geomorfologia %>% 
  ggplot(aes(x=X, y=ARGILA)) +
  geom_point(color="orange")+
  gghighlight(ARGILA >= 22, label_key = SUP)


