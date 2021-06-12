library(readxl)
library(dplyr)
library(esquisse)
library(ggplot2)

tidy.names <- c("action", "start", "category", "srcip", "srcport", "dstip", "dstport")

#carga de datasets
df.fw <- read_excel(path = "C://Cursos/Maestría/Data Science Aplicado a la Ciberseguridad/grupo1/data-raw/muestra logs firewall.xlsx")
df.ports <- read.csv(file = "C://Cursos/Maestría/Data Science Aplicado a la Ciberseguridad/grupo1/data-raw/service-names-port-numbers.csv")
df.ips <- read_excel(path = "C://Cursos/Maestría/Data Science Aplicado a la Ciberseguridad/grupo1/data-raw/ipsmaliciosas.xlsx")

#limpieza de datos de logs de fw
df.fw[df.fw=="N/A"] <- NA
df.fw <- na.omit(df.fw)

#se modifican nombres de columnas en el data frame
names(df.fw) <- tidy.names

df.ports <- df.ports %>% filter(Transport.Protocol == "tcp")
#limpieza de datos de servicios 
#se identifican valores duplicados
duplicados<-as.character(duplicated(df.ports$Port.Number))
df.ports$duplicado=duplicados
#se elimina columna de df.ports porque todos los valores eran NA
df.ports <- df.ports [,-10]
#se eliminan valores duplicados
df.ports[df.ports=="TRUE"] <- NA
df.ports <- na.omit(df.ports)

#concatenación con servicio
df <- df.fw %>%
  left_join(df.ports, by = c("dstport" = "Port.Number")) %>%
  select(tidy.names, Service.Name)
names(df) <- c(tidy.names, "dstsrv")

#cambio de tipo de variables
df$action <- as.factor(df$action)
df$category <- as.factor(df$category)

#limpieza data frame concatenado con servicio
df <- na.omit(df)

#concatenación con lista de ips maliciosas
dffinal <- df %>%
  left_join(df.ips, by = c("dstip" = "ipsmaliciosas")) %>%
  select(tidy.names, dstsrv, ipestado)
names(dffinal) <- c(tidy.names, "dstsrv", "ipsmaliciosas")

#limpieza data frame concatenado con ips maliciosas
dffinal <- na.omit(dffinal)

dffinal$action <- as.factor(dffinal$action)
dffinal$category <- as.factor(dffinal$category)

#eliminar columna source port
dffinal <- dffinal [,-5]

#esquisse::esquisser(dffinal) -> usado para elegir la mejor forma para presentar los datos.

#gráfico que relaciona IP destino, IP de origen y servicio utilizado
ggplot(dffinal) +
  aes(x = dstip, y = srcip) +
  geom_tile(size = 1.2) +
  theme_minimal() +
  facet_wrap(vars(dstsrv))