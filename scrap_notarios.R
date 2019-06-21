rm(list=ls())
setwd("~")

library(dplyr)
library(stringr)
library(rvest)

out <- "/home/dhjs/Documentos/CD"

glob <- read_html("https://www.notariadomexicano.org.mx/directorio-de-notarios/")
edos <- html_nodes(glob, ".eg-invisiblebutton") %>% html_attr("href") %>% as.data.frame()
colnames(edos)[1] <- "liga"
edos$liga <- as.character(edos$liga)
edos$name <- sapply(strsplit(edos$liga, "/"), "[", 5)

x <- 1
base <-  data.frame()
alfa <- seq(1:32)

pb = txtProgressBar(min = 1, max = length(alfa), style = 3)

for (x in 1:length(alfa)) {
  temp_edo <- read_html(edos$liga[x])
  
  estado <- edos$name[x]
  
  nombre <- html_text(html_nodes(temp_edo, ".panel-heading h4"))
  nombre <- as.data.frame(nombre)
  colnames(nombre)[1] <- "nombre"
  nombre$nombre <- as.character(nombre$nombre)
  nombre <- nombre %>% 
    mutate(
      nombre = sapply(strsplit(nombre, "NOT."), "[", 2),
      nombre = str_trim(nombre, "left"),
      nombre = str_to_title(nombre, locale = "en")
    )
  
  contacto <- html_text(html_nodes(temp_edo, ".mpc-cubebox-side__content p"))
  contacto <- as.data.frame(contacto)
  colnames(contacto)[1] <- "datos"
  contacto$datos <- as.character(contacto$datos)
  contacto <- contacto %>% 
    filter(str_detect(datos, "javascript")) %>% 
    mutate(
      phone = sapply(strsplit(datos, "javascript"), "[", 1),
      java = sapply(strsplit(datos, "javascript"), "[", 2),
      java = str_replace(java, "mostrarMail", ""),
      java = str_replace_all(java, "[:(');]", ""),
      name = sapply(strsplit(java, "[,]"), "[", 1),
      dom = sapply(strsplit(java, "[,]"), "[", 2),
      dom = str_trim(dom, side = "left"),
      fin = sapply(strsplit(java, "[,]"), "[", 3),
      fin = str_trim(fin, side = "left"),
      mail = paste(name, dom, sep = "@"),
      mail = paste(mail, fin, sep = ".")
    ) %>% 
    select(phone, mail)
  
  tempo <- bind_cols(nombre, contacto)
  tempo$estado <- estado
  tempo <- as.data.frame(tempo)
  
  base <- bind_rows(base, tempo)
  
  rm(tempo)
  
  setTxtProgressBar(pb, x)
}

write.csv(base, paste(out, "directorio_notarios.csv", sep = "/"), row.names = F, fileEncoding = "UTF-8")
