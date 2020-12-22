#---- Información del documento ----
# Titulo: Descriptivos PACES para preregistro
# Autor: Martín Venegas M
# Fecha: 11/11/20
# Contexto: Fondecyt Ciudadanía

# Este documento alberga descriptivos univariados que permitan conocer la viabilidad (en terminos de varianza) de las variables a utilizar para el articulo pensado.

#---- Cargar paquetes ----
pacman::p_load(tidyverse, ggplot2, sjlabelled, summarytools, sjmisc, openxlsx, writexl, haven)

#---- Cargar base de datos ----
ba <- haven::read_dta(file = "Base Apoderados v2.dta")
bd <- haven::read_dta(file = "Base Docentes v2.dta")
be <- haven::read_dta(file = "Base Estudiantes v2.dta")

#---- Breve procesamiento de los datos ----

# Base de datos de los apoderados
ba_proc <- ba %>% select(P9A:P16)

# Base de datos de los estudiantes
be_proc <- be %>% select(P24A:P29)

# ---- Rec cuestionario apoderados----
ba_proc$P9A[ba_proc$P9A == 5] <- NA
ba_proc$P9A[ba_proc$P9A == 9] <- NA
ba_proc$P9A <- factor(ba_proc$P9A, levels = c(1,2,3,4), labels = c("Nada importante", "Algo importante", "Importante", "Muy importante"))

ba_proc$P9B[ba_proc$P9B == 5] <- NA
ba_proc$P9B[ba_proc$P9B == 9] <- NA
ba_proc$P9B <- factor(ba_proc$P9B, levels = c(1,2,3,4), labels = c("Nada importante", "Algo importante", "Importante", "Muy importante"))

ba_proc$P9C[ba_proc$P9C == 5] <- NA
ba_proc$P9C[ba_proc$P9C == 9] <- NA
ba_proc$P9C <- factor(ba_proc$P9C, levels = c(1,2,3,4), labels = c("Nada importante", "Algo importante", "Importante", "Muy importante"))

ba_proc$P9D[ba_proc$P9D == 5] <- NA
ba_proc$P9D[ba_proc$P9D == 9] <- NA
ba_proc$P9D <- factor(ba_proc$P9D, levels = c(1,2,3,4), labels = c("Nada importante", "Algo importante", "Importante", "Muy importante"))

ba_proc$P9E[ba_proc$P9E == 5] <- NA
ba_proc$P9E[ba_proc$P9E == 9] <- NA
ba_proc$P9E <- factor(ba_proc$P9E, levels = c(1,2,3,4), labels = c("Nada importante", "Algo importante", "Importante", "Muy importante"))


ba_proc$P10A[ba_proc$P10A == 5] <- NA
ba_proc$P10A[ba_proc$P10A == 9] <- NA
ba_proc$P10A <- factor(ba_proc$P10A, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

ba_proc$P10B[ba_proc$P10B == 5] <- NA
ba_proc$P10B[ba_proc$P10B == 9] <- NA
ba_proc$P10B <- factor(ba_proc$P10B, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

ba_proc$P10C[ba_proc$P10C == 5] <- NA
ba_proc$P10C[ba_proc$P10C == 9] <- NA
ba_proc$P10C <- factor(ba_proc$P10C, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

ba_proc$P10D[ba_proc$P10D == 5] <- NA
ba_proc$P10D[ba_proc$P10D == 9] <- NA
ba_proc$P10D <- factor(ba_proc$P10D, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

ba_proc$P10E[ba_proc$P10E == 5] <- NA
ba_proc$P10E[ba_proc$P10E == 9] <- NA
ba_proc$P10E <- factor(ba_proc$P10E, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

ba_proc$P10F[ba_proc$P10F == 5] <- NA
ba_proc$P10F[ba_proc$P10F == 9] <- NA
ba_proc$P10F <- factor(ba_proc$P10F, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

ba_proc$P10G[ba_proc$P10G == 5] <- NA
ba_proc$P10G[ba_proc$P10G == 9] <- NA
ba_proc$P10G <- factor(ba_proc$P10G, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

ba_proc$P10H[ba_proc$P10H == 5] <- NA
ba_proc$P10H[ba_proc$P10H == 9] <- NA
ba_proc$P10H <- factor(ba_proc$P10H, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

ba_proc$P10I[ba_proc$P10I == 5] <- NA
ba_proc$P10I[ba_proc$P10I == 9] <- NA
ba_proc$P10I <- factor(ba_proc$P10I, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

ba_proc$P11A[ba_proc$P11A == 5] <- NA
ba_proc$P11A[ba_proc$P11A == 9] <- NA
ba_proc$P11A <- factor(ba_proc$P11A, levels = c(1,2,3,4), labels = c("Nunca", "Casi nunca", "Casi siempre", "Siempre"))

ba_proc$P11B[ba_proc$P11B == 5] <- NA
ba_proc$P11B[ba_proc$P11B == 9] <- NA
ba_proc$P11B <- factor(ba_proc$P11B, levels = c(1,2,3,4), labels = c("Nunca", "Casi nunca", "Casi siempre", "Siempre"))

ba_proc$P11C[ba_proc$P11C == 5] <- NA
ba_proc$P11C[ba_proc$P11C == 9] <- NA
ba_proc$P11C <- factor(ba_proc$P11C, levels = c(1,2,3,4), labels = c("Nunca", "Casi nunca", "Casi siempre", "Siempre"))

ba_proc$P11D[ba_proc$P11D == 5] <- NA
ba_proc$P11D[ba_proc$P11D == 9] <- NA
ba_proc$P11D <- factor(ba_proc$P11D, levels = c(1,2,3,4), labels = c("Nunca", "Casi nunca", "Casi siempre", "Siempre"))

ba_proc$P11E[ba_proc$P11E == 5] <- NA
ba_proc$P11E[ba_proc$P11E == 9] <- NA
ba_proc$P11E <- factor(ba_proc$P11E, levels = c(1,2,3,4), labels = c("Nunca", "Casi nunca", "Casi siempre", "Siempre"))

#---- Rec cuestionario estudiantes ----

be_proc$P24A[be_proc$P24A == 5] <- NA
be_proc$P24A[be_proc$P24A == 9] <- NA
be_proc$P24A <- factor(be_proc$P24A, levels = c(1,2,3,4), labels = c("Nada importante", "Algo importante", "Importante", "Muy importante"))

be_proc$P24B[be_proc$P24B == 5] <- NA
be_proc$P24B[be_proc$P24B == 9] <- NA
be_proc$P24B <- factor(be_proc$P24B, levels = c(1,2,3,4), labels = c("Nada importante", "Algo importante", "Importante", "Muy importante"))

be_proc$P24C[be_proc$P24C == 5] <- NA
be_proc$P24C[be_proc$P24C == 9] <- NA
be_proc$P24C <- factor(be_proc$P24C, levels = c(1,2,3,4), labels = c("Nada importante", "Algo importante", "Importante", "Muy importante"))

be_proc$P24D[be_proc$P24D == 5] <- NA
be_proc$P24D[be_proc$P24D == 9] <- NA
be_proc$P24D <- factor(be_proc$P24D, levels = c(1,2,3,4), labels = c("Nada importante", "Algo importante", "Importante", "Muy importante"))

be_proc$P24E[be_proc$P24E == 5] <- NA
be_proc$P24E[be_proc$P24E == 9] <- NA
be_proc$P24E <- factor(be_proc$P24E, levels = c(1,2,3,4), labels = c("Nada importante", "Algo importante", "Importante", "Muy importante"))


be_proc$P25A[be_proc$P25A == 5] <- NA
be_proc$P25A[be_proc$P25A == 9] <- NA
be_proc$P25A <- factor(be_proc$P25A, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

be_proc$P25B[be_proc$P25B == 5] <- NA
be_proc$P25B[be_proc$P25B == 9] <- NA
be_proc$P25B <- factor(be_proc$P25B, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

be_proc$P25C[be_proc$P25C == 5] <- NA
be_proc$P25C[be_proc$P25C == 9] <- NA
be_proc$P25C <- factor(be_proc$P25C, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

be_proc$P25D[be_proc$P25D == 5] <- NA
be_proc$P25D[be_proc$P25D == 9] <- NA
be_proc$P25D <- factor(be_proc$P25D, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

be_proc$P25E[be_proc$P25E == 5] <- NA
be_proc$P25E[be_proc$P25E == 9] <- NA
be_proc$P25E <- factor(be_proc$P25E, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

be_proc$P25F[be_proc$P25F == 5] <- NA
be_proc$P25F[be_proc$P25F == 9] <- NA
be_proc$P25F <- factor(be_proc$P25F, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

be_proc$P25G[be_proc$P25G == 5] <- NA
be_proc$P25G[be_proc$P25G == 9] <- NA
be_proc$P25G <- factor(be_proc$P25G, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

be_proc$P25H[be_proc$P25H == 5] <- NA
be_proc$P25H[be_proc$P25H == 9] <- NA
be_proc$P25H <- factor(be_proc$P25H, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

be_proc$P25I[be_proc$P25I == 5] <- NA
be_proc$P25I[be_proc$P25I == 9] <- NA
be_proc$P25I <- factor(be_proc$P25I, levels = c(1,2,3,4), labels = c("Muy en desacuerdo", "En desacuerdo", "De acuerdo", "Muy de acuerdo"))

be_proc$P26A[be_proc$P26A == 5] <- NA
be_proc$P26A[be_proc$P26A == 9] <- NA
be_proc$P26A <- factor(be_proc$P26A, levels = c(1,2,3,4), labels = c("Nunca", "Casi nunca", "Casi siempre", "Siempre"))

be_proc$P26B[be_proc$P26B == 5] <- NA
be_proc$P26B[be_proc$P26B == 9] <- NA
be_proc$P26B <- factor(be_proc$P26B, levels = c(1,2,3,4), labels = c("Nunca", "Casi nunca", "Casi siempre", "Siempre"))

be_proc$P26C[be_proc$P26C == 5] <- NA
be_proc$P26C[be_proc$P26C == 9] <- NA
be_proc$P26C <- factor(be_proc$P26C, levels = c(1,2,3,4), labels = c("Nunca", "Casi nunca", "Casi siempre", "Siempre"))

be_proc$P26D[be_proc$P26D == 5] <- NA
be_proc$P26D[be_proc$P26D == 9] <- NA
be_proc$P26D <- factor(be_proc$P26D, levels = c(1,2,3,4), labels = c("Nunca", "Casi nunca", "Casi siempre", "Siempre"))

be_proc$P26E[be_proc$P26E == 5] <- NA
be_proc$P26E[be_proc$P26E == 9] <- NA
be_proc$P26E <- factor(be_proc$P26E, levels = c(1,2,3,4), labels = c("Nunca", "Casi nunca", "Casi siempre", "Siempre"))

#---- Cambiar labels ----
# Base de datos apoderados

ba_proc$P9A <- set_label(ba_proc$P9A, "Provenir de una familia rica o con recursos")
ba_proc$P9B <- set_label(ba_proc$P9B, "Tener un buen nivel de educación")
ba_proc$P9C <- set_label(ba_proc$P9C, "Tener ambición")
ba_proc$P9D <- set_label(ba_proc$P9D, "El trabajo duro")

ba_proc$P10A <- set_label(ba_proc$P10A, "Las diferencias económias en Chile son demasiado grandes")
ba_proc$P10B <- set_label(ba_proc$P10B, "Es responsabilidad del gobierno reducir las diferencias económicas entre las personas con altos ingresos y aquellas con bajos ingresos")
ba_proc$P10C <- set_label(ba_proc$P10C, "En este colegio/liceo, los esfuerzos son recompensados")
ba_proc$P10D <- set_label(ba_proc$P10D, "En Chile, los que se esfuerzan salen adelante")
ba_proc$P10E <- set_label(ba_proc$P10E, "Está bien que aquellos que puedan pagar más tengan mejor educación")
ba_proc$P10F <- set_label(ba_proc$P10F, "Está bien que aquellos que puedan pagar tengan mejor acceso a salud")
ba_proc$P10G <- set_label(ba_proc$P10G, "Está bien que las personas más inteligentes y/o talentosas ganen más dinero aun cuando requieran esforzarse menor para ello")
ba_proc$P10H <- set_label(ba_proc$P10H, "En general, la inteligencia es algo que no cambia")
ba_proc$P10I <- set_label(ba_proc$P10I, "Se puede aprender nuevas cosas, pero no se puede cambiar la inteligencia de una persona")

ba_proc$P11A <- set_label(ba_proc$P11A, "Falta de habilidad o talento de las personas")
ba_proc$P11B <- set_label(ba_proc$P11B, "Solo mala suerte")
ba_proc$P11C <- set_label(ba_proc$P11C, "Falta de esfuerzo de los pobres")
ba_proc$P11D <- set_label(ba_proc$P11D, "Fallas del sistema económico")
ba_proc$P11E <- set_label(ba_proc$P11A, "Fallas del sistema educativo")


# Base de datos estudiantes
be_proc$P24A <- set_label(be_proc$P24A, "Provenir de una familia rica o con recursos")
be_proc$P24B <- set_label(be_proc$P24B, "Tener un buen nivel de educación")
be_proc$P24C <- set_label(be_proc$P24C, "Tener ambición")
be_proc$P24D <- set_label(be_proc$P24D, "El trabajo duro")

be_proc$P25A <- set_label(be_proc$P25A, "Las diferencias económias en Chile son demasiado grandes")
be_proc$P25B <- set_label(be_proc$P25B, "Es responsabilidad del gobierno reducir las diferencias económicas entre las personas con altos ingresos y aquellas con bajos ingresos")
be_proc$P25C <- set_label(be_proc$P25C, "En este colegio/liceo, los esfuerzos son recompensados")
be_proc$P25D <- set_label(be_proc$P25D, "En Chile, los que se esfuerzan salen adelante")
be_proc$P25E <- set_label(be_proc$P25E, "Está bien que aquellos que puedan pagar más tengan mejor educación")
be_proc$P25F <- set_label(be_proc$P25F, "Está bien que aquellos que puedan pagar tengan mejor acceso a salud")
be_proc$P25G <- set_label(be_proc$P25G, "Está bien que las personas más inteligentes y/o talentosas ganen más dinero aun cuando requieran esforzarse menor para ello")
be_proc$P25H <- set_label(be_proc$P25H, "En general, la inteligencia es algo que no cambia")
be_proc$P25I <- set_label(be_proc$P25I, "Se puede aprender nuevas cosas, pero no se puede cambiar la inteligencia de una persona")

be_proc$P26A <- set_label(be_proc$P26A, "Falta de habilidad o talento de las personas")
be_proc$P26B <- set_label(be_proc$P26B, "Solo mala suerte")
be_proc$P26C <- set_label(be_proc$P26C, "Falta de esfuerzo de los pobres")
be_proc$P26D <- set_label(be_proc$P26D, "Fallas del sistema económico")
be_proc$P26E <- set_label(be_proc$P26A, "Fallas del sistema educativo")

# Guardar bases
save(ba_proc, file = "ba_proc.RData")
save(be_proc, file = "be_proc.RData")

#---- Descriptivo general ----

view(dfSummary(ba_proc[,1:18])) # De la 18 a la 24 las preguntas son por ingresos

view(dfSummary(be_proc))
