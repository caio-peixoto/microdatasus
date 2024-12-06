############ Recodifica alguns municípios #####
# Essa função recodifica as alterações na variável do código do município a partir de 2000 até a presente data.
# Isso faz com que a série histórica municipal tenha sempre 5.570 municípios válidos.

Recodifica_mun <- function(var1) {

    var1[var1 == "179999"] <- "170000"
    var1[var1 == "170172"] <- "170000"
    var1[var1 == "171350"] <- "170000"
    var1[var1 == "172206"] <- "170000"

    var1[substring(var1,1,2) == "53"] <- "530010"

    var1[as.numeric(var1) >= 334501 & as.numeric(var1) <= 334530] <- "330455"
  

    var1[var1 == "339999"] <- "330000"
    var1[var1 == "330064"] <- "330000"

    var1[as.numeric(var1) >= 358001 & as.numeric(var1) <= 358058] <- "355030"
  
    var1[var1 == "430145"] <- "431454"
    var1[var1 == "431453"] <- "431454"

    var1
  }

############## Recodifica alguns municípios - Fim ####
