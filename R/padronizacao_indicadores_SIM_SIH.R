# PROADESS: Padronização de indicadores SIM e SIH --------------------------
# Função de Ajuste de Indicadores de Mortalidades (SIM) e Internações Hospitalares (SIH)
# Calcula Taxa Bruta e Taxa Padronizada, por indicador e por abrangência selecionada.
# Método Direto e Indireto.
#
# Resultados para:
#   - Municípios
#   - Regiões de Saúde
#   - Macrorregiões de Saúde
#   - UFs
#   - Grandes Regiões
#   - Brasil
#
#
# Método:
# 1) utiliza o pacote 'microdatasus' para numeradores SIM ou SIH.
# 2) utiliza o pacote 'brpop' para denominador população por sexo e idade.
#
# 3) arquivo 'cod_PAD.csv' contém variável "IdPad" com as faixas etárias utilizadas.
# 4) Lista com códigos das abrangências em 'Lista_Cirs_xx.csv'.
# 5) Salva resultados na subpasta 'Resultados_PAD'.
#
#
# Nota Técnica: "https://proadess.fiocruz.br/Nota%20Tecnica%20PROADESS_1_Padronizacao%20de%20indicadores.pdf"
# População base utilizada = Populaçao 2010 SVS.
#


# Estrutura dados Numerador e População: ----------------------------------

# Numerador:
# base_N_sim_sih (9 colunas):
# "Mun";"Ano";"Sexo";"Idade";"IdPad";"Cod_indicador";"Base";"Numerador";"Freq"
# 	"Numerador"=1
# 	"Base"= 3 (SIM) ou "Base"=1 (SIH)
#	"IdPad" = faixa etária utilizada do arquivo 'cod_PAD.cv'
#	"Freq"' = soma de óbitos/AIHs aprovadas 'tipo 1'
#
# Denominador ('POPULACAO' = soma de população):
# data_D_pop (6 colunas):
# 1 MUNIC_RES  C
# 2 ANO        C
# 3 SEXO       C
# 4 SITUACAO   C
# 5 FXETARIA   C
# 6 POPULACAO  N



# Carrega pacotes ---------------------------------------------------

library(microdatasus)
library(brpop)


loadlibrary <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x,
                     repos = 'http://cran.fiocruz.br/',
                     dep = TRUE,
                     type = "source")
    if (!require(x, character.only = TRUE))
      stop("Package not found")
  }
}

pacotes_pad <-
  c("dplyr",
    "lubridate",
    "foreign",
    "svMisc",
    "data.table",
    "tcltk",
    "readr",
    "stats")
for (i in 1:length(pacotes_pad)) {
  suppressPackageStartupMessages(loadlibrary(pacotes_pad[i]))
}



# Lista PAD com recortes etários ------------------------------------
PAD <- data.table(read.csv2("./cod_PAD.csv",as.is = TRUE, encoding = "UTF-8"))
#Estruta PAD:
#"id";"Nome";"IdadeI";"IdadeF";"Faixa"
#1;"Todas as idades";0;150;"c(""000-014"",""015-024"",""025-034"",""035-044"",""045-054"", ""055-064"", ""065-150"")"
#2;"15 anos ou mais";15;150;"c(""015-024"",""025-034"",""035-044"",""045-054"", ""055-064"", ""065-150"")"
#3;"18 anos ou mais";18;150;"c(""018-024"",""025-034"",""035-044"",""045-054"", ""055-064"", ""065-150"")"
#4;"20 anos ou mais";20;150;"c(""020-024"",""025-034"",""035-044"",""045-054"", ""055-064"", ""065-150"")"
#5;"40 anos ou mais";40;150;"c(""040-044"",""045-054"", ""055-064"", ""065-150"")"
#6;"60 anos ou mais";60;150;"c(""060-069"",""070-079"", ""080-150"")"
#7;"65 anos ou mais";65;150;"c( ""065-069"", ""070-079"",""080-150"")"
#8;"30 a 79 anos";30;79;"c(""030-039"",""040-049"",""050-059"", ""060-069"", ""070-079"")"
#9;"50 a 69 anos";50;69;"c(""050-054"",""055-059"",""060-064"",""065-069"")"
#10;"50 a 64 anos";50;64;"c(""050-054"", ""055-059"", ""060-064"")"
#11;"20 a 79 anos";20;79;"c(""020-029"",""030-039"",""040-049"",""050-059"", ""060-069"", ""070-079"")"
#12;"70 anos ou mais";70;150;"c( ""070-079"",""080-150"")"
#13;"45 anos ou mais";45;150;"c(""045-054"", ""055-064"", ""065-150"")"
#14;"30 a 69 anos";30;69;"c(""030-039"",""040-049"",""050-059"", ""060-069"")"
#15;"30 a 49 anos";30;49;"c(""030-034"",""035-039"",""040-044"", ""045-049"")"


# SELECAO PARAMETROS -------------------------------------------------

cat(sprintf("\n ESCOLHA A PADRONIZACAO : POR MUNICIPIOS, REGIAO DE SAUDE ou MR, via 'Lista_Cirs.csv' \n"))

#escolhe_regiao <- "municipio"
escolhe_regiao <- "regiao_saude"
#escolhe_regiao <- "macrorregiao"

# ESCOLHA SE A PADRONIZACAO usar: o banco SIM ou banco SIH
cat(sprintf("\n ESCOLHA entre PADRONIZAR o banco SIM ou banco SIH \n"))
escolhe_banco <- "SIM" #escolhe_banco <- "SIH"


# Seleciona Ano Inicial e Final -------------------------------------------
ANOINICIAL <- 2010
ANOFINAL <- 2010

ANO <- ANOINICIAL:ANOFINAL

cat(sprintf("\n ### INICIO da Padronizacao, usando o banco %s, recorte regional=%s, Ano Inicial=%d e Ano Final=%d \n",escolhe_banco,escolhe_regiao,ANOINICIAL,ANOFINAL))


# Baixar dados SIM --------------------------------------------------
sim_raw <- fetch_datasus(
  year_start = ANOINICIAL,
  year_end = ANOFINAL,
  uf = "all",
  information_system = "SIM-DO",
  vars = c("CODMUNRES", "DTOBITO", "CAUSABAS","IDADE","SEXO"),
  track_source = TRUE
)

sim <- process_sim(sim_raw)


# Faz o numerador do indicador  --------------------------------------------------

# Exemplo: B16 Mortalidade por diabetes.
# 	Definição: Número de óbitos por diabetes na população residente de 20 a 79 anos, por 100 mil habitantes de 20 a 79 anos de idade, em determinado espaço geográfico, no ano considerado.
# 	Numerador: número de óbitos por diabetes em residentes com 20 a 79 anos de idade. Códigos CID-10: E10-E14.
# 	Denominador: população total residente com 20 a 79 anos de idade.
# 	Cod_indicador = "B16"

sim <- sim %>% filter(
	IDADEanos >= 20 & IDADEanos <= 79 &
	(substring(CAUSABAS,1,2) == "E1" & as.numeric(substring(CAUSABAS,3,3)) <= 4)
	)

sim$Ano <- substring(sim$DTOBITO,1,4)

sim$SEXO[sim$SEXO=="Masculino"] <- "1"
sim$SEXO[sim$SEXO=="Feminino"] <- "2"
sim$SEXO <- as.numeric(sim$SEXO)

sim <- sim %>% select(CODMUNRES,Ano,SEXO,IDADEanos,CAUSABAS)

sim$IdPad <- 11 #aqui é definido as faixas etários do indicador
sim$Cod_indicador <- "B16"
sim$Base <- 3
sim$Numerador <- 1

sim <- sim %>%
  rename(
    Mun = CODMUNRES,
    Sexo = SEXO,
    Idade = IDADEanos
    )

sim$Freq <- 1

base <- sim %>% select(Mun,Ano,Sexo,Idade,IdPad,Cod_indicador,Base,Numerador,Freq)
rm(sim, sim_raw)


base <- base %>%
  mutate(
    Mun = as.integer(Mun),
    Ano = as.integer(Ano),
    Sexo = as.numeric(Sexo),
    Idade = as.numeric(Idade)
    )


base <- data.table(base, stringsAsFactors = FALSE)
base <- base[, list("Freq" = sum(Freq, na.rm = TRUE)), by = eval(paste0(names(base)[1:8], collapse =","))]




# Baixar dados População municipal por sexo e idade ------------------

data <- mun_sex_pop(source = "datasus2024")
data <- data[data$year >= ANOINICIAL & data$year <= ANOFINAL,]


data[[1]] <- substring(data[[1]],1,6)

 data <- data %>%
   rename(
     MUNIC_RES = code_muni,
     ANO = year,
     FXETARIA = age_group,
     POPULACAO = pop,
     SEXO = sex
   )

data <- data %>%
  dplyr::mutate(
    SEXO = dplyr::recode_values(
      .data$SEXO,
      NA ~ "9",
      "Male" ~ "1",
      "Female" ~ "2",
      default = .data$SEXO)
  )

data <- data %>%
  mutate(SEXO = as.numeric(SEXO))

data$SITUACAO <- "1"



data <- data %>% select(MUNIC_RES,ANO,SEXO,SITUACAO,FXETARIA,POPULACAO)

data <- subset(data, data$FXETARIA != "Total")

data <- data %>%
  dplyr::mutate(
    FXETARIA = dplyr::recode_values(
      .data$FXETARIA,
      "From 0 to 4 years" ~ "0004",
      "From 5 to 9 years" ~ "0509",
      "From 10 to 14 years" ~ "1014",
      "From 15 to 19 years" ~ "1519",
      "From 20 to 24 years" ~ "2024",
      "From 25 to 29 years" ~ "2529",
      "From 30 to 34 years" ~ "3034",
      "From 35 to 39 years" ~ "3539",
      "From 40 to 44 years" ~ "4044",
      "From 45 to 49 years" ~ "4549",
      "From 50 to 54 years" ~ "5054",
      "From 55 to 59 years" ~ "5559",
      "From 60 to 64 years" ~ "6064",
      "From 65 to 69 years" ~ "6569",
      "From 70 to 74 years" ~ "7074",
      "From 75 to 79 years" ~ "7579",
      "From 80 years or more" ~ "8099",
      default = .data$FXETARIA)
  )



data <- data %>%
  mutate(
    MUNIC_RES = as.integer(MUNIC_RES),
    ANO = as.integer(ANO),
    SEXO = as.numeric(SEXO),
    POPULACAO = as.numeric(POPULACAO)
  )

data <- data.table(data, stringsAsFactors = FALSE)
data <- data[, list("POPULACAO" = sum(POPULACAO)), by = eval(paste0(names(data)[1:5], collapse = ","))]




# Inicia Padronização ------------------------------------------------




#Transformacoes na base POP
#Nesta versao, criou-se um mecanismo que faz com que os resultados da tabulacao bruta(sem sex/idade) sejam aproveitados como o multiplicador
#da taxa padronizada indireta...Adotoram-se as medidas:

#1- Merge dos totais sem sex/idade para que eles fossem aproveitados na tabela IPADRAO(brasil 2010).
#2- Os resultados são carregados por meio da variavel IOBSPAD_BASE





#Funcao que transfere a informacao de determinados codigos municipais para a listagem oficial de municipios
############ Recodifica alguns municípios #####
Regrasmun <- function(var1) {

  var1[var1 == 179999] <- 170000
  var1[var1 == 170172] <- 170000
  var1[var1 == 171350] <- 170000
  var1[var1 == 172206] <- 170000

  var1[substring(var1,1,2) == 53] <- 530010

  #var1[as.numeric(var1) >= 334501 & as.numeric(var1) <= 334530] <- 330455
  var1[var1 >= 334501 & var1 <= 334530] <- 330455

  var1[var1 == 339999] <- 330000
  var1[var1 == 330064] <- 330000

  var1[as.numeric(var1) >= 358001 & as.numeric(var1) <= 358058] <- 355030
  #var1[var1 >= 358001 & var1 <= 358058] <- 355030

  var1[var1 == 430145] <- 431454
  var1[var1 == 431453] <- 431454

# Na nova lista com 5571 municípios do IBGE, temos agora:
# "Boa Esperança do Norte" - código "510183"
# https://pt.wikipedia.org/wiki/Boa_Esperança_do_Norte
#
#
# Não há dados para esse município nos bancos do datatus.
# Então ele será excluído, mantendo 5570 municípios no PROADESS.

var1[var1==510183] <- 510000

var1
}

############ Recodifica alguns municípios - Fim


options(scipen = 999)
options(stringsAsFactors = F)

#Seta o ano final da base
#E usado, pois muitas vezes trabalhamos com dados que ainda nao sao finalizados
#Os dados da população sofre um RBIND, gerando uma base com todos os anos


#data <- NULL




# Define Local da base População ------------------------------------------


### Aqui temos as faixas etárias com 4 dígitos, sendo idade simples de 0 a 19 anos, depois por grupo de 5 anos - formatação CENSO #####
#pasta <- "E:\\PROADESS\\Populacao_Tabnet\\BASE_POP_2026\\"
#pasta <- ".\\POP_SVS\\"

#list.files(pasta, full.names = TRUE, pattern = "*.dbf", recursive = T, ignore.case = TRUE)



###########################Inicio transformacoes POP#####################################
#for (i in ANO) {

#  pastaano <- paste0(pasta, i)
#  for (j in 1:length(list.files(
#    pastaano,
#    full.names = TRUE,
#    pattern = "*.dbf",
#    ignore.case = TRUE, recursive = F
#  ))) {
#    data <-
#      rbind(data, data.table(read.dbf(
#        list.files(
#          pastaano,
#          full.names = TRUE,
#          pattern = "*.dbf",
#          ignore.case = TRUE, recursive = F
#        )[j],
#        as.is = TRUE
#      )))
#    cat(sprintf("\n Lendo arquivo '%s'", list.files(
#      pastaano,
#      full.names = TRUE,
#      pattern = "*.dbf",
#      ignore.case = TRUE, recursive = F
#    )[j]))
#  }
#}



data[, MUNIC_RES := Regrasmun(data[, MUNIC_RES])]

format(object.size(data), units = "auto")

# 1 MUNIC_RES  C
# 2 ANO        C
# 3 SEXO       C
# 4 SITUACAO   C
# 5 FXETARIA   C
# 6 POPULACAO  N

setcolorder(data, c(1, 2, 3, 5, 4, 6))

# 1 MUNIC_RES  C
# 2 ANO        C
# 3 SEXO       C
# 5 FXETARIA   C
# 4 SITUACAO   C
# 6 POPULACAO  N

data <- data[, list("POPULACAO" = sum(POPULACAO)), by = eval(paste0(names(data)[1:4], collapse = ","))]
cat(sprintf("\n Lista das faixas etárias POPSVS '%s'", sort(unique(data$FXETARIA))))



###########################Fim transformacoes POP#####################################


# Leitura PAD, DADOS, CIRS ###########################################################

#PAD <- read.csv2("./cod_PAD.csv",as.is = TRUE, encoding = "UTF-8")
#PAD <- data.table(PAD)
#str(PAD)

cat(sprintf("\n Lista das faixas etárias POPSVS '%s'", sort(unique(PAD$Faixa))))


#if (escolhe_banco == "SIM") {
#     pasta <- "./Resultado/SIM/PAD/"
#}

#if (escolhe_banco == "SIH") {
#  pasta <- "./Resultado/SIH/PAD/"
#}


#base <- NULL

#for (z in 1:length(list.files(pasta, full.names = TRUE, pattern = ".csv"))) {
#
#  sink("./logs/padronizacao.txt", append = TRUE, split = TRUE)
#  cat(sprintf("Lendo arquivo '%s' \n", list.files(pasta, full.names = TRUE, pattern = ".csv")[z]))
#  sink()
#  base <- rbind(base, data.table(read.csv2(list.files(pasta, full.names = TRUE, pattern = ".csv")[z], as.is = TRUE, encoding = "UTF-8", na.strings = c("","-1"))))
#}



# base <- base[, list("Freq" = sum(Freq, na.rm = TRUE)), by = eval(paste0(names(base)[1:8], collapse =","))]
#
# base$Sexo[base$Sexo==9] <- NA



#  Memória ----------------------------------------------------------

base[, Mun := Regrasmun(base[, Mun])]

base <- base[, list("Freq" = sum(Freq, na.rm = TRUE)), by = eval(paste0(names(base)[1:8], collapse =","))]

# Devemos trocar a cir por abrangência ------------------------------------

if (isTRUE(escolhe_regiao == "municipio")) {
  Cirs <-
    read.csv2("./Lista_Cirs_MUN.csv",
              as.is = TRUE,
              encoding = "UTF-8")
}
if (isTRUE(escolhe_regiao == "regiao_saude")) {
  Cirs <-
    read.csv2("./Lista_Cirs_RS.csv",
              as.is = TRUE,
              encoding = "UTF-8")
}

if (isTRUE(escolhe_regiao == "macrorregiao")) {
  Cirs <-
    read.csv2("./Lista_Cirs_MR.csv",
              as.is = TRUE,
              encoding = "UTF-8")
}

#Cirs <- data.table(Cirs)

#View(Cirs)
str(Cirs)

Cirs <- Cirs[, 1:2]

# Cirs[[1]] <- as.character(Cirs[[1]])
# Cirs[[2]] <- as.character(Cirs[[2]])

Cirs[[1]] <- as.integer(Cirs[[1]])
Cirs[[2]] <- as.integer(Cirs[[2]])



# Exlclui os municípios ignorados -----------------------------------------

str(base)
#View(base)
base <- left_join(base, Cirs, by="Mun")

#base <- subset(base, substring(base$Cgr,1,4) != "9999" & !is.na(base$Mun))

base <- base[,1:9]

base <- base[, list("Freq" = sum(Freq, na.rm = TRUE)), by = eval(paste0(names(base)[1:8], collapse =","))]

str(base)



###################



base <-
  cbind(
    base,
    "UF" = substring(base$Mun, 1, 2),
    "RG" = substring(base$Mun, 1, 1),
    "BR" = 555
  )
#base <- merge(base,Cirs)

Mun <- as.integer(sort(unique(c(data[, MUNIC_RES], Cirs[, 1], base[, Mun]))))

Cgr <- sort(unique(Cirs[, 2]))
UF <-
  c(
    11,
    12,
    13,
    14,
    15,
    16,
    17,
    21,
    22,
    23,
    24,
    25,
    26,
    27,
    28,
    29,
    31,
    32,
    33,
    35,
    41,
    42,
    43,
    50,
    51,
    52,
    53
  )
RG <- c(1, 2, 3, 4, 5)
BR <- 555

Agregacoes <- c("Cgr", "UF", "RG", "BR")
SEXO <- 1:2


tabmerge <- NULL
#############################Leitura PAD, DADOS, CIRS#######################################################################################

#i=1

#Loop pelos tipos de IdPAD - cada id vai produzir um loop objeto res recebe a populacao
for (i in 1:length(unique(base$IdPad))) {
  res <- copy(data)
  Id <- unique(base$IdPad)[i]
  FaixasPAD <- eval(parse(text = PAD$Faixa[Id]))
  res[, Faixas := NA]
  res[, IdPad := Id]
  res[, NomeId := NA]
  res[, Faixas := as.integer(Faixas)]
  res[, IdPad := as.integer(IdPad)]
  res[, NomeId := as.character(NomeId)]



  #Tabela padrao - tabela com o padrao municipio ano sexo e faixa etaria
  tabpadrao <-
    data.table(expand.grid(Mun, ANO, SEXO, c(1:length(FaixasPAD))))
  tabpadrao[, NomeId := NA]
  tabpadrao[, NomeId := as.character(NomeId)]
  names(tabpadrao)[1] <- "Mun"
  names(tabpadrao)[2] <- "Ano"
  names(tabpadrao)[3] <- "Sexo"
  names(tabpadrao)[4] <- "Faixas"

  #criacao das faixas etarias no banco pop e agregacao- reconhecimento das faixas etarias
  for (p in 1:length(FaixasPAD)) {
    tabpadrao[Faixas == p, NomeId := FaixasPAD[p]]

    IdadeI <- as.numeric(strsplit(FaixasPAD[p], "-")[[1]][1])
    IdadeF <- as.numeric(strsplit(FaixasPAD[p], "-")[[1]][2])

    res[as.numeric(substring(res[, FXETARIA], 3, 4)) >= IdadeI & as.numeric(substring(res[, FXETARIA, ], 3, 4)) <= IdadeF, Faixas := p]
    res[as.numeric(substring(res[, FXETARIA], 3, 4)) >= IdadeI & as.numeric(substring(res[, FXETARIA, ], 3, 4)) <= IdadeF, NomeId := FaixasPAD[p]]

    # res[as.numeric(substring(res[, FXETARIA], 3, 4)) >= IdadeI & as.numeric(substring(res[, FXETARIA], 3, 4)) <= IdadeF, Faixas := p]
    # res[as.numeric(substring(res[, FXETARIA], 3, 4)) >= IdadeI & as.numeric(substring(res[, FXETARIA], 3, 4)) <= IdadeF, NomeId := FaixasPAD[p]]

  }

  setcolorder(res, c(1, 2, 3, 6, 7, 8, 4, 5))
  res <-
    res[, list("POPULACAO" = sum(POPULACAO)), by = eval(paste0(names(res)[1:6], collapse = ","))]
  res <- subset(res, !is.na(res$Faixas))
  res[, MUNIC_RES := as.character(MUNIC_RES)]
  res[, MUNIC_RES := as.integer(MUNIC_RES)]
  res[, ANO := as.character(ANO)]
  res[, ANO := as.integer(ANO)]
  res[, SEXO := as.character(SEXO)]
  res[, SEXO := as.integer(SEXO)]


  tabpadrao[, Mun := as.character(Mun)]
  tabpadrao[, Mun := as.integer(Mun)]

  tabpadrao[, Ano := as.character(Ano)]
  tabpadrao[, Ano := as.integer(Ano)]
  tabpadrao[, Sexo := as.integer(Sexo)]


  #merge padrao e populacao
  tabpadrao <-
    merge(
      tabpadrao,
      res,
      by.x = c("Mun", "Ano", "Sexo", "Faixas"),
      by.y = c("MUNIC_RES", "ANO", "SEXO", "Faixas"),
      all.x = TRUE
    )
  names(tabpadrao)[5] = "NomeId"
  tabpadrao[, NomeId.y := NULL]



  indicador <- unique(base$Cod_indicador[base$IdPad == Id])
  #z=6
  #objeto tmpind recebe os dados dos indicadores
  for (z in 1:length(indicador)) {
    tmpind <- subset(base, base$Cod_indicador == indicador[z])
    tmpind <- tmpind[, c(
      which(names(tmpind) == "Mun"),
      which(names(tmpind) == "Ano"),
      which(names(tmpind) == "Sexo"),
      which(names(tmpind) == "Idade"),
      which(names(tmpind) == "IdPad"),
      which(names(tmpind) == "Freq")
    ), with = F]

    #tmpind<-data.table(tmpind)
    tmpind[, Faixas := NA]
    tmpind[, Faixas := as.integer(Faixas)]



    for (p in 1:length(FaixasPAD)) {
      IdadeI <- as.numeric(strsplit(FaixasPAD[p], "-")[[1]][1])
      IdadeF <- as.numeric(strsplit(FaixasPAD[p], "-")[[1]][2])

      tmpind[as.numeric(tmpind[, Idade]) >= IdadeI &
               as.numeric(tmpind[, Idade]) <= IdadeF, Faixas := p]
      tmpind[as.numeric(tmpind[, Idade]) >= IdadeI &
               as.numeric(tmpind[, Idade]) <= IdadeF, NomeId := FaixasPAD[p]]

    }

    # tmpind[, Faixas := as.integer(Faixas)]
    # tmpind[, Mun := as.integer(Mun)]
    # tmpind[, Ano := as.integer(Ano)]


    setcolorder(tmpind, c(1, 2, 3, 7, 8, 5, 4, 6))
    tmpind <-
      tmpind[, list("Freq" = sum(Freq)), by = eval(paste0(names(tmpind)[1:6], collapse =","))]
    tmpind <- subset(tmpind, !is.na(tmpind$Faixas))

    # por fim. junta-se a informacao da pop com as frequencias dos indicadores

    tabpadrao$Mun <- as.integer(tabpadrao$Mun)
    tabpadrao$Ano <- as.integer(tabpadrao$Ano)
    tabpadrao$Sexo <- as.integer(tabpadrao$Sexo)
    tmpind$Mun <- as.integer(tmpind$Mun)
    tmpind$Ano <- as.integer(tmpind$Ano)
    tmpind$Sexo <- as.integer(tmpind$Sexo)



    tmpind <-
      merge(
        tabpadrao,
        tmpind,
        by = c("Mun", "Ano", "Sexo", "Faixas", "NomeId", "IdPad"),
        all.x = TRUE
      )
    tmpind[is.na(Freq), Freq := 0]
    tmpind[, Indicador := indicador[z]]

    tabmerge <- rbind(tabmerge, tmpind)


  }


}

cat(sprintf("\n estrutura do tabmerge \n"))
str(tabmerge)


#tabmerge2 recebe a informacao das cirs
str(tabmerge$Mun);str(Cirs$Mun)
tabmerge$Mun<- as.integer(tabmerge$Mun)
Cirs$Mun<- as.integer(Cirs$Mun)


tabmerge2 <- merge(tabmerge, Cirs, by = "Mun", all.x = TRUE)

# Correção retira ignorados parte2 ----------------------------------------

#tabmerge2 <- subset(tabmerge2, substring(tabmerge2$Cgr,1,4) != "9999" & !is.na(tabmerge2$Mun))
tabmerge2 <- subset(tabmerge2, !is.na(tabmerge2$Mun))
str(tabmerge$Mun);str(Cirs$Mun)









#Traz o total de casos, deconsiderando o sexo e idade...


# CORRECAO 2 --------------------------------------------------------------
tabmerge2$Mun <- as.integer(tabmerge2$Mun)
base$Mun <- as.integer(base$Mun)
#MODIFICADO
tabmerge2 <-
  merge(
    tabmerge2,
    base[, list("Freq" = sum(Freq, na.rm = TRUE)), by = eval(paste0(names(base)[c(1, 2, 5, 6)], collapse =","))],
    by.x = c("Mun", "Ano", "Indicador"),
    by.y = c("Mun", "Ano", "Cod_indicador"),
    all.x = TRUE
  )


tabmerge2[, "UF" := trunc(Mun / 10000)]
tabmerge2[, "RG" := trunc(Mun / 100000)]
tabmerge2[, "BR" := 555]

names(tabmerge2)[9] <- "Freq"
names(tabmerge2)[12] <- "Freq_base"
tabmerge2[is.na(Freq_base), Freq_base := 0]
tabmerge2[is.na(IdPad.x), IdPad.x := IdPad.y]
names(tabmerge2)[7] <- "IdPad"


tabmerge2 <- tabmerge2[!is.na(IdPad), ]


tabmerge2[Mun == 170000 & Ano == 2010 & Indicador == "M11_N_SIM"]





# ALTA MEMÓRIA nos subsets!! ----------------------------------------------------------


#tabmerge2 retira de sua base indicadores nao compativeis com determinado sexos

tabmerge2 <-
  subset(tabmerge2, !(Indicador == "G01_N_SIH" & Sexo == 1))
tabmerge2 <-
  subset(tabmerge2, !(Indicador == "M10_N_SIM" & Sexo == 1))
tabmerge2 <-
  subset(tabmerge2, !(Indicador == "M11_N_SIM" & Sexo == 1))
tabmerge2 <-
  subset(tabmerge2, !(Indicador == "M12_N_SIM" & Sexo == 2))


# Indicador M50:

tabmerge2 <-
  subset(tabmerge2, !(Indicador == "M51_PULMAO_N" & Sexo == 2))

  tabmerge2 <-
  subset(tabmerge2, !(Indicador == "M52_PULMAO_N" & Sexo == 1))

# Mortalidade Prematura:

  tabmerge2 <- subset(tabmerge2, !((Indicador == "M17_N_SIM_H" | Indicador == "M22_N_SIM_H") & Sexo == 2))
  tabmerge2 <- subset(tabmerge2, !((Indicador == "M24_N_SIM_H" |Indicador == "M27_N_SIM_H") & Sexo == 2))
  tabmerge2 <- subset(tabmerge2, !((Indicador == "M30_N_SIM_H" |Indicador == "M33_N_SIM_H"|Indicador == "M36_N_SIM_H") & Sexo == 2))

  tabmerge2 <- subset(tabmerge2, !((Indicador == "M18_N_SIM_M" | Indicador == "M19_N_SIM_M") & Sexo == 1))

  tabmerge2 <- subset(tabmerge2, !((Indicador == "M20_N_SIM_M" |Indicador == "M21_N_SIM_M") & Sexo == 1))

  tabmerge2 <- subset(tabmerge2, !((Indicador == "M25_N_SIM_M" |Indicador == "M28_N_SIM_M") & Sexo == 1))


  tabmerge2 <- subset(tabmerge2, !((Indicador == "M31_N_SIM_M"|Indicador == "M34_N_SIM_M") & Sexo == 1))

  tabmerge2 <- subset(tabmerge2, !((Indicador == "M37_N_SIM_M"|Indicador == "M38_N_SIM_M") & Sexo == 1))




# Ano base definição - 2010 -----------------------------------------------


#Aqui usa-se a variavel Freq_base que representa a soma dos casos sem considerar o sexo e idade para 2010 - IOBSPAD_BASE...

IPADRAO <-
  tabmerge2[Ano == 2010 &
              BR == 555, list(
                "IPOPPAD" = sum(POPULACAO, na.rm = TRUE),
                "ICASOSPAD" = sum(Freq, na.rm = TRUE),
                "IOBSPAD_BASE" = sum(Freq_base, na.rm = TRUE)
              ), by = "Ano,Sexo,NomeId,IdPad,Indicador"]

IPADRAO[, "IOBSPAD" := sum(ICASOSPAD, na.rm = TRUE), by = "Indicador"]
IPADRAO[, "IPOPTOTPAD" := sum(IPOPPAD, na.rm = TRUE), by = "Indicador"]
IPADRAO[, "ITXESPCPAD" := ICASOSPAD / IPOPPAD]
tabmerge2 <-
  merge(
    tabmerge2,
    IPADRAO[, Ano := NULL],
    by = c("Sexo", "NomeId", "IdPad", "Indicador"),
    all.x = TRUE
  )
tabmerge2[, "IEXPPAD" := ITXESPCPAD * Freq]







Cgrbase <-
  tabmerge2[, list(
    "POPULACAO" = sum(as.numeric(POPULACAO), na.rm = TRUE),
    "Freq" = sum(as.numeric(Freq)),
    "Freq_base" = sum(as.numeric(Freq_base)),
    "IPOPPAD" = mean(as.numeric(IPOPPAD)),
    "ICASOSPAD" = mean(as.numeric(ICASOSPAD)),
    "IOBSPAD_BASE" = mean(as.numeric(IOBSPAD_BASE)),
    "IOBSPAD" = mean(as.numeric(IOBSPAD)),
    "IPOPTOTPAD" = mean(as.numeric(IPOPTOTPAD), na.rm = TRUE),
    "ITXESPCPAD" = mean(as.numeric(ITXESPCPAD))
  ), by = "Cgr,Ano,Sexo,Faixas,NomeId,IdPad,Indicador"]
UFbase <-
  tabmerge2[, list(
    "POPULACAO" = sum(as.numeric(POPULACAO), na.rm = TRUE),
    "Freq" = sum(as.numeric(Freq)),
    "Freq_base" = sum(as.numeric(Freq_base)),
    "IPOPPAD" = mean(as.numeric(IPOPPAD)),
    "ICASOSPAD" = mean(as.numeric(ICASOSPAD)),
    "IOBSPAD_BASE" = mean(as.numeric(IOBSPAD_BASE)),
    "IOBSPAD" = mean(as.numeric(IOBSPAD)),
    "IPOPTOTPAD" = mean(as.numeric(IPOPTOTPAD), na.rm = TRUE),
    "ITXESPCPAD" = mean(as.numeric(ITXESPCPAD))
  ), by = "UF,Ano,Sexo,Faixas,NomeId,IdPad,Indicador"]
RGbase <-
  tabmerge2[, list(
    "POPULACAO" = sum(as.numeric(POPULACAO), na.rm = TRUE),
    "Freq" = sum(as.numeric(Freq)),
    "Freq_base" = sum(as.numeric(Freq_base)),
    "IPOPPAD" = mean(as.numeric(IPOPPAD)),
    "ICASOSPAD" = mean(as.numeric(ICASOSPAD)),
    "IOBSPAD_BASE" = mean(as.numeric(IOBSPAD_BASE)),
    "IOBSPAD" = mean(as.numeric(IOBSPAD)),
    "IPOPTOTPAD" = mean(as.numeric(IPOPTOTPAD), na.rm = TRUE),
    "ITXESPCPAD" = mean(as.numeric(ITXESPCPAD))
  ), by = "RG,Ano,Sexo,Faixas,NomeId,IdPad,Indicador"]
BRbase <-
  tabmerge2[, list(
    "POPULACAO" = sum(as.numeric(POPULACAO), na.rm = TRUE),
    "Freq" = sum(as.numeric(Freq)),
    "Freq_base" = sum(as.numeric(Freq_base)),
    "IPOPPAD" = mean(as.numeric(IPOPPAD)),
    "ICASOSPAD" = mean(as.numeric(ICASOSPAD)),
    "IOBSPAD_BASE" = mean(as.numeric(IOBSPAD_BASE)),
    "IOBSPAD" = mean(as.numeric(IOBSPAD)),
    "IPOPTOTPAD" = mean(as.numeric(IPOPTOTPAD), na.rm = TRUE),
    "ITXESPCPAD" = mean(as.numeric(ITXESPCPAD))
  ), by = "BR,Ano,Sexo,Faixas,NomeId,IdPad,Indicador"]


# base$Mun <- as.character(base$Mun)
# Cirs$Mun <- as.character(Cirs$Mun)
base$Mun <- as.integer(base$Mun)
Cirs$Mun <- as.integer(Cirs$Mun)

Estats <- merge(base, Cirs, by = "Mun")

# Correcao exclui ignorados parte 3 ---------------------------------------

Estats <- subset(Estats, substring(Estats$Cirs,1,4) != "9999" & !is.na(Estats$Mun))


names(Estats)[6] <- "Indicador"
Estats[, UF := as.integer(UF)]
Estats[, RG := as.integer(RG)]


rm(list = ls()[!ls() %in% c("Cgrbase",
                            "UFbase",
                            "RGbase",
                            "BRbase",
                            "Estats",
                            "escolhe_banco",
                            "escolhe_regiao")])


MULTI = 100000

Cgrbase[, "IEXP" := sum(ITXESPCPAD * POPULACAO, na.rm = TRUE), by = "Cgr,Ano,Indicador"]

Cgrbase <-
  merge(Cgrbase,
        Cgrbase[, list("FreqTOT" = sum(as.numeric(Freq), na.rm = TRUE)), by = "Cgr,Ano,Indicador"],
        by = c("Cgr", "Ano", "Indicador"),
        all.x = TRUE)

Cgrbase <-
  merge(Cgrbase,
        Cgrbase[, list("POPTOT" = sum(as.numeric(POPULACAO), na.rm = TRUE)), by = "Cgr,Ano,Indicador"],
        by = c("Cgr", "Ano", "Indicador"),
        all.x = TRUE)



Cgrbase <-
  merge(
    Cgrbase,
    Cgrbase[Ano == 2010, list("POP2010" = sum(as.numeric(POPULACAO), na.rm = TRUE)), by = "Sexo,Faixas,NomeId,IdPad,Indicador"],
    by = c("Sexo", "Faixas", "NomeId", "IdPad", "Indicador"),
    all.x = TRUE
  )


Cgrbase <-
  merge(Cgrbase,
        Cgrbase[Ano == 2010, list("POPTOT2010" = sum(as.numeric(POPULACAO), na.rm = TRUE)), by = "Indicador"],
        by = c("Indicador"),
        all.x = TRUE)


Cgrbase[, TXBRUTA := as.numeric(MULTI * FreqTOT / POPTOT)]

Cgrbase[, TXBRUTA_BASE := as.numeric(MULTI * Freq_base / POPTOT)]

Cgrbase[, TXBRUTA_BASE_BR2010 := as.numeric(MULTI * IOBSPAD_BASE / POPTOT2010)]

Cgrbase[, TXDPAD := sum(as.numeric(MULTI * (Freq / POPULACAO) * (POP2010 /POPTOT2010))), by = "Cgr,Ano,IdPad,Indicador"]

Cgrbase[, "ISIR" := FreqTOT / IEXP, by = "Cgr,Ano,Indicador"]

Cgrbase[, "TXIPAD" := as.numeric(MULTI * ISIR * (IOBSPAD_BASE / POPTOT2010)), by = "Cgr,Ano,Indicador"]

Cgrbase[FreqTOT <= 100, "INFSIR95I" := (qchisq(0.025, 2 * FreqTOT) / 2) /IEXP]

Cgrbase[FreqTOT <= 100, "SUPSIR95I" := (qchisq(0.975, 2 * (FreqTOT + 1)) /2) / IEXP]


Cgrbase[FreqTOT <= 100, "INFTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (qchisq(0.025, 2 *FreqTOT) / 2) / IEXP]

Cgrbase[FreqTOT <= 100, "SUPTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (qchisq(0.975, 2 *(FreqTOT + 1)) / 2) / IEXP]


Cgrbase[FreqTOT > 100, "INFSIR95I" := (1 - sqrt(FreqTOT)) ^ 2 / IEXP]

Cgrbase[FreqTOT > 100, "SUPSIR95I" := (1 + sqrt(FreqTOT + 1)) ^ 2 / IEXP]


Cgrbase[FreqTOT > 100, "INFTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (1 - sqrt(FreqTOT)) ^ 2 / IEXP]

Cgrbase[FreqTOT > 100, "SUPTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (1 + sqrt(FreqTOT + 1)) ^ 2 / IEXP]

#Cgrbase <- Cgrbase[Cgr != 99999]
Cgrbase <- subset(Cgrbase,substring(Cgrbase$Cgr,1,4) != "9999")
#Cgrbase <- Cgrbase[Cgr != 99999 & Cgr != 999999]

#BRfinal<-Cgrbase[,list("TXBRUTA" = mean(TXBRUTA),"TXDPAD" = mean(TXDPAD)),by = "Cgr,Ano,Indicador"]
Cgrfinal <-
  Cgrbase[, list(
    "FreqTOT" = mean(FreqTOT),
    "FreqTOT_base" = mean(Freq_base),
    "FreqTOT_base_BR2010" = mean(IOBSPAD_BASE),
    "POPTOT" = mean(POPTOT),
    "POPTOT_2010" = mean(POPTOT2010),
    "IEXP" = mean(IEXP),
    "TXBRUTA" = mean(TXBRUTA),
    "TXBRUTA_BASE" = mean(TXBRUTA_BASE),
    "TXBRUTA_BASE_BR2010" = mean(TXBRUTA_BASE_BR2010),
    "TXDPAD" = mean(TXDPAD),
    "TXIPAD" = mean(TXIPAD),
    "INFTX95I" = mean(INFTX95I),
    "SUPTX95I" = mean(SUPTX95I),
    "ISIR" = mean(ISIR),
    "INFSIR95I" = mean(INFSIR95I),
    "SUPSIR95I" = mean(SUPSIR95I)
  ), by = "Cgr,Ano,Indicador"]



EstatsCgr <-
  Estats[, list("MediaT" = sum(Idade * Freq, na.rm = TRUE) / sum(Freq, na.rm = TRUE)), by = "Ano,Indicador,Cgr"]
EstatsCgr <-
  merge(EstatsCgr,
        Estats[Sexo == 1, list("MediaH" = sum(Idade * Freq, na.rm = TRUE) / sum(Freq, na.rm = TRUE)), by = "Ano,Indicador,Cgr,Sexo"][, Sexo :=NULL],
        by = c("Ano", "Indicador", "Cgr"),
        all.x = TRUE)
EstatsCgr <-
  merge(EstatsCgr,
        Estats[Sexo == 2, list("MediaM" = sum(Idade * Freq, na.rm = TRUE) / sum(Freq, na.rm = TRUE)), by = "Ano,Indicador,Cgr,Sexo"][, Sexo :=NULL],
        by = c("Ano", "Indicador", "Cgr"),
        all.x = TRUE)
Cgrfinal <-
  merge(Cgrfinal,
        EstatsCgr,
        by = c("Ano", "Indicador", "Cgr"),
        all.x = TRUE)
Cgrfinal[, "Nome" := substring(Indicador, 1, 3)]


#Exceção A51 - ou - A05_N_SIH
Cgrfinal$TXBRUTA[Cgrfinal$Indicador == 'A05_N_SIH'] = Cgrfinal$TXBRUTA[Cgrfinal$Indicador == 'A05_N_SIH'] /100
Cgrfinal$TXBRUTA_BASE[Cgrfinal$Indicador == 'A05_N_SIH'] = Cgrfinal$TXBRUTA_BASE[Cgrfinal$Indicador == 'A05_N_SIH'] /100
Cgrfinal$TXBRUTA_BASE_BR2010[Cgrfinal$Indicador == 'A05_N_SIH'] = Cgrfinal$TXBRUTA_BASE_BR2010[Cgrfinal$Indicador == 'A05_N_SIH'] /100

Cgrfinal$TXDPAD[Cgrfinal$Indicador == 'A05_N_SIH'] = Cgrfinal$TXDPAD[Cgrfinal$Indicador == 'A05_N_SIH'] /100

Cgrfinal$TXIPAD[Cgrfinal$Indicador == 'A05_N_SIH'] = Cgrfinal$TXIPAD[Cgrfinal$Indicador == 'A05_N_SIH'] /100

Cgrfinal$INFTX95I[Cgrfinal$Indicador == 'A05_N_SIH'] = Cgrfinal$INFTX95I[Cgrfinal$Indicador == 'A05_N_SIH'] /100

Cgrfinal$SUPTX95I[Cgrfinal$Indicador == 'A05_N_SIH'] = Cgrfinal$SUPTX95I[Cgrfinal$Indicador == 'A05_N_SIH'] /100







#Exceção A51 - ou - A05_N_SIH
Cgrfinal$TXBRUTA[Cgrfinal$Indicador == 'A05_N2_SIH'] = Cgrfinal$TXBRUTA[Cgrfinal$Indicador == 'A05_N2_SIH'] /100
Cgrfinal$TXBRUTA_BASE[Cgrfinal$Indicador == 'A05_N2_SIH'] = Cgrfinal$TXBRUTA_BASE[Cgrfinal$Indicador == 'A05_N2_SIH'] /100
Cgrfinal$TXBRUTA_BASE_BR2010[Cgrfinal$Indicador == 'A05_N2_SIH'] = Cgrfinal$TXBRUTA_BASE_BR2010[Cgrfinal$Indicador == 'A05_N2_SIH'] /100

Cgrfinal$TXDPAD[Cgrfinal$Indicador == 'A05_N2_SIH'] = Cgrfinal$TXDPAD[Cgrfinal$Indicador == 'A05_N2_SIH'] /100

Cgrfinal$TXIPAD[Cgrfinal$Indicador == 'A05_N2_SIH'] = Cgrfinal$TXIPAD[Cgrfinal$Indicador == 'A05_N2_SIH'] /100

Cgrfinal$INFTX95I[Cgrfinal$Indicador == 'A05_N2_SIH'] = Cgrfinal$INFTX95I[Cgrfinal$Indicador == 'A05_N2_SIH'] /100

Cgrfinal$SUPTX95I[Cgrfinal$Indicador == 'A05_N2_SIH'] = Cgrfinal$SUPTX95I[Cgrfinal$Indicador == 'A05_N2_SIH'] /100






#Exceção A51 - ou - A05_N_SIH
Cgrfinal$TXBRUTA[Cgrfinal$Indicador == 'A05_N3_SIH'] = Cgrfinal$TXBRUTA[Cgrfinal$Indicador == 'A05_N3_SIH'] /100
Cgrfinal$TXBRUTA_BASE[Cgrfinal$Indicador == 'A05_N3_SIH'] = Cgrfinal$TXBRUTA_BASE[Cgrfinal$Indicador == 'A05_N3_SIH'] /100
Cgrfinal$TXBRUTA_BASE_BR2010[Cgrfinal$Indicador == 'A05_N3_SIH'] = Cgrfinal$TXBRUTA_BASE_BR2010[Cgrfinal$Indicador == 'A05_N3_SIH'] /100

Cgrfinal$TXDPAD[Cgrfinal$Indicador == 'A05_N3_SIH'] = Cgrfinal$TXDPAD[Cgrfinal$Indicador == 'A05_N3_SIH'] /100

Cgrfinal$TXIPAD[Cgrfinal$Indicador == 'A05_N3_SIH'] = Cgrfinal$TXIPAD[Cgrfinal$Indicador == 'A05_N3_SIH'] /100

Cgrfinal$INFTX95I[Cgrfinal$Indicador == 'A05_N3_SIH'] = Cgrfinal$INFTX95I[Cgrfinal$Indicador == 'A05_N3_SIH'] /100

Cgrfinal$SUPTX95I[Cgrfinal$Indicador == 'A05_N3_SIH'] = Cgrfinal$SUPTX95I[Cgrfinal$Indicador == 'A05_N3_SIH'] /100





#Cgrfinal <- subset(Cgrfinal, Cgrfinal$Nome == "M16" |Cgrfinal$Nome == "M26" | Cgrfinal$Nome == "M29"| Cgrfinal$Nome == "M32"| Cgrfinal$Nome == "M32"| Cgrfinal$Nome == "M35"| Cgrfinal$Nome == "M23")

#escolhe_regiao <- "municipio"
#escolhe_regiao <- "regiao_saude"
#escolhe_regiao <- "macrorregiao"


if (escolhe_banco == "SIM" & escolhe_regiao == "macrorregiao") {
  write.csv2(
    Cgrfinal,
    "./Resultados_PAD/SIM_MR_Cgr.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}

if (escolhe_banco == "SIM" & escolhe_regiao == "regiao_saude") {
  write.csv2(
    Cgrfinal,
    "./Resultados_PAD/SIM_RS_Cgr.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}

if (escolhe_banco == "SIM" & escolhe_regiao == "municipio") {
  write.csv2(
    Cgrfinal,
    "./Resultados_PAD/SIM_MUN_Cgr.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}

if (escolhe_banco == "SIH"& escolhe_regiao == "macrorregiao") {
  write.csv2(
    Cgrfinal,
    "./Resultados_PAD/SIH_MR_Cgr.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}

if (escolhe_banco == "SIH"& escolhe_regiao == "regiao_saude") {
  write.csv2(
    Cgrfinal,
    "./Resultados_PAD/SIH_RS_Cgr.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}

if (escolhe_banco == "SIH"& escolhe_regiao == "municipio") {
  write.csv2(
    Cgrfinal,
    "./Resultados_PAD/SIH_MUN_Cgr.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}

rm(Cgrfinal)
rm(Cgrbase)




UFbase[, "IEXP" := sum(ITXESPCPAD * POPULACAO, na.rm = TRUE), by = "UF,Ano,Indicador"]

UFbase <-
  merge(UFbase,
        UFbase[, list("FreqTOT" = sum(as.numeric(Freq), na.rm = TRUE)), by = "UF,Ano,Indicador"],
        by = c("UF", "Ano", "Indicador"),
        all.x = TRUE)

UFbase <-
  merge(UFbase,
        UFbase[, list("POPTOT" = sum(as.numeric(POPULACAO), na.rm = TRUE)), by = "UF,Ano,Indicador"],
        by = c("UF", "Ano", "Indicador"),
        all.x = TRUE)



UFbase <-
  merge(
    UFbase,
    UFbase[Ano == 2010, list("POP2010" = sum(as.numeric(POPULACAO), na.rm = TRUE)), by = "Sexo,Faixas,NomeId,IdPad,Indicador"],
    by = c("Sexo", "Faixas", "NomeId", "IdPad", "Indicador"),
    all.x = TRUE
  )


UFbase <-
  merge(UFbase, UFbase[Ano == 2010, list("POPTOT2010" = sum(as.numeric(POPULACAO), na.rm = TRUE)), by = "Indicador"], by = c("Indicador"), all.x = TRUE)


UFbase[, TXBRUTA := as.numeric(MULTI * FreqTOT / POPTOT)]

UFbase[, TXBRUTA_BASE := as.numeric(MULTI * Freq_base / POPTOT)]

UFbase[, TXBRUTA_BASE_BR2010 := as.numeric(MULTI * IOBSPAD_BASE / POPTOT2010)]

UFbase[, TXDPAD := sum(as.numeric(MULTI * (Freq / POPULACAO) * (POP2010 /POPTOT2010))), by = "UF,Ano,IdPad,Indicador"]

UFbase[, "ISIR" := FreqTOT / IEXP, by = "UF,Ano,Indicador"]

UFbase[, "TXIPAD" := as.numeric(MULTI * ISIR * (IOBSPAD_BASE / POPTOT2010)), by = "UF,Ano,Indicador"]

UFbase[FreqTOT <= 100, "INFSIR95I" := (qchisq(0.025, 2 * FreqTOT) / 2) /IEXP]

UFbase[FreqTOT <= 100, "SUPSIR95I" := (qchisq(0.975, 2 * (FreqTOT + 1)) /2) / IEXP]


UFbase[FreqTOT <= 100, "INFTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (qchisq(0.025, 2 *FreqTOT) / 2) / IEXP]

UFbase[FreqTOT <= 100, "SUPTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (qchisq(0.975, 2 *(FreqTOT + 1)) / 2) / IEXP]


UFbase[FreqTOT > 100, "INFSIR95I" := (1 - sqrt(FreqTOT)) ^ 2 / IEXP]

UFbase[FreqTOT > 100, "SUPSIR95I" := (1 + sqrt(FreqTOT + 1)) ^ 2 / IEXP]


UFbase[FreqTOT > 100, "INFTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (1 - sqrt(FreqTOT)) ^ 2 / IEXP]

UFbase[FreqTOT > 100, "SUPTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (1 + sqrt(FreqTOT + 1)) ^ 2 / IEXP]


#UFfinal<-UFbase[,list("TXBRUTA" = mean(TXBRUTA),"TXDPAD" = mean(TXDPAD)),by = "UF,Ano,Indicador"]
UFfinal <- UFbase[, list(
  "FreqTOT" = mean(FreqTOT),
  "FreqTOT_base" = mean(Freq_base),
  "FreqTOT_base_BR2010" = mean(IOBSPAD_BASE),
  "POPTOT" = mean(POPTOT),
  "POPTOT_2010" = mean(POPTOT2010),
  "IEXP" = mean(IEXP),
  "TXBRUTA" = mean(TXBRUTA),
  "TXBRUTA_BASE" = mean(TXBRUTA_BASE),
  "TXBRUTA_BASE_BR2010" = mean(TXBRUTA_BASE_BR2010),
  "TXDPAD" = mean(TXDPAD),
  "TXIPAD" = mean(TXIPAD),
  "INFTX95I" = mean(INFTX95I),
  "SUPTX95I" = mean(SUPTX95I),
  "ISIR" = mean(ISIR),
  "INFSIR95I" = mean(INFSIR95I),
  "SUPSIR95I" = mean(SUPSIR95I)
), by = "UF,Ano,Indicador"]



EstatsUF <-
  Estats[, list("MediaT" = sum(Idade * Freq, na.rm = TRUE) / sum(Freq, na.rm = TRUE)), by = "Ano,Indicador,UF"]
EstatsUF <-
  merge(EstatsUF,
        Estats[Sexo == 1, list("MediaH" = sum(Idade * Freq, na.rm = TRUE) / sum(Freq, na.rm = TRUE)), by = "Ano,Indicador,UF,Sexo"][, Sexo :=NULL],
        by = c("Ano", "Indicador", "UF"),
        all.x = TRUE)
EstatsUF <-
  merge(EstatsUF,
        Estats[Sexo == 2, list("MediaM" = sum(Idade * Freq, na.rm = TRUE) / sum(Freq, na.rm = TRUE)), by = "Ano,Indicador,UF,Sexo"][, Sexo :=NULL],
        by = c("Ano", "Indicador", "UF"),
        all.x = TRUE)
UFfinal <-
  merge(UFfinal,
        EstatsUF,
        by = c("Ano", "Indicador", "UF"),
        all.x = TRUE)
UFfinal[, "Nome" := substring(Indicador, 1, 3)]
UFfinal <- UFfinal[UF >= 11 & UF <= 53, ]

#Exceção A51 - ou - A05_N_SIH
UFfinal$TXBRUTA[UFfinal$Indicador == 'A05_N_SIH'] = UFfinal$TXBRUTA[UFfinal$Indicador == 'A05_N_SIH'] /100
UFfinal$TXBRUTA_BASE[UFfinal$Indicador == 'A05_N_SIH'] = UFfinal$TXBRUTA_BASE[UFfinal$Indicador == 'A05_N_SIH'] /100
UFfinal$TXBRUTA_BASE_BR2010[UFfinal$Indicador == 'A05_N_SIH'] = UFfinal$TXBRUTA_BASE_BR2010[UFfinal$Indicador == 'A05_N_SIH'] /100
UFfinal$TXDPAD[UFfinal$Indicador == 'A05_N_SIH'] = UFfinal$TXDPAD[UFfinal$Indicador == 'A05_N_SIH'] /100
UFfinal$TXIPAD[UFfinal$Indicador == 'A05_N_SIH'] = UFfinal$TXIPAD[UFfinal$Indicador == 'A05_N_SIH'] /100
UFfinal$INFTX95I[UFfinal$Indicador == 'A05_N_SIH'] = UFfinal$INFTX95I[UFfinal$Indicador == 'A05_N_SIH'] /100
UFfinal$SUPTX95I[UFfinal$Indicador == 'A05_N_SIH'] = UFfinal$SUPTX95I[UFfinal$Indicador == 'A05_N_SIH'] /100



#Exceção A51 - ou - A05_N_SIH
UFfinal$TXBRUTA[UFfinal$Indicador == 'A05_N2_SIH'] = UFfinal$TXBRUTA[UFfinal$Indicador == 'A05_N2_SIH'] /100
UFfinal$TXBRUTA_BASE[UFfinal$Indicador == 'A05_N2_SIH'] = UFfinal$TXBRUTA_BASE[UFfinal$Indicador == 'A05_N2_SIH'] /100
UFfinal$TXBRUTA_BASE_BR2010[UFfinal$Indicador == 'A05_N2_SIH'] = UFfinal$TXBRUTA_BASE_BR2010[UFfinal$Indicador == 'A05_N2_SIH'] /100
UFfinal$TXDPAD[UFfinal$Indicador == 'A05_N2_SIH'] = UFfinal$TXDPAD[UFfinal$Indicador == 'A05_N2_SIH'] /100
UFfinal$TXIPAD[UFfinal$Indicador == 'A05_N2_SIH'] = UFfinal$TXIPAD[UFfinal$Indicador == 'A05_N2_SIH'] /100
UFfinal$INFTX95I[UFfinal$Indicador == 'A05_N2_SIH'] = UFfinal$INFTX95I[UFfinal$Indicador == 'A05_N2_SIH'] /100
UFfinal$SUPTX95I[UFfinal$Indicador == 'A05_N2_SIH'] = UFfinal$SUPTX95I[UFfinal$Indicador == 'A05_N2_SIH'] /100


#Exceção A51 - ou - A05_N_SIH
UFfinal$TXBRUTA[UFfinal$Indicador == 'A05_N3_SIH'] = UFfinal$TXBRUTA[UFfinal$Indicador == 'A05_N3_SIH'] /100
UFfinal$TXBRUTA_BASE[UFfinal$Indicador == 'A05_N3_SIH'] = UFfinal$TXBRUTA_BASE[UFfinal$Indicador == 'A05_N3_SIH'] /100
UFfinal$TXBRUTA_BASE_BR2010[UFfinal$Indicador == 'A05_N3_SIH'] = UFfinal$TXBRUTA_BASE_BR2010[UFfinal$Indicador == 'A05_N3_SIH'] /100
UFfinal$TXDPAD[UFfinal$Indicador == 'A05_N3_SIH'] = UFfinal$TXDPAD[UFfinal$Indicador == 'A05_N3_SIH'] /100
UFfinal$TXIPAD[UFfinal$Indicador == 'A05_N3_SIH'] = UFfinal$TXIPAD[UFfinal$Indicador == 'A05_N3_SIH'] /100
UFfinal$INFTX95I[UFfinal$Indicador == 'A05_N3_SIH'] = UFfinal$INFTX95I[UFfinal$Indicador == 'A05_N3_SIH'] /100
UFfinal$SUPTX95I[UFfinal$Indicador == 'A05_N3_SIH'] = UFfinal$SUPTX95I[UFfinal$Indicador == 'A05_N3_SIH'] /100



#UFfinal <- subset(UFfinal, UFfinal$Nome == "M16" |UFfinal$Nome == "M26" | UFfinal$Nome == "M29"| UFfinal$Nome == "M32"| UFfinal$Nome == "M32"| UFfinal$Nome == "M35"| UFfinal$Nome == "M23")

#escolhe_regiao <- "municipio"
#escolhe_regiao <- "regiao_saude"
#escolhe_regiao <- "macrorregiao"


if (escolhe_banco == "SIM" & escolhe_regiao == "macrorregiao") {
  write.csv2(
    UFfinal,
    "./Resultados_PAD/SIM_MR_UF.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}
if (escolhe_banco == "SIM" & escolhe_regiao == "regiao_saude") {
  write.csv2(
    UFfinal,
    "./Resultados_PAD/SIM_RS_UF.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}
if (escolhe_banco == "SIM" & escolhe_regiao == "municipio") {
  write.csv2(
    UFfinal,
    "./Resultados_PAD/SIM_MUN_UF.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}
if (escolhe_banco == "SIH"& escolhe_regiao == "macrorregiao") {
  write.csv2(
    UFfinal,
    "./Resultados_PAD/SIH_MR_UF.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}

if (escolhe_banco == "SIH"& escolhe_regiao == "regiao_saude") {
  write.csv2(
    UFfinal,
    "./Resultados_PAD/SIH_RS_UF.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}

if (escolhe_banco == "SIH"& escolhe_regiao == "municipio") {
  write.csv2(
    UFfinal,
    "./Resultados_PAD/SIH_MUN_UF.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}


rm(UFbase)
rm(UFfinal)


RGbase[, "IEXP" := sum(ITXESPCPAD * POPULACAO, na.rm = TRUE), by = "RG,Ano,Indicador"]

RGbase <-
  merge(RGbase,
        RGbase[, list("FreqTOT" = sum(as.numeric(Freq), na.rm = TRUE)), by = "RG,Ano,Indicador"],
        by = c("RG", "Ano", "Indicador"),
        all.x = TRUE)

RGbase <-
  merge(RGbase,
        RGbase[, list("POPTOT" = sum(as.numeric(POPULACAO), na.rm = TRUE)), by = "RG,Ano,Indicador"],
        by = c("RG", "Ano", "Indicador"),
        all.x = TRUE)



RGbase <-
  merge(
    RGbase,
    RGbase[Ano == 2010, list("POP2010" = sum(as.numeric(POPULACAO), na.rm = TRUE)), by = "Sexo,Faixas,NomeId,IdPad,Indicador"],
    by = c("Sexo", "Faixas", "NomeId", "IdPad", "Indicador"),
    all.x = TRUE
  )


RGbase <-
  merge(RGbase, RGbase[Ano == 2010, list("POPTOT2010" = sum(as.numeric(POPULACAO), na.rm = TRUE)), by = "Indicador"], by = c("Indicador"), all.x = TRUE)


RGbase[, TXBRUTA := as.numeric(MULTI * FreqTOT / POPTOT)]

RGbase[, TXBRUTA_BASE := as.numeric(MULTI * Freq_base / POPTOT)]

RGbase[, TXBRUTA_BASE_BR2010 := as.numeric(MULTI * IOBSPAD_BASE / POPTOT2010)]

RGbase[, TXDPAD := sum(as.numeric(MULTI * (Freq / POPULACAO) * (POP2010 /POPTOT2010))), by = "RG,Ano,IdPad,Indicador"]

RGbase[, "ISIR" := FreqTOT / IEXP, by = "RG,Ano,Indicador"]

RGbase[, "TXIPAD" := as.numeric(MULTI * ISIR * (IOBSPAD_BASE / POPTOT2010)), by = "RG,Ano,Indicador"]

RGbase[FreqTOT <= 100, "INFSIR95I" := (qchisq(0.025, 2 * FreqTOT) / 2) /IEXP]

RGbase[FreqTOT <= 100, "SUPSIR95I" := (qchisq(0.975, 2 * (FreqTOT + 1)) /2) / IEXP]


RGbase[FreqTOT <= 100, "INFTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (qchisq(0.025, 2 *FreqTOT) / 2) / IEXP]

RGbase[FreqTOT <= 100, "SUPTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (qchisq(0.975, 2 * (FreqTOT + 1)) / 2) / IEXP]


RGbase[FreqTOT > 100, "INFSIR95I" := (1 - sqrt(FreqTOT)) ^ 2 / IEXP]

RGbase[FreqTOT > 100, "SUPSIR95I" := (1 + sqrt(FreqTOT + 1)) ^ 2 / IEXP]


RGbase[FreqTOT > 100, "INFTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (1 - sqrt(FreqTOT)) ^ 2 / IEXP]

RGbase[FreqTOT > 100, "SUPTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (1 + sqrt(FreqTOT + 1)) ^ 2 / IEXP]


#RGfinal<-RGbase[,list("TXBRUTA" = mean(TXBRUTA),"TXDPAD" = mean(TXDPAD)),by = "RG,Ano,Indicador"]
RGfinal <- RGbase[, list(
  "FreqTOT" = mean(FreqTOT),
  "FreqTOT_base" = mean(Freq_base),
  "FreqTOT_base_BR2010" = mean(IOBSPAD_BASE),
  "POPTOT" = mean(POPTOT),
  "POPTOT_2010" = mean(POPTOT2010),
  "IEXP" = mean(IEXP),
  "TXBRUTA" = mean(TXBRUTA),
  "TXBRUTA_BASE" = mean(TXBRUTA_BASE),
  "TXBRUTA_BASE_BR2010" = mean(TXBRUTA_BASE_BR2010),
  "TXDPAD" = mean(TXDPAD),
  "TXIPAD" = mean(TXIPAD),
  "INFTX95I" = mean(INFTX95I),
  "SUPTX95I" = mean(SUPTX95I),
  "ISIR" = mean(ISIR),
  "INFSIR95I" = mean(INFSIR95I),
  "SUPSIR95I" = mean(SUPSIR95I)
), by = "RG,Ano,Indicador"]


EstatsRG <-
  Estats[, list("MediaT" = sum(Idade * Freq, na.rm = TRUE) / sum(Freq, na.rm = TRUE)), by = "Ano,Indicador,RG"]
EstatsRG <-
  merge(EstatsRG,
        Estats[Sexo == 1, list("MediaH" = sum(Idade * Freq, na.rm = TRUE) / sum(Freq, na.rm = TRUE)), by = "Ano,Indicador,RG,Sexo"][, Sexo :=NULL],
        by = c("Ano", "Indicador", "RG"),
        all.x = TRUE)
EstatsRG <-
  merge(EstatsRG,
        Estats[Sexo == 2, list("MediaM" = sum(Idade * Freq, na.rm = TRUE) / sum(Freq, na.rm = TRUE)), by = "Ano,Indicador,RG,Sexo"][, Sexo :=NULL],
        by = c("Ano", "Indicador", "RG"),
        all.x = TRUE)
RGfinal <-
  merge(RGfinal,
        EstatsRG,
        by = c("Ano", "Indicador", "RG"),
        all.x = TRUE)
RGfinal[, "Nome" := substring(Indicador, 1, 3)]
RGfinal <- RGfinal[RG >= 1 & RG <= 5, ]


#Exceção A51 - ou - A05_N_SIH
RGfinal$TXBRUTA[RGfinal$Indicador == 'A05_N_SIH'] = RGfinal$TXBRUTA[RGfinal$Indicador == 'A05_N_SIH'] /100
RGfinal$TXBRUTA_BASE[RGfinal$Indicador == 'A05_N_SIH'] = RGfinal$TXBRUTA_BASE[RGfinal$Indicador == 'A05_N_SIH'] /100
RGfinal$TXBRUTA_BASE_BR2010[RGfinal$Indicador == 'A05_N_SIH'] = RGfinal$TXBRUTA_BASE_BR2010[RGfinal$Indicador == 'A05_N_SIH'] /100
RGfinal$TXDPAD[RGfinal$Indicador == 'A05_N_SIH'] = RGfinal$TXDPAD[RGfinal$Indicador == 'A05_N_SIH'] /100
RGfinal$TXIPAD[RGfinal$Indicador == 'A05_N_SIH'] = RGfinal$TXIPAD[RGfinal$Indicador == 'A05_N_SIH'] /100
RGfinal$INFTX95I[RGfinal$Indicador == 'A05_N_SIH'] = RGfinal$INFTX95I[RGfinal$Indicador == 'A05_N_SIH'] /100
RGfinal$SUPTX95I[RGfinal$Indicador == 'A05_N_SIH'] = RGfinal$SUPTX95I[RGfinal$Indicador == 'A05_N_SIH'] /100




#RGfinal <- subset(RGfinal, RGfinal$Nome == "M16" |RGfinal$Nome == "M26" | RGfinal$Nome == "M29"| RGfinal$Nome == "M32"| RGfinal$Nome == "M32"| RGfinal$Nome == "M35"| RGfinal$Nome == "M23")



#escolhe_regiao <- "municipio"
#escolhe_regiao <- "regiao_saude"
#escolhe_regiao <- "macrorregiao"


if (escolhe_banco == "SIM" & escolhe_regiao == "macrorregiao") {
  write.csv2(
    RGfinal,
    "./Resultados_PAD/SIM_MR_RG.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}
if (escolhe_banco == "SIM" & escolhe_regiao == "regiao_saude") {
  write.csv2(
    RGfinal,
    "./Resultados_PAD/SIM_RS_RG.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}
if (escolhe_banco == "SIM" & escolhe_regiao == "municipio") {
  write.csv2(
    RGfinal,
    "./Resultados_PAD/SIM_MUN_RG.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}
if (escolhe_banco == "SIH"& escolhe_regiao == "macrorregiao") {
  write.csv2(
    RGfinal,
    "./Resultados_PAD/SIH_MR_RG.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}
if (escolhe_banco == "SIH"& escolhe_regiao == "regiao_saude") {
  write.csv2(
    RGfinal,
    "./Resultados_PAD/SIH_RS_RG.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}

if (escolhe_banco == "SIH"& escolhe_regiao == "municipio") {
  write.csv2(
    RGfinal,
    "./Resultados_PAD/SIH_MUN_RG.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}


rm(RGfinal)
rm(RGbase)




BRbase[, "IEXP" := sum(ITXESPCPAD * POPULACAO, na.rm = TRUE), by = "BR,Ano,Indicador"]

BRbase <-
  merge(BRbase,
        BRbase[, list("FreqTOT" = sum(as.numeric(Freq), na.rm = TRUE)), by = "BR,Ano,Indicador"],
        by = c("BR", "Ano", "Indicador"),
        all.x = TRUE)

BRbase <-
  merge(BRbase,
        BRbase[, list("POPTOT" = sum(as.numeric(POPULACAO), na.rm = TRUE)), by = "BR,Ano,Indicador"],
        by = c("BR", "Ano", "Indicador"),
        all.x = TRUE)


BRbase <-
  merge(
    BRbase,
    BRbase[Ano == 2010, list("POP2010" = sum(as.numeric(POPULACAO), na.rm = TRUE)), by = "Sexo,Faixas,NomeId,IdPad,Indicador"],
    by = c("Sexo", "Faixas", "NomeId", "IdPad", "Indicador"),
    all.x = TRUE
  )


BRbase <-
  merge(BRbase, BRbase[Ano == 2010, list("POPTOT2010" = sum(as.numeric(POPULACAO), na.rm = TRUE)), by = "Indicador"], by = c("Indicador"), all.x = TRUE)


BRbase[, TXBRUTA := as.numeric(MULTI * FreqTOT / POPTOT)]

BRbase[, TXBRUTA_BASE := as.numeric(MULTI * Freq_base / POPTOT)]

BRbase[, TXBRUTA_BASE_BR2010 := as.numeric(MULTI * IOBSPAD_BASE / POPTOT2010)]

BRbase[, TXDPAD := sum(as.numeric(MULTI * (Freq / POPULACAO) * (POP2010 /POPTOT2010))), by = "BR,Ano,IdPad,Indicador"]

BRbase[, "ISIR" := FreqTOT / IEXP, by = "Indicador,BR,Ano"]

BRbase[, "TXIPAD" := as.numeric(MULTI * ISIR * (IOBSPAD_BASE / POPTOT2010)), by = "BR,Ano,Indicador"]

BRbase[FreqTOT <= 100, "INFSIR95I" := (qchisq(0.025, 2 * FreqTOT) / 2) /IEXP]

BRbase[FreqTOT <= 100, "SUPSIR95I" := (qchisq(0.975, 2 * (FreqTOT + 1)) /2) / IEXP]


BRbase[FreqTOT <= 100, "INFTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (qchisq(0.025, 2 *FreqTOT) / 2) / IEXP]

BRbase[FreqTOT <= 100, "SUPTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (qchisq(0.975, 2 * (FreqTOT + 1)) / 2) / IEXP]


BRbase[FreqTOT > 100, "INFSIR95I" := (1 - sqrt(FreqTOT)) ^ 2 / IEXP]

BRbase[FreqTOT > 100, "SUPSIR95I" := (1 + sqrt(FreqTOT + 1)) ^ 2 / IEXP]


BRbase[FreqTOT > 100, "INFTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (1 - sqrt(FreqTOT)) ^ 2 / IEXP]

BRbase[FreqTOT > 100, "SUPTX95I" := MULTI * (IOBSPAD_BASE / POPTOT2010) * (1 + sqrt(FreqTOT + 1)) ^ 2 / IEXP]


#BRfinal<-BRbase[,list("TXBRUTA" = mean(TXBRUTA),"TXDPAD" = mean(TXDPAD)),by = "BR,Ano,Indicador"]
BRfinal <- BRbase[, list(
  "FreqTOT" = mean(FreqTOT),
  "FreqTOT_base" = mean(Freq_base),
  "FreqTOT_base_BR2010" = mean(IOBSPAD_BASE),
  "POPTOT" = mean(POPTOT),
  "POPTOT_2010" = mean(POPTOT2010),
  "IEXP" = mean(IEXP),
  "TXBRUTA" = mean(TXBRUTA),
  "TXBRUTA_BASE" = mean(TXBRUTA_BASE),
  "TXBRUTA_BASE_BR2010" = mean(TXBRUTA_BASE_BR2010),
  "TXDPAD" = mean(TXDPAD),
  "TXIPAD" = mean(TXIPAD),
  "INFTX95I" = mean(INFTX95I),
  "SUPTX95I" = mean(SUPTX95I),
  "ISIR" = mean(ISIR),
  "INFSIR95I" = mean(INFSIR95I),
  "SUPSIR95I" = mean(SUPSIR95I)
), by = "BR,Ano,Indicador"]


EstatsBR <-
  Estats[, list("MediaT" = sum(Idade * Freq, na.rm = TRUE) / sum(Freq, na.rm = TRUE)), by = "Ano,Indicador,BR"]
EstatsBR <-
  merge(EstatsBR,
        Estats[Sexo == 1, list("MediaH" = sum(Idade * Freq, na.rm = TRUE) / sum(Freq, na.rm = TRUE)), by = "Ano,Indicador,BR,Sexo"][, Sexo :=NULL],
        by = c("Ano", "Indicador", "BR"),
        all.x = TRUE)
EstatsBR <-
  merge(EstatsBR,
        Estats[Sexo == 2, list("MediaM" = sum(Idade * Freq, na.rm = TRUE) / sum(Freq, na.rm = TRUE)), by = "Ano,Indicador,BR,Sexo"][, Sexo :=NULL],
        by = c("Ano", "Indicador", "BR"),
        all.x = TRUE)
BRfinal <-
  merge(BRfinal,
        EstatsBR,
        by = c("Ano", "Indicador", "BR"),
        all.x = TRUE)
BRfinal[, "Nome" := substring(Indicador, 1, 3)]
BRfinal <- BRfinal[BR == 555, ]

#Excecao A51 - ou - A05_N_SIH
BRfinal$TXBRUTA[BRfinal$Indicador == 'A05_N_SIH'] = BRfinal$TXBRUTA[BRfinal$Indicador == 'A05_N_SIH'] /100
BRfinal$TXBRUTA_BASE[BRfinal$Indicador == 'A05_N_SIH'] = BRfinal$TXBRUTA_BASE[BRfinal$Indicador == 'A05_N_SIH'] /100
BRfinal$TXBRUTA_BASE_BR2010[BRfinal$Indicador == 'A05_N_SIH'] = BRfinal$TXBRUTA_BASE_BR2010[BRfinal$Indicador == 'A05_N_SIH'] /100
BRfinal$TXDPAD[BRfinal$Indicador == 'A05_N_SIH'] = BRfinal$TXDPAD[BRfinal$Indicador == 'A05_N_SIH'] /100
BRfinal$TXIPAD[BRfinal$Indicador == 'A05_N_SIH'] = BRfinal$TXIPAD[BRfinal$Indicador == 'A05_N_SIH'] /100
BRfinal$INFTX95I[BRfinal$Indicador == 'A05_N_SIH'] = BRfinal$INFTX95I[BRfinal$Indicador == 'A05_N_SIH'] /100
BRfinal$SUPTX95I[BRfinal$Indicador == 'A05_N_SIH'] = BRfinal$SUPTX95I[BRfinal$Indicador == 'A05_N_SIH'] /100



#Excecao A51 - ou - A05_N_SIH
BRfinal$TXBRUTA[BRfinal$Indicador == 'A05_N2_SIH'] = BRfinal$TXBRUTA[BRfinal$Indicador == 'A05_N2_SIH'] /100
BRfinal$TXBRUTA_BASE[BRfinal$Indicador == 'A05_N2_SIH'] = BRfinal$TXBRUTA_BASE[BRfinal$Indicador == 'A05_N2_SIH'] /100
BRfinal$TXBRUTA_BASE_BR2010[BRfinal$Indicador == 'A05_N2_SIH'] = BRfinal$TXBRUTA_BASE_BR2010[BRfinal$Indicador == 'A05_N2_SIH'] /100
BRfinal$TXDPAD[BRfinal$Indicador == 'A05_N2_SIH'] = BRfinal$TXDPAD[BRfinal$Indicador == 'A05_N2_SIH'] /100
BRfinal$TXIPAD[BRfinal$Indicador == 'A05_N2_SIH'] = BRfinal$TXIPAD[BRfinal$Indicador == 'A05_N2_SIH'] /100
BRfinal$INFTX95I[BRfinal$Indicador == 'A05_N2_SIH'] = BRfinal$INFTX95I[BRfinal$Indicador == 'A05_N2_SIH'] /100
BRfinal$SUPTX95I[BRfinal$Indicador == 'A05_N2_SIH'] = BRfinal$SUPTX95I[BRfinal$Indicador == 'A05_N2_SIH'] /100




#Excecao A51 - ou - A05_N_SIH
BRfinal$TXBRUTA[BRfinal$Indicador == 'A05_N3_SIH'] = BRfinal$TXBRUTA[BRfinal$Indicador == 'A05_N3_SIH'] /100
BRfinal$TXBRUTA_BASE[BRfinal$Indicador == 'A05_N3_SIH'] = BRfinal$TXBRUTA_BASE[BRfinal$Indicador == 'A05_N3_SIH'] /100
BRfinal$TXBRUTA_BASE_BR2010[BRfinal$Indicador == 'A05_N3_SIH'] = BRfinal$TXBRUTA_BASE_BR2010[BRfinal$Indicador == 'A05_N3_SIH'] /100
BRfinal$TXDPAD[BRfinal$Indicador == 'A05_N3_SIH'] = BRfinal$TXDPAD[BRfinal$Indicador == 'A05_N3_SIH'] /100
BRfinal$TXIPAD[BRfinal$Indicador == 'A05_N3_SIH'] = BRfinal$TXIPAD[BRfinal$Indicador == 'A05_N3_SIH'] /100
BRfinal$INFTX95I[BRfinal$Indicador == 'A05_N3_SIH'] = BRfinal$INFTX95I[BRfinal$Indicador == 'A05_N3_SIH'] /100
BRfinal$SUPTX95I[BRfinal$Indicador == 'A05_N3_SIH'] = BRfinal$SUPTX95I[BRfinal$Indicador == 'A05_N3_SIH'] /100






#BRfinal <- subset(BRfinal, BRfinal$Nome == "M16" |BRfinal$Nome == "M26" | BRfinal$Nome == "M29"| BRfinal$Nome == "M32"| BRfinal$Nome == "M32"| BRfinal$Nome == "M35"| BRfinal$Nome == "M23")


#escolhe_regiao <- "municipio"
#escolhe_regiao <- "regiao_saude"
#escolhe_regiao <- "macrorregiao"

if (escolhe_banco == "SIM" & escolhe_regiao == "macrorregiao") {
  write.csv2(
    BRfinal,
    "./Resultados_PAD/SIM_MR_BR.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}
if (escolhe_banco == "SIM" & escolhe_regiao == "regiao_saude") {
  write.csv2(
    BRfinal,
    "./Resultados_PAD/SIM_RS_BR.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}
if (escolhe_banco == "SIM" & escolhe_regiao == "municipio") {
  write.csv2(
    BRfinal,
    "./Resultados_PAD/SIM_MUN_BR.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}
if (escolhe_banco == "SIH" & escolhe_regiao == "macrorregiao") {
  write.csv2(
    BRfinal,
    "./Resultados_PAD/SIH_MR_BR.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}
if (escolhe_banco == "SIH" & escolhe_regiao == "regiao_saude") {
  write.csv2(
    BRfinal,
    "./Resultados_PAD/SIH_RS_BR.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}

if (escolhe_banco == "SIH" & escolhe_regiao == "municipio") {
  write.csv2(
    BRfinal,
    "./Resultados_PAD/SIH_MUN_BR.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8",
    na = ""
  )
}

#
rm(BRfinal)
rm(BRbase)


rm(Estats)
rm(EstatsBR)
rm(EstatsRG)
rm(EstatsUF)
rm(EstatsCgr)


gc()


