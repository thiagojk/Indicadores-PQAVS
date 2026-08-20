# =========================================================
# EXECUTAR TODO O PROCESSAMENTO DO PQAVS
# =========================================================

library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr)


source("Scripts/00_config.R")
source("Scripts/01_funcoes.R")
source("Scripts/02_indicadores.R")

# Cria SMS e SES
source("Scripts/PFVS.R")

# Usa SMS para adicionar PQAVS_Incentivo em Dados_Completos
source("Scripts/03_consolidacao.R")

# Cria Estados e usa SES para adicionar PQAVS_Incentivo
source("Scripts/04_resumos.R")

# Salva Excel e RDS
source("Scripts/05_salvar.R")
