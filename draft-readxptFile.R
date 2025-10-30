library(foreign)
library(readr)
library(dplyr)
library(stringr)

pyr_0708 <- read.xport("UPHOPM_E_2007-2008.xpt")
write.csv(pyr_0708, file = "pyrethrin0708.csv")
pyr_0910 <- read.xport("UPHOPM_F_2009-2010.xpt")
write.csv(pyr_0910, file = "pyrethrin0910.csv")
pyr_1112 <- read.xport("UPHOPM_G_2011-2012.xpt")
write.csv(pyr_1112, file = "pyrethrin1112.csv")
lung_0708 <- read.xport("SPX_E_2007-2008.xpt")
write.csv(lung_0708, file = "lung0708.csv")
lung_0910 <- read.xport("SPX_F_2009-2010.xpt")
write.csv(lung_0910, file = "lung0910.csv")
lung_1112 <- read.xport("SPX_G_2011-2012.xpt")
write.csv(lung_1112, file = "lung1112.csv")
demo_0708 <- read.xport("DEMO_E_2007-2008.xpt")
write.csv(demo_0708, file = "demo0708.csv")
demo_0910 <- read.xport("DEMO_F_2009-2010.xpt")
write.csv(demo_0910, file = "demo0910.csv")
demo_1112 <- read.xport("DEMO_G_2011-2012.xpt")
write.csv(demo_1112, file = "demo1112.csv")
#______#
# ---- Setup ----
py78 <- read.csv("pyrethrin0708.csv")
lu78 <- read.csv("lung0708.csv")
de78 <- read.csv("demo0708.csv")
py78_clean <- py78 |> select(SEQN, URXCB3)
lu78_clean <- lu78 |> select(SEQN, SPXNFEV1, SPXNFVC, SPXNPEF)
de78_clean <- de78 |> select(SEQN, RIAGENDR, RIDAGEYR, RIDAGEMN, RIDAGEEX)

# --- 1) Make sure SEQN is the same type everywhere ---
py78_clean <- py78_clean %>% mutate(SEQN = as.integer(SEQN))
lu78_clean <- lu78_clean %>% mutate(SEQN = as.integer(SEQN))
de78_clean <- de78_clean %>% mutate(SEQN = as.integer(SEQN))

# --- 2) Check duplicates in each file ---
dups_py <- py78_clean %>% count(SEQN) %>% filter(n > 1)
dups_lu <- lu78_clean %>% count(SEQN) %>% filter(n > 1)
dups_de <- de78_clean %>% count(SEQN) %>% filter(n > 1)
message(sprintf("Duplicates: py=%d, lu=%d, de=%d",
                nrow(dups_py), nrow(dups_lu), nrow(dups_de)))

# --- 3) Do they have the same SEQN (set) and the same order? ---
same_set_py_lu <- setequal(py78_clean$SEQN, lu78_clean$SEQN)
same_set_py_de <- setequal(py78_clean$SEQN, de78_clean$SEQN)
same_order_py_lu <- identical(py78_clean$SEQN, lu78_clean$SEQN)
same_order_py_de <- identical(py78_clean$SEQN, de78_clean$SEQN)

message(sprintf("Same SET of SEQN?  py vs lu: %s; py vs de: %s",
                same_set_py_lu, same_set_py_de))
message(sprintf("Same ORDER of SEQN? py vs lu: %s; py vs de: %s",
                same_order_py_lu, same_order_py_de))

# 4) Merge on the common SEQN (present in all three)
merged_78 <- py78_clean %>%
  inner_join(lu78_clean, by = "SEQN") %>%
  inner_join(de78_clean, by = "SEQN") %>%
  arrange(SEQN) %>%
  select(SEQN, URXCB3, SPXNFEV1, SPXNFVC, SPXNPEF,
         RIAGENDR, RIDAGEYR, RIDAGEMN, RIDAGEEX)

message(sprintf("Merged rows (in all three) = %d; columns = %d",
                nrow(merged_78), ncol(merged_78)))

write_csv(merged_78, "nhanes_0708_merged.csv")