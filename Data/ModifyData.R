library(stringr)
library(dplyr)
library(MASS)

###################
### Create DATA ###
###################

load('Data/dbfictif.Rda')

db.fictif <- db.fictif[, c(1:23, 32)]
colnames(db.fictif) <- c("policy_no", "veh.num", "renewal_date", "start_date", "end_date", "risk_expo",            
                         "NbClaims", "TotalCost", "freq_payment", "year_veh", "sex", "year_birth", "year_licence",    
                         "language", "job_type", "car_type", "territory", "use", "car_make", "car_color", "need_glasses",         
                         "food", "hair_color", 'Type' )

df <- db.fictif[,c(1:6, 9:15, 17, 18, 16, 19:24, 7,8)]

df$risk_expo <- round(df$risk_expo, 2)

### Translate ###
unique(df$job_type)
df$job_type <- str_replace(df$job_type, "Autre", "Poet")
df$job_type <- str_replace(df$job_type, "Hockeyeur", "Hockey Player")
df$job_type <- str_replace(df$job_type, "Technicien", "Plumber")
df$job_type <- str_replace(df$job_type, "Ingénieur", "Engineer")
df$job_type <- str_replace(df$job_type, "Infirmière", "Nurse")
df$job_type <- str_replace(df$job_type, "Informaticien", "Accountant")
df$job_type <- str_replace(df$job_type, "Médecin", "Physician")
df$job_type <- str_replace(df$job_type, "Actuaire", "Actuary")
df$job_type <- str_replace(df$job_type, "Avocat", "Lawyer")
df$job_type <- str_replace(df$job_type, "Professeur", "Teacher")
unique(df$job_type)


unique(df$car_type)
df$car_type <- str_replace(df$car_type, "Utilitaires", "SUV")
df$car_type <- str_replace(df$car_type, "Compacte", "Minivan")
df$car_type <- str_replace(df$car_type, "Sous-compacte", "Sedan")
df$car_type <- str_replace(df$car_type, "Intermédiaire", "Pickup")
df$car_type <- str_replace(df$car_type, "Luxe", "Luxury")
unique(df$car_type)

unique(df$language)
df$language <- str_replace(df$language, "F", "French")
df$language <- str_replace(df$language, "A", "English")
unique(df$language)

unique(df$territory)
df$territory <- str_replace(df$territory, "Urbain", "Urban")
df$territory <- str_replace(df$territory, "Semi-urbain", "Suburban")
unique(df$territory)

unique(df$use)
df$use <- str_replace(df$use, "Travail-occasionnel", "Commute")
df$use <- str_replace(df$use, "Travail-quotidien", "Commercial")
df$use <- str_replace(df$use, "Loisir", "Pleasure")
unique(df$use)

unique(df$car_make)
df$car_make <- str_replace(df$car_make, "Autres", "FERRARI")
unique(df$car_make)

unique(df$car_color)
df$car_color <- str_replace(df$car_color, "Autre", "Other")
df$car_color <- str_replace(df$car_color, "Rouge", "Red")
unique(df$car_color)

unique(df$need_glasses)
df$need_glasses <- str_replace(df$need_glasses, "Non", "No")
df$need_glasses <- str_replace(df$need_glasses, "Oui", "Yes")
unique(df$need_glasses)

unique(df$food)
df$food <- str_replace(df$food, "Végétarien", "Vegetarian")
df$food <- str_replace(df$food, "Végétalien", "Vegan")
df$food <- str_replace(df$food, "Carnivore", "Other")
unique(df$food)

unique(df$hair_color)
df$hair_color <- str_replace(df$hair_color, "Bruns", "Dark")
df$hair_color <- str_replace(df$hair_color, "Blonds", "Blonde")
df$hair_color <- str_replace(df$hair_color, "Roux", "Red")
df$hair_color <- str_replace(df$hair_color, "Bleus", "None")
unique(df$hair_color)

###

df2 <- df %>%
  arrange(policy_no, veh.num, renewal_date) %>%
  group_by(policy_no, veh.num) %>%
  mutate(nb.contract = n(),
         contract.number = row_number()) %>%
  filter(contract.number <=5) %>%
  filter(nb.contract >= 5)
df2 <- df2[,-25]

df2 <- df2 %>%
  mutate(ind.0 = ifelse(NbClaims == 0, 1, 0)) %>%
  arrange(policy_no, veh.num, renewal_date) %>%
  group_by(policy_no, veh.num) %>%
  mutate(lagn1 = lag(NbClaims, 1),
         lagn2 = lag(NbClaims, 2),
         lagn3 = lag(NbClaims, 3),
         lagn4 = lag(NbClaims, 4),
         lagk1 = lag(ind.0, 1),
         lagk2 = lag(ind.0, 2),
         lagk3 = lag(ind.0, 3),
         lagk4 = lag(ind.0, 4))
df2 <- df2[,-26]
df2$past.n <- rowSums(df2[,c("lagn1", "lagn2", "lagn3", "lagn4")], na.rm=TRUE)
df2$past.k <- rowSums(df2[,c("lagk1", "lagk2", "lagk3", "lagk4")], na.rm=TRUE)

save(df2, file='Data/df2.Rda')

