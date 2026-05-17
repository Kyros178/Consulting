####### Statistical Consulting: Project

### Packages
install.packages("markovchain")
library(dplyr)
library(tidyr)
library(markovchain)
### Data Management ################################################################################

### Bats
bat <- read.csv("rspb20141370supp2_bat.csv", sep=";")

#### Data management: I want to have the dataset in longformat
# Renaming the position variables
n_positions <- ncol(bat) 
colnames(bat)[1:n_positions] <- paste0("pos_", 1:n_positions)

# Turning the dataset from wide to long format
bat_long <- bat %>%
  # Creating a Sequence-ID
  mutate(sequence_id = row_number()) %>%
  #Pivot
  pivot_longer(
    cols = -sequence_id,          
    names_to = "timepoint",       
    values_to = "sound"           
  ) %>%
  # Creating integer values for the new position variable
  mutate(timepoint = as.integer(gsub("\\D", "", timepoint))) %>%
  # remove NAs for shorter sequences
  filter(!is.na(sound)) %>%
  #Sorting
  arrange(sequence_id, timepoint)

#### Chickadee
chick <- read.csv("rspb20141370supp2_Chickadee.csv", sep=";")

#### Data management: I want to have the dataset in longformat
# Renaming the position variables
n_positions <- ncol(chick) 
colnames(chick)[1:n_positions] <- paste0("pos_", 1:n_positions)

# Turning the dataset from wide to long format
chick_long <- chick %>%
  # Creating a Sequence-ID
  mutate(sequence_id = row_number()) %>%
  #Pivot
  pivot_longer(
    cols = -sequence_id,          
    names_to = "timepoint",       
    values_to = "sound"           
  ) %>%
  # Creating integer values for the new position variable
  mutate(timepoint = as.integer(gsub("\\D", "", timepoint))) %>%
  # remove NAs for shorter sequences
  filter(!is.na(sound)) %>%
  #Sorting
  arrange(sequence_id, timepoint)

#### Finch
finch <- read.csv("rspb20141370supp2_Finch.csv", sep=";")

#### Data management: I want to have the dataset in longformat
# Renaming the position variables
n_positions <- ncol(finch) 
colnames(finch)[1:n_positions] <- paste0("pos_", 1:n_positions)

# Turning the dataset from wide to long format
finch_long <- finch %>%
  # Creating a Sequence-ID
  mutate(sequence_id = row_number()) %>%
  #Pivot
  pivot_longer(
    cols = -sequence_id,          
    names_to = "timepoint",       
    values_to = "sound"           
  ) %>%
  # Creating integer values for the new position variable
  mutate(timepoint = as.integer(gsub("\\D", "", timepoint))) %>%
  # remove NAs for shorter sequences
  filter(!is.na(sound)) %>%
  #Sorting
  arrange(sequence_id, timepoint)

#### Hyrax
hyrax <- read.csv("rspb20141370supp2_Hyrax.csv", sep=";")

#### Data management: I want to have the dataset in longformat
# Renaming the position variables
n_positions <- ncol(hyrax) 
colnames(hyrax)[1:n_positions] <- paste0("pos_", 1:n_positions)

# Turning the dataset from wide to long format
hyrax_long <- hyrax %>%
  # Creating a Sequence-ID
  mutate(sequence_id = row_number()) %>%
  #Pivot
  pivot_longer(
    cols = -sequence_id,          
    names_to = "timepoint",       
    values_to = "sound"           
  ) %>%
  # Creating integer values for the new position variable
  mutate(timepoint = as.integer(gsub("\\D", "", timepoint))) %>%
  # remove NAs for shorter sequences
  filter(!is.na(sound)) %>%
  #Sorting
  arrange(sequence_id, timepoint)

#### Orangutan
orang <- read.csv("rspb20141370supp2_Orangutan.csv", sep=";")

#### Data management: I want to have the dataset in longformat
# Renaming the position variables
n_positions <- ncol(orang) 
colnames(orang)[1:n_positions] <- paste0("pos_", 1:n_positions)

# Turning the dataset from wide to long format
orang_long <- orang %>%
  # Creating a Sequence-ID
  mutate(sequence_id = row_number()) %>%
  #Pivot
  pivot_longer(
    cols = -sequence_id,          
    names_to = "timepoint",       
    values_to = "sound"           
  ) %>%
  # Creating integer values for the new position variable
  mutate(timepoint = as.integer(gsub("\\D", "", timepoint))) %>%
  # remove NAs for shorter sequences
  filter(!is.na(sound)) %>%
  #Sorting
  arrange(sequence_id, timepoint)

#### Orca
orca <- read.csv("rspb20141370supp2_Orca.csv", sep=";")

#### Data management: I want to have the dataset in longformat
# Renaming the position variables
n_positions <- ncol(orca) 
colnames(orca)[1:n_positions] <- paste0("pos_", 1:n_positions)

# Turning the dataset from wide to long format
orca_long <- orca %>%
  # Creating a Sequence-ID
  mutate(sequence_id = row_number()) %>%
  #Pivot
  pivot_longer(
    cols = -sequence_id,          
    names_to = "timepoint",       
    values_to = "sound"           
  ) %>%
  # Creating integer values for the new position variable
  mutate(timepoint = as.integer(gsub("\\D", "", timepoint))) %>%
  # remove NAs for shorter sequences
  filter(!is.na(sound)) %>%
  #Sorting
  arrange(sequence_id, timepoint)


#### Pilot
pilot <- read.csv("rspb20141370supp2_Pilot.csv", sep=";")

#### Data management: I want to have the dataset in longformat
# Renaming the position variables
n_positions <- ncol(pilot) 
colnames(pilot)[1:n_positions] <- paste0("pos_", 1:n_positions)

# Turning the dataset from wide to long format
pilot_long <- pilot %>%
  # Creating a Sequence-ID
  mutate(sequence_id = row_number()) %>%
  #Pivot
  pivot_longer(
    cols = -sequence_id,          
    names_to = "timepoint",       
    values_to = "sound"           
  ) %>%
  # Creating integer values for the new position variable
  mutate(timepoint = as.integer(gsub("\\D", "", timepoint))) %>%
  # remove NAs for shorter sequences
  filter(!is.na(sound)) %>%
  #Sorting
  arrange(sequence_id, timepoint)

#### English
eng <- read.csv("rspb20141370supp2_English.csv", sep=";")

#### Data management: I want to have the dataset in longformat
# Renaming the position variables
n_positions <- ncol(eng) 
colnames(eng)[1:n_positions] <- paste0("pos_", 1:n_positions)

# Turning the dataset from wide to long format
eng_long <- eng %>%
  # Creating a Sequence-ID
  mutate(sequence_id = row_number()) %>%
  #Pivot
  pivot_longer(
    cols = -sequence_id,          
    names_to = "timepoint",       
    values_to = "sound"           
  ) %>%
  # Creating integer values for the new position variable
  mutate(timepoint = as.integer(gsub("\\D", "", timepoint))) %>%
  # remove NAs for shorter sequences
  filter(!is.na(sound)) %>%
  #Sorting
  arrange(sequence_id, timepoint)

##### Exploratory Analysis ##########################################################################

### Bat
# First exploratory steps
unique(bat_long$sound) #how many distinct sounds
max(bat_long$timepoint) #how long is the longest sequence

#relative frequencies of sounds
table(bat_long$sound)
barplot(table(bat_long$sound))

#frequencies of every sound at every position
freq_table_bat <- bat_long %>%
  group_by(timepoint, sound) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n), .by = timepoint)

freq_table_bat

freq_list_bat <- bat_long %>%
  group_by(timepoint) %>%
  group_map(~ count(.x, sound) %>% mutate(prop = n / sum(n)))
freq_list_bat

# frequencies of every sequence length
seq_length_table_bat <- bat_long %>%
  group_by(sequence_id) %>%
  summarise(length = n(), .groups = "drop") %>%
  count(length, name = "n_sequences")
seq_length_table_bat


###Chickadee
# First exploratory steps
unique(chick_long$sound) #how many distinct sounds
max(chick_long$timepoint) #how long is the longest sequence

#relative frequencies of sounds
table(chick_long$sound)
barplot(table(chick_long$sound))

#frequencies of every sound at every position
freq_table_chick <- chick_long %>%
  group_by(timepoint, sound) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n), .by = timepoint)

freq_table_chick

freq_list_chick <- chick_long %>%
  group_by(timepoint) %>%
  group_map(~ count(.x, sound) %>% mutate(prop = n / sum(n)))
freq_list_chick

# frequencies of every sequence length
seq_length_table_chick <- chick_long %>%
  group_by(sequence_id) %>%
  summarise(length = n(), .groups = "drop") %>%
  count(length, name = "n_sequences")
seq_length_table_chick

### Finch
# First exploratory steps
unique(finch_long$sound) #how many distinct sounds
max(finch_long$timepoint) #how long is the longest sequence

#relative frequencies of sounds
table(finch_long$sound)
barplot(table(finch_long$sound))

#frequencies of every sound at every position
freq_table_finch <- finch_long %>%
  group_by(timepoint, sound) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n), .by = timepoint)

freq_table_finch

freq_list_finch <- finch_long %>%
  group_by(timepoint) %>%
  group_map(~ count(.x, sound) %>% mutate(prop = n / sum(n)))
freq_list_finch

# frequencies of every sequence length
seq_length_table_finch <- finch_long %>%
  group_by(sequence_id) %>%
  summarise(length = n(), .groups = "drop") %>%
  count(length, name = "n_sequences")
seq_length_table_finch

### Hyrax
# First exploratory steps
unique(hyrax_long$sound) #how many distinct sounds
max(hyrax_long$timepoint) #how long is the longest sequence

#relative frequencies of sounds
table(hyrax_long$sound)
barplot(table(hyrax_long$sound))

#frequencies of every sound at every position
freq_table_hyrax <- hyrax_long %>%
  group_by(timepoint, sound) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n), .by = timepoint)

freq_table_hyrax

freq_list_hyrax <- hyrax_long %>%
  group_by(timepoint) %>%
  group_map(~ count(.x, sound) %>% mutate(prop = n / sum(n)))
freq_list_hyrax

# frequencies of every sequence length
seq_length_table_hyrax <- hyrax_long %>%
  group_by(sequence_id) %>%
  summarise(length = n(), .groups = "drop") %>%
  count(length, name = "n_sequences")
seq_length_table_hyrax


### Hyrax
# First exploratory steps
unique(orang_long$sound) #how many distinct sounds
max(orang_long$timepoint) #how long is the longest sequence

#relative frequencies of sounds
table(orang_long$sound)
barplot(table(orang_long$sound))

#frequencies of every sound at every position
freq_table_orang <- orang_long %>%
  group_by(timepoint, sound) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n), .by = timepoint)

freq_table_orang

freq_list_orang <- orang_long %>%
  group_by(timepoint) %>%
  group_map(~ count(.x, sound) %>% mutate(prop = n / sum(n)))
freq_list_orang

# frequencies of every sequence length
seq_length_table_orang <- orang_long %>%
  group_by(sequence_id) %>%
  summarise(length = n(), .groups = "drop") %>%
  count(length, name = "n_sequences")
seq_length_table_orang

### Orca

# First exploratory steps
unique(orca_long$sound) #how many distinct sounds
max(orca_long$timepoint) #how long is the longest sequence

#relative frequencies of sounds
table(orca_long$sound)
barplot(table(orca_long$sound))

#frequencies of every sound at every position
freq_table_orca <- orca_long %>%
  group_by(timepoint, sound) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n), .by = timepoint)

freq_table_orca

freq_list_orca <- orca_long %>%
  group_by(timepoint) %>%
  group_map(~ count(.x, sound) %>% mutate(prop = n / sum(n)))
freq_list_orca

# frequencies of every sequence length
seq_length_table_orca <- orca_long %>%
  group_by(sequence_id) %>%
  summarise(length = n(), .groups = "drop") %>%
  count(length, name = "n_sequences")
seq_length_table_orca


### Pilot
# First exploratory steps
unique(pilot_long$sound) #how many distinct sounds
max(pilot_long$timepoint) #how long is the longest sequence

#relative frequencies of sounds
table(pilot_long$sound)
barplot(table(pilot_long$sound))

#frequencies of every sound at every position
freq_table_pilot <- pilot_long %>%
  group_by(timepoint, sound) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n), .by = timepoint)

freq_table_pilot

freq_list_pilot <- pilot_long %>%
  group_by(timepoint) %>%
  group_map(~ count(.x, sound) %>% mutate(prop = n / sum(n)))
freq_list_pilot

# frequencies of every sequence length
seq_length_table_pilot <- pilot_long %>%
  group_by(sequence_id) %>%
  summarise(length = n(), .groups = "drop") %>%
  count(length, name = "n_sequences")
seq_length_table_pilot

### English
# First exploratory steps
unique(eng_long$sound) #how many distinct sounds
max(eng_long$timepoint) #how long is the longest sequence

#relative frequencies of sounds
table(eng_long$sound)
barplot(table(eng_long$sound))

#frequencies of every sound at every position
freq_table_eng <- eng_long %>%
  group_by(timepoint, sound) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = n / sum(n), .by = timepoint)

freq_table_eng

freq_list_eng <- eng_long %>%
  group_by(timepoint) %>%
  group_map(~ count(.x, sound) %>% mutate(prop = n / sum(n)))
freq_list_eng

# frequencies of every sequence length
seq_length_table_eng <- eng_long %>%
  group_by(sequence_id) %>%
  summarise(length = n(), .groups = "drop") %>%
  count(length, name = "n_sequences")
seq_length_table_eng


### Markov Chain Models ##########################################################################
## Bat
bat_mc0 <- prop.table(table(bat_long$sound))
bat_mc0

# Extract sequences by ID
sequences_bat <- bat_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  summarise(seq = list(sound), .groups = "drop") %>% 
  pull(seq)

# first order MC
bat_mc1 <- markovchainFit(data = sequences_bat, method = "mle")
bat_mc1$estimate

# second order MC
bat_transitions_2 <- bat_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  mutate(
    s_t2  = lag(sound, 1),   
    s_t1  = sound,            
    s_t0  = lead(sound, 1)    
  ) %>% 
  ungroup() %>% 
  filter(!is.na(s_t2), !is.na(s_t0))


bat_transitions_2 <- bat_transitions_2 %>% 
  mutate(from_pair = paste(s_t2, s_t1, sep = "->"))

bat_count_mat2 <- table(from  = bat_transitions_2$from_pair,
                    to = bat_transitions_2$s_t0)

bat_mc2 <- prop.table(bat_count_mat2, margin = 1)
print(round(bat_mc2, 3))



## Chick
chick_mc0 <- prop.table(table(chick_long$sound))
chick_mc0

# Extract sequences by ID
sequences_chick <- chick_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  summarise(seq = list(sound), .groups = "drop") %>% 
  pull(seq)

# first order MC
chick_mc1 <- markovchainFit(data = sequences_chick, method = "mle")
chick_mc1$estimate

# second order MC
chick_transitions_2 <- chick_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  mutate(
    s_t2  = lag(sound, 1),   
    s_t1  = sound,            
    s_t0  = lead(sound, 1)    
  ) %>% 
  ungroup() %>% 
  filter(!is.na(s_t2), !is.na(s_t0))


chick_transitions_2 <- chick_transitions_2 %>% 
  mutate(from_pair = paste(s_t2, s_t1, sep = "->"))

chick_count_mat2 <- table(from  = chick_transitions_2$from_pair,
                        to = chick_transitions_2$s_t0)

chick_mc2 <- prop.table(chick_count_mat2, margin = 1)
print(round(chick_mc2, 3))



## Finch
finch_mc0 <- prop.table(table(finch_long$sound))
finch_mc0

# Extract sequences by ID
sequences_finch <- finch_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  summarise(seq = list(sound), .groups = "drop") %>% 
  pull(seq)

# first order MC
finch_mc1 <- markovchainFit(data = sequences_finch, method = "mle")
finch_mc1$estimate

# second order MC
finch_transitions_2 <- finch_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  mutate(
    s_t2  = lag(sound, 1),   
    s_t1  = sound,            
    s_t0  = lead(sound, 1)    
  ) %>% 
  ungroup() %>% 
  filter(!is.na(s_t2), !is.na(s_t0))


finch_transitions_2 <- finch_transitions_2 %>% 
  mutate(from_pair = paste(s_t2, s_t1, sep = "->"))

finch_count_mat2 <- table(from  = finch_transitions_2$from_pair,
                        to = finch_transitions_2$s_t0)

finch_mc2 <- prop.table(finch_count_mat2, margin = 1)
print(round(finch_mc2, 3))



## Hyrax
hyrax_mc0 <- prop.table(table(hyrax_long$sound))
hyrax_mc0

# Extract sequences by ID
sequences_hyrax <- hyrax_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  summarise(seq = list(sound), .groups = "drop") %>% 
  pull(seq)

# first order MC
hyrax_mc1 <- markovchainFit(data = sequences_hyrax, method = "mle")
hyrax_mc1$estimate

# second order MC
hyrax_transitions_2 <- hyrax_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  mutate(
    s_t2  = lag(sound, 1),   
    s_t1  = sound,            
    s_t0  = lead(sound, 1)    
  ) %>% 
  ungroup() %>% 
  filter(!is.na(s_t2), !is.na(s_t0))


hyrax_transitions_2 <- hyrax_transitions_2 %>% 
  mutate(from_pair = paste(s_t2, s_t1, sep = "->"))

hyrax_count_mat2 <- table(from  = hyrax_transitions_2$from_pair,
                        to = hyrax_transitions_2$s_t0)

hyrax_mc2 <- prop.table(hyrax_count_mat2, margin = 1)
print(round(hyrax_mc2, 3))



## Orangutan
orang_mc0 <- prop.table(table(orang_long$sound))
orang_mc0

# Extract sequences by ID
sequences_orang <- orang_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  summarise(seq = list(sound), .groups = "drop") %>% 
  pull(seq)

# first order MC
orang_mc1 <- markovchainFit(data = sequences_orang, method = "mle")
orang_mc1$estimate

# second order MC
orang_transitions_2 <- orang_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  mutate(
    s_t2  = lag(sound, 1),   
    s_t1  = sound,            
    s_t0  = lead(sound, 1)    
  ) %>% 
  ungroup() %>% 
  filter(!is.na(s_t2), !is.na(s_t0))


orang_transitions_2 <- orang_transitions_2 %>% 
  mutate(from_pair = paste(s_t2, s_t1, sep = "->"))

orang_count_mat2 <- table(from  = orang_transitions_2$from_pair,
                        to = orang_transitions_2$s_t0)

orang_mc2 <- prop.table(orang_count_mat2, margin = 1)
print(round(orang_mc2, 3))



## Orca
orca_mc0 <- prop.table(table(orca_long$sound))
orca_mc0

# Extract sequences by ID
sequences_orca <- orca_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  summarise(seq = list(sound), .groups = "drop") %>% 
  pull(seq)

# first order MC
orca_mc1 <- markovchainFit(data = sequences_orca, method = "mle")
orca_mc1$estimate

# second order MC
orca_transitions_2 <- orca_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  mutate(
    s_t2  = lag(sound, 1),   
    s_t1  = sound,            
    s_t0  = lead(sound, 1)    
  ) %>% 
  ungroup() %>% 
  filter(!is.na(s_t2), !is.na(s_t0))


orca_transitions_2 <- orca_transitions_2 %>% 
  mutate(from_pair = paste(s_t2, s_t1, sep = "->"))

orca_count_mat2 <- table(from  = orca_transitions_2$from_pair,
                        to = orca_transitions_2$s_t0)

orca_mc2 <- prop.table(orca_count_mat2, margin = 1)
print(round(orca_mc2, 3))



## Pilot
pilot_mc0 <- prop.table(table(pilot_long$sound))
pilot_mc0

# Extract sequences by ID
sequences_pilot <- pilot_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  summarise(seq = list(sound), .groups = "drop") %>% 
  pull(seq)

# first order MC
pilot_mc1 <- markovchainFit(data = sequences_pilot, method = "mle")
pilot_mc1$estimate

# second order MC
pilot_transitions_2 <- pilot_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  mutate(
    s_t2  = lag(sound, 1),   
    s_t1  = sound,            
    s_t0  = lead(sound, 1)    
  ) %>% 
  ungroup() %>% 
  filter(!is.na(s_t2), !is.na(s_t0))


pilot_transitions_2 <- pilot_transitions_2 %>% 
  mutate(from_pair = paste(s_t2, s_t1, sep = "->"))

pilot_count_mat2 <- table(from  = pilot_transitions_2$from_pair,
                        to = pilot_transitions_2$s_t0)

pilot_mc2 <- prop.table(pilot_count_mat2, margin = 1)
print(round(pilot_mc2, 3))



## English (lol)
eng_mc0 <- prop.table(table(eng_long$sound))
eng_mc0

# Extract sequences by ID
sequences_eng <- eng_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  summarise(seq = list(sound), .groups = "drop") %>% 
  pull(seq)

# first order MC
eng_mc1 <- markovchainFit(data = sequences_eng, method = "mle")
eng_mc1$estimate

# second order MC
eng_transitions_2 <- eng_long %>% 
  arrange(sequence_id, timepoint) %>% 
  group_by(sequence_id) %>% 
  mutate(
    s_t2  = lag(sound, 1),   
    s_t1  = sound,            
    s_t0  = lead(sound, 1)    
  ) %>% 
  ungroup() %>% 
  filter(!is.na(s_t2), !is.na(s_t0))


eng_transitions_2 <- eng_transitions_2 %>% 
  mutate(from_pair = paste(s_t2, s_t1, sep = "->"))

eng_count_mat2 <- table(from  = eng_transitions_2$from_pair,
                        to = eng_transitions_2$s_t0)

eng_mc2 <- prop.table(eng_count_mat2, margin = 1)
print(round(eng_mc2, 3))