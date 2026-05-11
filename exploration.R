library(dplyr)
library(tidyr)

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