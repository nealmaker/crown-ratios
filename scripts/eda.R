summary(train)

train %>% 
  ggplot(aes(dbh_mid, cr_rate)) +
  geom_smooth() + 
  scale_y_continuous(limits = c(-20, 18))

train %>% 
  ggplot(aes(cr_mid, cr_rate)) +
  geom_smooth() + 
  scale_y_continuous(limits = c(-20, 18))

train %>% 
  ggplot(aes(ba_mid, cr_rate)) +
  geom_smooth() + 
  scale_y_continuous(limits = c(-20, 18))

train %>% 
  ggplot(aes(bal_mid, cr_rate)) +
  geom_smooth() + 
  scale_y_continuous(limits = c(-20, 18))

train %>% 
  ggplot(aes(lat, cr_rate)) +
  geom_smooth() + 
  scale_y_continuous(limits = c(-20, 18))

train %>% 
  ggplot(aes(lon, cr_rate)) +
  geom_smooth() + 
  scale_y_continuous(limits = c(-20, 18))

train %>% 
  mutate(spp = reorder(spp, -cr_rate, FUN = mean)) %>% 
  ggplot(aes(spp, cr_rate)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

train %>% 
  mutate(forest_type_s = reorder(forest_type_s, -cr_rate, FUN = mean)) %>% 
  ggplot(aes(forest_type_s, cr_rate)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
