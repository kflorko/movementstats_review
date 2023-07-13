fish_raster_ext

# generate availability sample
set.seed(2023)
data_ssf <- seal %>%
  mutate(date = as.POSIXct(date)) %>%
  make_track(lon, lat, date) %>%
  steps() %>%
  random_steps() %>% 
  arrange(case_) %>%
  amt::extract_covariates(fish_raster, where = "both") 


# fit SSF1 model
m1 <- data_ssf |> 
  fit_clogit(case_ ~ preydiv_end +  cos(ta_) + log(sl_) + #studyarea_end +
               # add terms for home range
               #x2_ + y2_ + I(x2_^2 + y2_^2) +
               strata(step_id_))

# generate redistribution kernel
k1 <- redistribution_kernel(m1, map = fish_raster, start = start,
                            stochastic = TRUE,
                            tolerance.outside = 1,
                            n.control = 1e5)

# Now simulate a path of length 1e3
n_steps = 1e5
n_steps1 = n_steps + 1
n_ind <- 1000
burnin <- n_steps/50
n_steps = 1e5
n_steps1 = n_steps + 1
x = n_steps1-burnin


# Now simulate a path of length 30 (number of locations) for 50 times (number of paths). This will lead a transient UD. For a steady state UD, either increase `n` or choose many more random starting points. 
  start.time <- Sys.time()
  set.seed(2023)
# for multiple animals
#p1 <- replicate(n = n_ind, amt::simulate_path(k1, n = n_steps, start = start), simplify = FALSE) 
# for one animal
p1 <- amt::simulate_path(k1, n = n_steps, start = start)
  end.time <- Sys.time()
  print(round(end.time - start.time,2))

bind_p1 <- dplyr::bind_rows(p1)


# plot all simulated paths
ggplot() + 
  geom_tile(data = fish, aes(x = lon,y = lat,fill = preydiv)) +
  scale_fill_viridis(option = "mako", limits = c(0.5, 0.75), name = "Prey\ndiversity") +
  geom_polygon(data = nat_trans, aes(x=long,y=lat,group=group), fill = "grey80", color = "white") +
  geom_polygon(data = nat_trans, aes(x=long,y=lat,group=group), fill = "grey80", color = "white") +
  geom_point(data = bind_p1, aes(x = x_,y = y_), alpha = 0.61) +
  geom_path(data = bind_p1, aes(x = x_,y = y_)) +
  theme_void() +
  coord_cartesian(xlim = c(150000,550000), ylim = c(-500000,-50000), expand = F) 


# estimate SSUD - if multiple animals
#uds <- lapply(c(5,50050), function(i) {
#  tibble(
#    rep = 1:n_ind, 
#    path = map(p1, ~ dplyr::slice(.x, i))) |> 
#  unnest(cols = path) |> 
#  filter(!is.na(x_)) |> 
#  make_track(x_, y_) |> 
#  amt::hr_kde(trast = fish_raster, which_min = "global") |>
#  hr_ud() |>
#  terra::as.data.frame(xy = TRUE)
#})


# estimate SSUD - if only one animal
uds <- tibble(rep = 1:n_steps1, 
                   x_ = p1$x_, y_ = p1$y_,
                   t_ = p1$t_, dt = p1$dt) |> 
  filter(!is.na(x_)) |> 
  make_track(x_, y_) |> 
  hr_kde(trast = fish_raster, which_min = "global") %>%
  hr_ud() %>% 
  terra::as.data.frame(xy = TRUE)


# plot SSUD
map_ssf1 <- ggplot() + 
  geom_tile(data = as.data.frame(uds), aes(x = x,y = y, fill = preydiv)) +
  scale_fill_viridis(option = "mako", name = "SSF1 prediction") +
  geom_polygon(data = nat_trans, aes(x=long,y=lat,group=group), fill = "grey80", color = "white") +
  coord_cartesian(xlim = c(150000,550000), ylim = c(-500000,-50000), expand = F) +
  theme_void()
map_ssf1

