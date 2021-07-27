if (!require('palmerpenguins')) devtools::install_github("allisonhorst/palmerpenguins"); library('palmerpenguins')

require(ggplot2)
require(tidymodels)

penguins

penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(flipper_length_mm, bill_length_mm, color = sex, size = body_mass_g)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~species)

penguins_df <- penguins %>%
  filter(!is.na(sex)) %>%
  select(-year, -island)

set.seed(123)
penguin_split <- initial_split(penguins_df, strata = sex)
penguin_train <- training(penguin_split)
penguin_test <- testing(penguin_split)
penguin_split
penguin_cv <- vfold_cv(data = penguin_train, v = 10, repeats = 10, strata = sex)
penguin_cv

glm_spec <- logistic_reg() %>%
  set_engine("glm")

penguin_wf <- workflow() %>%
  add_formula(sex ~ .)

doParallel::registerDoParallel()

glm_rs <- penguin_wf %>%
  add_model(glm_spec) %>%
  fit_resamples(
    resamples = penguin_cv,
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )

glm_rs

collect_metrics(glm_rs)

glm_rs %>%
  conf_mat_resampled()

glm_rs %>%
  collect_predictions() %>%
  group_by(id, id2) %>%
  roc_curve(sex, .pred_female)

glm_rs %>%
  collect_predictions() %>%
  group_by(id, id2) %>%
  roc_curve(sex, .pred_female) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id2)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = TRUE, alpha = 0.5, size = 0.8) +
  coord_equal() +
  facet_wrap(~id) + 
  labs(color='Fold', x = "1 - Specificity", y = "Sensitivity", title = "ROC & AUC by Fold and Repeat") +
  theme_minimal()

penguin_final <- penguin_wf %>%
  add_model(glm_spec) %>%
  last_fit(penguin_split)

penguin_final

penguin_final %>%
  collect_predictions() %>%
  roc_curve(sex, .pred_female)

r <- penguin_final %>%
  collect_predictions() %>%
  #  group_by(id, id2) %>%
  roc_curve(sex, .pred_female) %>% 
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_point(size = 0.2, aes(color = .threshold)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = TRUE, alpha = 0.3, size = 0.5) +
  geom_text(aes(label = round(.threshold, 2)), size = 2.5, vjust = -0.5, fontface = "bold") +
  coord_equal()
plotly::ggplotly(r)



library(probably)

thresh <- 0.75

penguin_final %>%
  collect_predictions()  %>%
  mutate(.pred_class = make_two_class_pred(.pred_female, 
                                           levels(sex),
                                           threshold = thresh),
         .pred_class = factor(.pred_class)) %>%
  conf_mat(sex, .pred_class)


collect_metrics(penguin_final)

collect_predictions(penguin_final) %>%
  conf_mat(sex, .pred_class)

collect_predictions(penguin_final) %>%
  sensitivity(sex, .pred_class)

collect_predictions(penguin_final) %>%
  specificity(sex, .pred_class)

collect_predictions(penguin_final) %>%
  precision(sex, .pred_class)

penguin_final$.workflow[[1]] %>%
  tidy(exponentiate = TRUE)

penguin_final$.workflow[[1]] %>%
  tidy(exponentiate = TRUE) %>% 
  select(term, estimate) %>% 
  mutate(term = as.factor(term)) %>% 
  ggplot(aes(reorder(term, estimate), estimate, 
             fill =term)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.7) + 
  labs(title = "Increase in odds of penguin being Female by one unit increase in each variable",
       x = "Variable", y = "Odds increase by Times") +
  geom_text(aes(label = round(estimate, 3)), 
            nudge_y = 0.15 , 
            size = 4.5, 
            colour = 'black', 
            fontface = 'bold') +
  coord_flip() +
  theme_light()
  
penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(bill_depth_mm, bill_length_mm, color = sex, size = body_mass_g)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~species)
