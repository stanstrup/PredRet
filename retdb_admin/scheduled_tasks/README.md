To schedule running of the script that rebuilds the models and predicts all retention times every 30 minutes run:
```
crontab -e
```

and add the following

```
*/30 * * * * cd "/SHINY_PATH/retpred_shiny/retdb_admin/scheduled_tasks" && Rscript "/SHINY_PATH/retpred_shiny/retdb_admin/scheduled_tasks/calc_all_models.R" && Rscript "/SHINY_PATH/retpred_shiny/retdb_admin/scheduled_tasks/make_predictions.R"
```
replacing SHINY_PATH appropriately.
