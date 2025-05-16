cd metrics
./get_metrics.sh
cd ..

Rscript gen_model_fits.R
Rscript gen_notebook_data.R

R -e "require(rmarkdown); render('index.Rmd')"
