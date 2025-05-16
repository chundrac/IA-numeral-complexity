for dataset in "uninum" "sand"
do
    python3 gen_LDL_comprehension.py $dataset
    python3 gen_LDL_production.py $dataset
    python3 gen_MDL.py $dataset
    python3 gen_surprisals.py $dataset
done

python3 gen_surprisals_sensitivity.py uninum
python3 gen_surprisals_sensitivity.py sand
