
BIN=../../_product

touch runas2testlazysabidi.csv
touch runas2testeagersatotalorder.csv
touch runas2testnonsaeager.csv

for num_changes in 27 30 35 40 45 50; do
#for num_changes in 2 5 7 10 15 17 20 23 25 27 30 35 40 45 50; do
#for sht in 2 3 4 5 6 7 10 12 15 17 20 ; do
#for sht in 17 20 22 25 ; do
    for samp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 ; do
    echo "sheet depth=$sht/20; same number=$samp"
    args="--Random.self_init --stats-test 10 --num-changes $num_changes"
    $BIN/runas2.native --adapton-module LazySABidi $args --stats-out runas2testlazysabidi.csv
    $BIN/runas2.native --adapton-module NonSAEager $args --stats-out runas2testnonsaeager.csv
#    $BIN/runas2.native --adapton-module EagerSATotalOrder $args --stats-out runas2testeagersatotalorder.csv
    done
done
