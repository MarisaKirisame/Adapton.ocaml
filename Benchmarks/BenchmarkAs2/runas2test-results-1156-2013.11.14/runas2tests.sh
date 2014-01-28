
BIN=../../_product

touch runas2testlazysabidi.csv
touch runas2testeagersatotalorder.csv
touch runas2testnonsaeager.csv

for sht in 2 3 4 5 6 7 10 12 13 14 15; do
    for samp in 1 2 3 4 5 6 7 8; do
    echo "sheet depth=$sht/20; same number=$samp"
    args="--Random.self_init --stats-test $sht"
    $BIN/runas2.native --adapton-module LazySABidi $args --stats-out runas2testlazysabidi.csv
    $BIN/runas2.native --adapton-module NonSAEager $args --stats-out runas2testnonsaeager.csv
    $BIN/runas2.native --adapton-module EagerSATotalOrder $args --stats-out runas2testeagersatotalorder.csv
    done
done
