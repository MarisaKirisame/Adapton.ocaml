
BIN=../_product

touch runas2testlazysabidi.csv
touch runas2testeagersatotalorder.csv
touch runas2testnonsaeager.csv

for sht in 2 3 4 5 6 7 10 12 ; do
#for sht in 2 3 4 5 6 7 10 12 15 17 20 ; do
#for sht in 17 20 22 25 ; do
    for samp in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32; do
    echo "sheet depth=$sht/20; same number=$samp"
    args="--Random.self_init --stats-test $sht"
    $BIN/runas2testlazysabidi.native $args --stats-out runas2testlazysabidi.csv  
    $BIN/runas2testnonsaeager.native $args --stats-out runas2testnonsaeager.csv 
    $BIN/runas2testeagersatotalorder.native $args --stats-out runas2testeagersatotalorder.csv
    done   
done
