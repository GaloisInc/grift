rm -rf *.log
rm -rf results/
mkdir results
for file in rv64ui-p-*
do
    echo "testing $file..."
    grift-sim 1000 "$file" >> "results/$file.out"
done
