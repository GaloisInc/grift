rm -rf results/
mkdir results
for file in rv64ud-p-*
do
    echo "testing $file..."
    grift-sim --arch=RV64IMAFD --steps=1000 "$file" >> "results/$file.out"
done
