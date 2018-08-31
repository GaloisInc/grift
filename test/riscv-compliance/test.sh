rm -rf results/
mkdir results
for file in *.elf
do
    echo "testing $file..."
    grift-sim --arch=RV32IM --steps=1000000 --coverage="results/$file.log" "$file" > "results/$file.out"
done
