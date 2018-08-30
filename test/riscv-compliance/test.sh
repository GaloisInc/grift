rm -rf *.log
rm results/*
for file in *.elf
do
    echo "testing $file..."
    grift-sim RV32IM 1000000 "$file" > "results/$file.out"
done
