rm -rf *.log
rm results/*
for file in *.elf
do
    echo "testing $file..."
    grift 1000000 "$file" > "results/$file.out"
done
