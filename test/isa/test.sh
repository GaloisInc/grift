for file in rv64ui-p-*
do
    echo "testing $file..."
    riscv-sim 1000000 "$file" >> "results/$file.out"
done
