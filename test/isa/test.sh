rm -rf *.log
for file in rv64uf-p-*
do
    echo "testing $file..."
    grift 1000000 "$file" >> "results/$file.out"
done
