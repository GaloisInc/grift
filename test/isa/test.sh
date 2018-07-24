rm -rf *.log
for file in rv64ui-p-*
do
    echo "testing $file..."
    grift 1000000 "$file" >> "results/$file.out"
done
