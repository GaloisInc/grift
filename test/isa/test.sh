rm -rf *.log
for file in rv64ud-p-*
do
    echo "testing $file..."
    grift-sim 1000000 "$file" >> "results/$file.out"
done
