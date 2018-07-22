for file in rv64ua-p-*
do
    echo "testing $file..."
    grift 1000000 "$file" >> "results/$file.out"
done
