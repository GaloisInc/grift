for file in rv64um-p-*
do
    echo "testing $file..."
    grift 1000000 "$file" >> "results/$file.out"
done
