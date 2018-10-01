rm -rf isa-results/
mkdir isa-results

for file in riscv-tests/build/share/riscv-tests/isa/rv64uc-p-*
do
    [[ $file == *.dump ]] && continue
    f=$(basename $file)
    echo "testing $file..."
    grift-sim --arch=RV64GC --steps=1000 "$file" >> "./isa-results/$f.out"
done
