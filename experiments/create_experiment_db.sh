rm -rf *.db

for filename in *.txt; do
    touch ${filename%.*}.db
    sp-process import-data -D sqlite:${filename%.*}.db experiments $filename
    rm -f $filename
done

