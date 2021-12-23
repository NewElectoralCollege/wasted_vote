path= #Path to database here

mkdir -p nec

for year in 1976 1980 1984 1988 1992 1996 2000 2004 2008 2012 2016 2020
do
    mkdir -p nec/$year
done

for file in $path/*/*.json
do
    out=nec/${file:39}
    $wasted_vote --nec-format --out="$out" "$file"
done

echo "[" > nec/total.json

for year in 1976 1980 1984 1988 1992 1996 2000 2004 2008 2012 2016 2020
do
    for file in nec/$year/*.json
    do
        state=${file:-4}

        cat "$file" >> nec/total.json
        echo "," >> nec/total.json
    done
done

sed -i '$ s/.$//' nec/total.json

echo "]" >> nec/total.json