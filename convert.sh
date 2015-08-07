CONFIG=tw2sp.json
OPENCC=opencc

fs=$(find zh-tw -name '*.md')


for f in $fs
do
    new_f=$(echo $f | sed -e "s/zh-tw/zh-cn/g")
    $OPENCC -c $CONFIG < $f > $new_f
done
