#!/bin/bash

OUT_YML="_site.yml"
TITLE="Early Detection of Hepatocellular Carcinoma"
#DESCRIPTION="Put subtitle here"

echo 'name: "HCC"' > $OUT_YML
echo 'output_dir: "."' >> $OUT_YML
echo 'navbar:' >> $OUT_YML
echo '  title: '$TITLE >> $OUT_YML
echo '  left:' >> $OUT_YML
echo '    - text: "Home"' >> $OUT_YML
echo '      href: index.html' >> $OUT_YML


OUT_INDEX="index.Rmd"

echo '---' > $OUT_INDEX
echo 'title: '$TITLE >> $OUT_INDEX
echo '---' >> $OUT_INDEX
echo >> $OUT_INDEX
echo $DESCRIPTION >> $OUT_INDEX
echo >> $OUT_INDEX

files="`ls *.html`"
for f in $files
do
  if [ $f != "index.html" ]
  then
    echo '<li><a href="'$f'">'${f}'</a></li>' >> $OUT_INDEX
  fi
done

###     echo '<li><a href="'$f'">'${f#_}'</a></li>' >> $OUT_INDEX

