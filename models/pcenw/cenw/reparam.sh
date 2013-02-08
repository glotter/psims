while read pv; do
  echo pv = $pv
  p=$(echo $pv | sed -e 's/[ ]*=.*//')
  echo p = _${p}_
  v=$(echo $pv | sed -e 's/^.*=[ ]*//')
  echo v = _${v}_

  for f in *.cfg; do
    sed -i $f -e "/^$p/s/=.*/= $v/g"
  done
done <params
