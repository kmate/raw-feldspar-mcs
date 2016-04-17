for i in * ; do
  if [ -d "$i" ]; then
    echo "Testing " $i
    ./test.sh "$i" $1
  fi
done

