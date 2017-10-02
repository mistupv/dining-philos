for i in $(seq 0 99); do
  erl -noshell -eval "dining:main(\"bugged/run_$i.txt\")." -s init stop
done
