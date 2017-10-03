if [ $# -ne 2 ]; then
  echo "Usage: $0 <input_file> <label>"
  exit
fi

cat $1 | awk -v label=$2 'BEGIN{
}
{
  if ($1 in sum) {
    sum[$1] += $2;
    times[$1] += 1;
  }
  else {
    sum[$1] = $2;
    times[$1] = 1;
  }
}
END{
  for (x in sum)
    printf("%d %s %.3f\n", x, label, sum[x]/times[x]);
}'
