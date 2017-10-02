if [ $# -ne 1 ]; then
  echo "Usage: $0 <input_file>"
  exit
fi

cat $1 | awk 'BEGIN{
}
{
  if ($2 in sum) {
    sum[$2] += $5;
    times[$2] += 1;
  }
  else {
    sum[$2] = $5;
    times[$2] = 1;
  }
}
END{
  for (x in sum)
    printf("%d %f\n", x, sum[x]/times[x]);
}'
