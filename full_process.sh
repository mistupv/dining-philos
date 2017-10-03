cat normal/run* > normal_all.txt
cat bugged/run* > bugged_all.txt
./comp_avg.sh normal_all.txt Correct > normal_avg.txt
./comp_avg.sh bugged_all.txt Bugged > bugged_avg.txt
cat normal_avg.txt bugged_avg.txt > full_avg.txt
