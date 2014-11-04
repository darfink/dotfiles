function gz -d "Compare original and gripped file size" -a file
	if [ (count $argv) != 1 ]
		echo 'Usage: `gz <file>`'
		return 1
	end

	set origsize (wc -c < $file)
	set gzipsize (gzip -c $file | wc -c)
	set ratio (echo "$gzipsize * 100 / $origsize" | bc -l)

	printf "orig: %d bytes\n" $origsize
	printf "gzip: %d bytes (%2.2f%%)\n" $gzipsize $ratio
end
