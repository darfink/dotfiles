function stats -d "Print stats from line-wise input"
        cat | sort | uniq -c | sort -r
end