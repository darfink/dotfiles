function pass -d "Generate a random password" -a length
        if not echo $length | grep -q -E '^[0-9]+$'
                echo 'Usage: `pass <length>`'
                return 1
        end

        strings /dev/urandom | grep -o '[[:alnum:]]' | head -n $length | tr -d '\n'
        echo ""
end