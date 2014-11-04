function history-stat -d "Display the ten most used commands"
        history | stats | head -n 10
end