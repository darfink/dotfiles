function fish_title
  switch $_
    case fish
      echo (prompt_pwd)
    case '*'
      echo $_
  end
end
